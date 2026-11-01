type stream_id = { ms : int64; seq : int64 }
type stream_entry = { id : stream_id; fields : (string * string) list }

type stream = {
  (* Newest first. XADD is the hot path; reads reverse once. *)
  mutable entries_rev : stream_entry list;
  mutable last_id : stream_id option;
}

type sorted_set = (string, float) Hashtbl.t
type geo = (string, float * float) Hashtbl.t

type value =
  | String of string
  | List of string list
  | Stream of stream
  | Zset of sorted_set
  | Geo of geo

type entry = { value : value; expires_at_ms : int64 option }

type t = {
  entries : (string, entry) Hashtbl.t;
  versions : (string, int64) Hashtbl.t;
  mutable next_version : int64;
}

type wrong_type = [ `Wrong_type ]
type incr_error = [ wrong_type | `Not_an_integer | `Overflow ]
type 'a lookup = [ `Missing | `Value of 'a | wrong_type ]
type key_type = [ `None | `String | `List | `Stream | `Zset ]
type geo_lookup = (float * float) option

type geosearch_error =
  [ wrong_type | `Invalid_center of float * float | `Invalid_radius of float ]

type stream_write_error =
  [ wrong_type | `Invalid_stream_id of string | `Id_not_greater | `Id_too_small ]

type stream_read_error = [ wrong_type | `Invalid_stream_id of string ]
type stream_id_spec = Auto_all | Auto_seq of int64 | Explicit of stream_id
type range_bound = Min | Max | Inclusive of stream_id

let create () =
  {
    entries = Hashtbl.create 32;
    versions = Hashtbl.create 32;
    next_version = 0L;
  }

let bump_version store key =
  let next = Int64.add store.next_version 1L in
  store.next_version <- next;
  Hashtbl.replace store.versions key next

let compare_stream_id left right =
  let ms_cmp = Int64.compare left.ms right.ms in
  if ms_cmp <> 0 then ms_cmp else Int64.compare left.seq right.seq

let min_stream_id = { ms = 0L; seq = 1L }
let stream_id_to_string { ms; seq } = Printf.sprintf "%Ld-%Ld" ms seq

let parse_non_negative_int64 value =
  match Integer.of_string_opt value with
  | Some parsed when parsed >= 0L -> Some parsed
  | _ -> None

let parse_complete_or_incomplete_id raw =
  match String.split_on_char '-' raw with
  | [ ms ] -> (
      match parse_non_negative_int64 ms with
      | Some ms -> Ok { ms; seq = 0L }
      | None -> Error (`Invalid_stream_id raw))
  | [ ms; seq ] -> (
      match (parse_non_negative_int64 ms, parse_non_negative_int64 seq) with
      | Some ms, Some seq -> Ok { ms; seq }
      | _ -> Error (`Invalid_stream_id raw))
  | _ -> Error (`Invalid_stream_id raw)

let parse_xadd_id_spec raw =
  if raw = "*" then Ok Auto_all
  else
    match String.split_on_char '-' raw with
    | [ ms; "*" ] -> (
        match parse_non_negative_int64 ms with
        | Some ms -> Ok (Auto_seq ms)
        | None -> Error (`Invalid_stream_id raw))
    | _ ->
        Result.map (fun id -> Explicit id) (parse_complete_or_incomplete_id raw)

let parse_range_bound ~is_start raw =
  match raw with
  | "-" when is_start -> Ok Min
  | "+" when not is_start -> Ok Max
  | _ ->
      Result.map (fun id -> Inclusive id) (parse_complete_or_incomplete_id raw)

let is_expired ~now_ms = function
  | None -> false
  | Some expires_at_ms -> Expiry.is_expired ~now_ms ~expires_at_ms

let expire_if_needed ~now_ms store key =
  match Hashtbl.find_opt store.entries key with
  | Some { expires_at_ms; _ } when is_expired ~now_ms expires_at_ms ->
      Hashtbl.remove store.entries key;
      bump_version store key
  | _ -> ()

let get_entry ~now_ms store key =
  expire_if_needed ~now_ms store key;
  Hashtbl.find_opt store.entries key

let key_type ~now_ms store key =
  match get_entry ~now_ms store key with
  | None -> `None
  | Some { value = String _; _ } -> `String
  | Some { value = List _; _ } -> `List
  | Some { value = Stream _; _ } -> `Stream
  | Some { value = Zset _ | Geo _; _ } -> `Zset

let watch_version ~now_ms store key =
  expire_if_needed ~now_ms store key;
  Hashtbl.find_opt store.versions key |> Option.value ~default:0L

let list_length values = Int64.of_int (List.length values)

let normalize_index length index =
  if index < 0L then Int64.add length index else index

let normalize_range_bounds length start_index stop_index =
  let last = Int64.sub length 1L in
  let start_index = normalize_index length start_index in
  let stop_index = normalize_index length stop_index in
  if start_index > last || stop_index < 0L then None
  else
    let start_index = if start_index < 0L then 0L else start_index in
    let stop_index = if stop_index > last then last else stop_index in
    if start_index > stop_index then None else Some (start_index, stop_index)

let get_string ~now_ms store key =
  match get_entry ~now_ms store key with
  | None -> `Missing
  | Some { value = String value; _ } -> `Value value
  | Some { value = List _ | Stream _ | Zset _ | Geo _; _ } -> `Wrong_type

let set_string store ~key ~value ~expires_at_ms =
  Hashtbl.replace store.entries key { value = String value; expires_at_ms };
  bump_version store key

let incr_string ~now_ms store ~key =
  match get_entry ~now_ms store key with
  | None ->
      Hashtbl.replace store.entries key
        { value = String "1"; expires_at_ms = None };
      bump_version store key;
      Ok 1L
  | Some { value = List _ | Stream _ | Zset _ | Geo _; _ } -> Error `Wrong_type
  | Some { value = String value; expires_at_ms } -> (
      match Integer.of_string_opt value with
      | None -> Error `Not_an_integer
      | Some current when Int64.equal current Int64.max_int -> Error `Overflow
      | Some current ->
          let next = Int64.add current 1L in
          Hashtbl.replace store.entries key
            { value = String (Int64.to_string next); expires_at_ms };
          bump_version store key;
          Ok next)

let with_list ~now_ms store key on_missing on_list =
  match get_entry ~now_ms store key with
  | None -> on_missing ()
  | Some { value = String _ | Stream _ | Zset _ | Geo _; _ } ->
      Error `Wrong_type
  | Some { value = List values; expires_at_ms } -> on_list values expires_at_ms

let rpush ~now_ms store ~key values =
  with_list ~now_ms store key
    (fun () ->
      Hashtbl.replace store.entries key
        { value = List values; expires_at_ms = None };
      bump_version store key;
      Ok (list_length values))
    (fun existing expires_at_ms ->
      let updated = existing @ values in
      Hashtbl.replace store.entries key { value = List updated; expires_at_ms };
      bump_version store key;
      Ok (list_length updated))

let lpush ~now_ms store ~key values =
  with_list ~now_ms store key
    (fun () ->
      let updated = List.rev values in
      Hashtbl.replace store.entries key
        { value = List updated; expires_at_ms = None };
      bump_version store key;
      Ok (list_length updated))
    (fun existing expires_at_ms ->
      let updated = List.rev_append values existing in
      Hashtbl.replace store.entries key { value = List updated; expires_at_ms };
      bump_version store key;
      Ok (list_length updated))

let lrange ~now_ms store ~key start_index stop_index =
  with_list ~now_ms store key
    (fun () -> Ok [])
    (fun values _ ->
      let length = list_length values in
      if length = 0L then Ok []
      else
        match normalize_range_bounds length start_index stop_index with
        | None -> Ok []
        | Some (start_index, stop_index) ->
            let rec collect index values acc =
              match values with
              | [] -> List.rev acc
              | value :: rest ->
                  if index > stop_index then List.rev acc
                  else if index >= start_index then
                    collect (Int64.add index 1L) rest (value :: acc)
                  else collect (Int64.add index 1L) rest acc
            in
            Ok (collect 0L values []))

let llen ~now_ms store ~key =
  with_list ~now_ms store key
    (fun () -> Ok 0L)
    (fun values _ -> Ok (list_length values))

let lrem ~now_ms store ~key count element =
  let remove_from_left limit values =
    let rec loop removed acc = function
      | [] -> (removed, List.rev acc)
      | value :: rest when value = element && (limit = 0L || removed < limit) ->
          loop (Int64.add removed 1L) acc rest
      | value :: rest -> loop removed (value :: acc) rest
    in
    loop 0L [] values
  in
  with_list ~now_ms store key
    (fun () -> Ok 0L)
    (fun values expires_at_ms ->
      let removed, updated =
        if count >= 0L then remove_from_left count values
        else
          let removed, reversed =
            remove_from_left (Int64.neg count) (List.rev values)
          in
          (removed, List.rev reversed)
      in
      if removed > 0L then (
        if updated = [] then Hashtbl.remove store.entries key
        else
          Hashtbl.replace store.entries key
            { value = List updated; expires_at_ms };
        bump_version store key);
      Ok removed)

let lpop ~now_ms store ~key =
  with_list ~now_ms store key
    (fun () -> Ok None)
    (fun values expires_at_ms ->
      match values with
      | [] -> Ok None
      | value :: rest ->
          if rest = [] then Hashtbl.remove store.entries key
          else
            Hashtbl.replace store.entries key
              { value = List rest; expires_at_ms };
          bump_version store key;
          Ok (Some value))

let with_stream ~now_ms store key on_missing on_stream =
  match get_entry ~now_ms store key with
  | None -> on_missing ()
  | Some { value = String _ | List _ | Zset _ | Geo _; _ } -> Error `Wrong_type
  | Some { value = Stream stream; expires_at_ms } ->
      on_stream stream expires_at_ms

let compare_zset_entry (member_a, score_a) (member_b, score_b) =
  let score_cmp = Float.compare score_a score_b in
  if score_cmp <> 0 then score_cmp else String.compare member_a member_b

let zset_entries zset =
  Hashtbl.to_seq zset |> List.of_seq |> List.sort compare_zset_entry

let with_zset ~now_ms store key on_missing on_zset =
  match get_entry ~now_ms store key with
  | None -> on_missing ()
  | Some { value = String _ | List _ | Stream _ | Geo _; _ } ->
      Error `Wrong_type
  | Some { value = Zset zset; expires_at_ms } -> on_zset zset expires_at_ms

let zset_length zset = Int64.of_int (Hashtbl.length zset)

let zadd ~now_ms store ~key score_members =
  let apply zset expires_at_ms =
    let added = ref 0L in
    let changed = ref false in
    List.iter
      (fun (score, member) ->
        match Hashtbl.find_opt zset member with
        | None ->
            added := Int64.add !added 1L;
            changed := true;
            Hashtbl.replace zset member score
        | Some old_score ->
            if Float.compare old_score score <> 0 then (
              changed := true;
              Hashtbl.replace zset member score))
      score_members;
    if !changed then (
      Hashtbl.replace store.entries key { value = Zset zset; expires_at_ms };
      bump_version store key);
    Ok !added
  in
  with_zset ~now_ms store key
    (fun () ->
      let zset = Hashtbl.create 16 in
      apply zset None)
    apply

let zrank ~now_ms store ~key ~member =
  with_zset ~now_ms store key
    (fun () -> Ok None)
    (fun zset _ ->
      let entries = zset_entries zset in
      let rec loop index = function
        | [] -> Ok None
        | (current_member, _) :: rest ->
            if current_member = member then Ok (Some index)
            else loop (Int64.add index 1L) rest
      in
      loop 0L entries)

let zrange ~now_ms store ~key start_index stop_index =
  with_zset ~now_ms store key
    (fun () -> Ok [])
    (fun zset _ ->
      let entries = zset_entries zset in
      let length = Int64.of_int (List.length entries) in
      if length = 0L then Ok []
      else
        match normalize_range_bounds length start_index stop_index with
        | None -> Ok []
        | Some (start_index, stop_index) ->
            let rec collect index entries acc =
              match entries with
              | [] -> List.rev acc
              | (member, _) :: rest ->
                  if index > stop_index then List.rev acc
                  else if index >= start_index then
                    collect (Int64.add index 1L) rest (member :: acc)
                  else collect (Int64.add index 1L) rest acc
            in
            Ok (collect 0L entries []))

let zcard ~now_ms store ~key =
  with_zset ~now_ms store key
    (fun () -> Ok 0L)
    (fun zset _ -> Ok (zset_length zset))

let zscore ~now_ms store ~key ~member =
  with_zset ~now_ms store key
    (fun () -> Ok None)
    (fun zset _ -> Ok (Hashtbl.find_opt zset member))

let zrem ~now_ms store ~key members =
  with_zset ~now_ms store key
    (fun () -> Ok 0L)
    (fun zset expires_at_ms ->
      let removed = ref 0L in
      List.iter
        (fun member ->
          if Hashtbl.mem zset member then (
            Hashtbl.remove zset member;
            removed := Int64.add !removed 1L))
        members;
      if !removed > 0L then (
        if Hashtbl.length zset = 0 then Hashtbl.remove store.entries key
        else
          Hashtbl.replace store.entries key { value = Zset zset; expires_at_ms };
        bump_version store key);
      Ok !removed)

let valid_longitude longitude = longitude >= -180. && longitude <= 180.

let valid_latitude latitude =
  latitude >= -85.05112878 && latitude <= 85.05112878

let with_geo ~now_ms store key on_missing on_geo =
  match get_entry ~now_ms store key with
  | None -> on_missing ()
  | Some { value = String _ | List _ | Stream _ | Zset _; _ } ->
      Error `Wrong_type
  | Some { value = Geo geo; expires_at_ms } -> on_geo geo expires_at_ms

let geoadd ~now_ms store ~key locations =
  let apply geo expires_at_ms =
    let added = ref 0L in
    let changed = ref false in
    let invalid_member =
      List.find_map
        (fun (longitude, latitude, member) ->
          if valid_longitude longitude && valid_latitude latitude then None
          else Some member)
        locations
    in
    match invalid_member with
    | Some member -> Error (`Invalid_coordinates member)
    | None ->
        List.iter
          (fun (longitude, latitude, member) ->
            match Hashtbl.find_opt geo member with
            | None ->
                added := Int64.add !added 1L;
                changed := true;
                Hashtbl.replace geo member (longitude, latitude)
            | Some (old_longitude, old_latitude) ->
                if old_longitude <> longitude || old_latitude <> latitude then (
                  changed := true;
                  Hashtbl.replace geo member (longitude, latitude)))
          locations;
        if !changed then (
          Hashtbl.replace store.entries key { value = Geo geo; expires_at_ms };
          bump_version store key);
        Ok !added
  in
  with_geo ~now_ms store key
    (fun () ->
      let geo = Hashtbl.create 16 in
      apply geo None)
    apply

let geopos ~now_ms store ~key members =
  with_geo ~now_ms store key
    (fun () -> Ok (List.map (fun _ -> None) members))
    (fun geo _ ->
      Ok (List.map (fun member -> Hashtbl.find_opt geo member) members))

let earth_radius_m = 6371000.
let radians degrees = degrees *. Float.pi /. 180.

let haversine_distance_meters (lon1, lat1) (lon2, lat2) =
  let dlat = radians (lat2 -. lat1) in
  let dlon = radians (lon2 -. lon1) in
  let lat1 = radians lat1 in
  let lat2 = radians lat2 in
  let a =
    (sin (dlat /. 2.) ** 2.)
    +. (cos lat1 *. cos lat2 *. (sin (dlon /. 2.) ** 2.))
  in
  let c = 2. *. atan2 (sqrt a) (sqrt (1. -. a)) in
  earth_radius_m *. c

let geodist ~now_ms store ~key ~member1 ~member2 =
  with_geo ~now_ms store key
    (fun () -> Ok None)
    (fun geo _ ->
      match (Hashtbl.find_opt geo member1, Hashtbl.find_opt geo member2) with
      | Some pos1, Some pos2 -> Ok (Some (haversine_distance_meters pos1 pos2))
      | _ -> Ok None)

let geosearch ~now_ms store ~key ~longitude ~latitude ~radius_meters =
  if not (valid_longitude longitude && valid_latitude latitude) then
    Error (`Invalid_center (longitude, latitude))
  else if radius_meters < 0. then Error (`Invalid_radius radius_meters)
  else
    with_geo ~now_ms store key
      (fun () -> Ok [])
      (fun geo _ ->
        let center = (longitude, latitude) in
        Ok
          (Hashtbl.to_seq geo |> List.of_seq
          |> List.filter_map (fun (member, position) ->
              if haversine_distance_meters center position <= radius_meters then
                Some member
              else None)
          |> List.sort String.compare))

let next_auto_id stream ~now_ms =
  match stream.last_id with
  | None ->
      if now_ms = 0L then { ms = 0L; seq = 1L } else { ms = now_ms; seq = 0L }
  | Some last ->
      let ms = if Int64.compare now_ms last.ms < 0 then last.ms else now_ms in
      if Int64.compare ms last.ms = 0 then { ms; seq = Int64.add last.seq 1L }
      else { ms; seq = 0L }

let next_partial_id stream ms =
  match stream.last_id with
  | None -> if ms = 0L then { ms; seq = 1L } else { ms; seq = 0L }
  | Some last when Int64.compare ms last.ms < 0 -> raise Exit
  | Some last when Int64.compare ms last.ms = 0 ->
      { ms; seq = Int64.add last.seq 1L }
  | Some _ -> if ms = 0L then { ms; seq = 1L } else { ms; seq = 0L }

let validate_explicit_id stream id =
  if compare_stream_id id min_stream_id < 0 then Error `Id_too_small
  else
    match stream.last_id with
    | Some last when compare_stream_id id last <= 0 -> Error `Id_not_greater
    | _ -> Ok id

let append_stream_entry store key (stream : stream) expires_at_ms entry =
  stream.entries_rev <- entry :: stream.entries_rev;
  stream.last_id <- Some entry.id;
  Hashtbl.replace store.entries key { value = Stream stream; expires_at_ms };
  bump_version store key

let xadd ~now_ms store ~key ~id_spec fields =
  let create_stream () = { entries_rev = []; last_id = None } in
  let perform stream expires_at_ms =
    match parse_xadd_id_spec id_spec with
    | Error (`Invalid_stream_id raw) -> Error (`Invalid_stream_id raw)
    | Ok spec ->
        let id_result =
          match spec with
          | Auto_all -> Ok (next_auto_id stream ~now_ms)
          | Auto_seq ms -> (
              try Ok (next_partial_id stream ms)
              with Exit -> Error `Id_not_greater)
          | Explicit id -> validate_explicit_id stream id
        in
        Result.map
          (fun id ->
            let entry = { id; fields } in
            append_stream_entry store key stream expires_at_ms entry;
            id)
          id_result
  in
  with_stream ~now_ms store key
    (fun () -> perform (create_stream ()) None)
    perform

let resolve_xread_id ~now_ms store ~key raw =
  match raw with
  | "$" -> (
      match
        with_stream ~now_ms store key
          (fun () -> Ok None)
          (fun stream _ -> Ok stream.last_id)
      with
      | Ok (Some id) -> Ok (stream_id_to_string id)
      | Ok None -> Ok "0-0"
      | Error `Wrong_type -> Error `Wrong_type)
  | _ -> Result.map stream_id_to_string (parse_complete_or_incomplete_id raw)

let xrange ~now_ms store ~key ~start_spec ~end_spec =
  let within_range start_bound end_bound id =
    let lower_ok =
      match start_bound with
      | Min -> true
      | Inclusive start_id -> compare_stream_id id start_id >= 0
      | Max -> false
    in
    let upper_ok =
      match end_bound with
      | Max -> true
      | Inclusive end_id -> compare_stream_id id end_id <= 0
      | Min -> false
    in
    lower_ok && upper_ok
  in
  let start_bound = parse_range_bound ~is_start:true start_spec in
  let end_bound = parse_range_bound ~is_start:false end_spec in
  match (start_bound, end_bound) with
  | Error (`Invalid_stream_id raw), _ | _, Error (`Invalid_stream_id raw) ->
      Error (`Invalid_stream_id raw)
  | Ok start_bound, Ok end_bound ->
      with_stream ~now_ms store key
        (fun () -> Ok [])
        (fun stream _ ->
          Ok
            (List.filter
               (fun entry -> within_range start_bound end_bound entry.id)
               (List.rev stream.entries_rev)))

let xread ~now_ms store ~queries =
  let parse_query (key, raw_id) =
    Result.map (fun id -> (key, id)) (parse_complete_or_incomplete_id raw_id)
  in
  let rec gather parsed_queries acc =
    match parsed_queries with
    | [] -> Ok (List.rev acc)
    | (key, id) :: rest -> (
        match
          with_stream ~now_ms store key
            (fun () -> Ok [])
            (fun stream _ -> Ok (List.rev stream.entries_rev))
        with
        | Error `Wrong_type -> Error `Wrong_type
        | Ok entries ->
            let matching =
              List.filter
                (fun entry -> compare_stream_id entry.id id > 0)
                entries
            in
            let acc = if matching = [] then acc else (key, matching) :: acc in
            gather rest acc)
  in
  let rec parse_all acc = function
    | [] -> Ok (List.rev acc)
    | query :: rest -> (
        match parse_query query with
        | Ok parsed -> parse_all (parsed :: acc) rest
        | Error (`Invalid_stream_id raw) -> Error (`Invalid_stream_id raw))
  in
  Result.bind (parse_all [] queries) (fun parsed -> gather parsed [])
