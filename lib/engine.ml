type blocked =
  | List_pop of { key : string; timeout_ms : int64 option }
  | Stream_read of {
      queries : (string * string) list;
      timeout_ms : int64 option;
    }

type outcome = {
  reply : Resp.reply option;
  block : blocked option;
  wake_keys : string list;
}

type t = { store : Store.t }

let create () = { store = Store.create () }

let load_string state ~key ~value ~expires_at_ms =
  Store.set_string state.store ~key ~value ~expires_at_ms

let watch_version ~now_ms state key =
  Store.watch_version ~now_ms state.store key

let wrong_type_message =
  "Operation against a key holding the wrong kind of value"

let not_integer_message = "value is not an integer or out of range"
let overflow_message = "increment or decrement would overflow"

let stream_id_not_greater_message =
  "The ID specified in XADD is equal or smaller than the target stream top item"

let stream_id_too_small_message =
  "The ID specified in XADD must be greater than 0-0"

let invalid_coordinates_message member =
  Printf.sprintf "invalid coordinates for member: %s" member

let immediate ?(wake_keys = []) reply =
  { reply = Some reply; block = None; wake_keys }

let block_list key timeout_ms =
  { reply = None; block = Some (List_pop { key; timeout_ms }); wake_keys = [] }

let block_stream queries timeout_ms =
  {
    reply = None;
    block = Some (Stream_read { queries; timeout_ms });
    wake_keys = [];
  }

let type_name = function
  | `None -> "none"
  | `String -> "string"
  | `List -> "list"
  | `Stream -> "stream"
  | `Zset -> "zset"

let score_to_string score =
  let fractional, _ = modf score in
  if fractional = 0. then Printf.sprintf "%.0f" score
  else Printf.sprintf "%.17g" score

let encode_geopos = function
  | None -> Resp.Null_array
  | Some (longitude, latitude) ->
      Resp.Array
        [
          Resp.Bulk_string (Some (score_to_string longitude));
          Resp.Bulk_string (Some (score_to_string latitude));
        ]

let unit_multiplier unit =
  match String.lowercase_ascii unit with
  | "m" -> Some 1.
  | "km" -> Some 0.001
  | _ -> None

let encode_stream_entry entry =
  Resp.Array
    [
      Resp.Bulk_string (Some (Store.stream_id_to_string entry.Store.id));
      Resp.Array
        (List.concat_map
           (fun (field, value) ->
             [ Resp.Bulk_string (Some field); Resp.Bulk_string (Some value) ])
           entry.Store.fields);
    ]

let encode_xread_reply streams =
  if streams = [] then Resp.Null_array
  else
    Resp.Array
      (List.map
         (fun (key, entries) ->
           Resp.Array
             [
               Resp.Bulk_string (Some key);
               Resp.Array (List.map encode_stream_entry entries);
             ])
         streams)

let handle_xread ~now_ms state ~block_ms ~queries =
  let resolved_queries_result =
    queries
    |> List.fold_left
         (fun acc (key, raw_id) ->
           Result.bind acc (fun acc ->
               Result.map
                 (fun resolved_id -> (key, resolved_id) :: acc)
                 (Store.resolve_xread_id ~now_ms state.store ~key raw_id)))
         (Ok [])
  in
  match resolved_queries_result with
  | Error `Wrong_type ->
      immediate
        (Resp.Prefixed_error
           { prefix = "WRONGTYPE"; message = wrong_type_message })
  | Error (`Invalid_stream_id raw) ->
      immediate
        (Resp.Error
           (Printf.sprintf
              "Invalid stream ID specified as stream command argument: %s" raw))
  | Ok resolved_queries -> (
      let resolved_queries = List.rev resolved_queries in
      match Store.xread ~now_ms state.store ~queries:resolved_queries with
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message })
      | Error (`Invalid_stream_id raw) ->
          immediate
            (Resp.Error
               (Printf.sprintf
                  "Invalid stream ID specified as stream command argument: %s"
                  raw))
      | Ok streams when streams = [] -> (
          match block_ms with
          | Some timeout_ms when timeout_ms < 0L ->
              block_stream resolved_queries None
          | Some timeout_ms -> block_stream resolved_queries (Some timeout_ms)
          | None -> immediate Resp.Null_array)
      | Ok streams -> immediate (encode_xread_reply streams))

let handle ~now_ms state = function
  | Command.Ping None -> immediate (Resp.Simple_string "PONG")
  | Ping (Some message) -> immediate (Resp.Bulk_string (Some message))
  | Echo value -> immediate (Resp.Bulk_string (Some value))
  | Type key ->
      immediate
        (Resp.Simple_string (type_name (Store.key_type ~now_ms state.store key)))
  | Get key -> (
      match Store.get_string ~now_ms state.store key with
      | `Missing -> immediate (Resp.Bulk_string None)
      | `Value value -> immediate (Resp.Bulk_string (Some value))
      | `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Set { key; value; expiry } ->
      let expires_at_ms =
        match expiry with
        | None -> None
        | Some (Command.Relative_ms relative_ms) ->
            Some (Expiry.add_saturating now_ms relative_ms)
        | Some (Command.Absolute_ms expires_at_ms) -> Some expires_at_ms
      in
      Store.set_string state.store ~key ~value ~expires_at_ms;
      immediate (Resp.Simple_string "OK")
  | Incr key -> (
      match Store.incr_string ~now_ms state.store ~key with
      | Ok value -> immediate ~wake_keys:[ key ] (Resp.Integer value)
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message })
      | Error `Not_an_integer -> immediate (Resp.Error not_integer_message)
      | Error `Overflow -> immediate (Resp.Error overflow_message))
  | Rpush { key; values } -> (
      match Store.rpush ~now_ms state.store ~key values with
      | Ok length -> immediate ~wake_keys:[ key ] (Resp.Integer length)
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Lpush { key; values } -> (
      match Store.lpush ~now_ms state.store ~key values with
      | Ok length -> immediate ~wake_keys:[ key ] (Resp.Integer length)
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Lrange { key; start_index; stop_index } -> (
      match Store.lrange ~now_ms state.store ~key start_index stop_index with
      | Ok values ->
          immediate
            (Resp.Array
               (List.map (fun value -> Resp.Bulk_string (Some value)) values))
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Llen key -> (
      match Store.llen ~now_ms state.store ~key with
      | Ok length -> immediate (Resp.Integer length)
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Lrem { key; count; element } -> (
      match Store.lrem ~now_ms state.store ~key count element with
      | Ok removed -> immediate (Resp.Integer removed)
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Lpop key -> (
      match Store.lpop ~now_ms state.store ~key with
      | Ok (Some value) -> immediate (Resp.Bulk_string (Some value))
      | Ok None -> immediate (Resp.Bulk_string None)
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Zadd { key; score_members } -> (
      match Store.zadd ~now_ms state.store ~key score_members with
      | Ok added -> immediate (Resp.Integer added)
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Zrank { key; member } -> (
      match Store.zrank ~now_ms state.store ~key ~member with
      | Ok None -> immediate (Resp.Bulk_string None)
      | Ok (Some rank) -> immediate (Resp.Integer rank)
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Zrange { key; start_index; stop_index } -> (
      match Store.zrange ~now_ms state.store ~key start_index stop_index with
      | Ok members ->
          immediate
            (Resp.Array
               (List.map (fun member -> Resp.Bulk_string (Some member)) members))
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Zcard key -> (
      match Store.zcard ~now_ms state.store ~key with
      | Ok cardinality -> immediate (Resp.Integer cardinality)
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Zscore { key; member } -> (
      match Store.zscore ~now_ms state.store ~key ~member with
      | Ok None -> immediate (Resp.Bulk_string None)
      | Ok (Some score) ->
          immediate (Resp.Bulk_string (Some (score_to_string score)))
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Zrem { key; members } -> (
      match Store.zrem ~now_ms state.store ~key members with
      | Ok removed -> immediate (Resp.Integer removed)
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Geoadd { key; locations } -> (
      match Store.geoadd ~now_ms state.store ~key locations with
      | Ok added -> immediate (Resp.Integer added)
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message })
      | Error (`Invalid_coordinates member) ->
          immediate (Resp.Error (invalid_coordinates_message member)))
  | Geopos { key; members } -> (
      match Store.geopos ~now_ms state.store ~key members with
      | Ok positions ->
          immediate (Resp.Array (List.map encode_geopos positions))
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Geodist { key; member1; member2; unit } -> (
      let unit = Option.value ~default:"m" unit in
      match unit_multiplier unit with
      | None ->
          immediate
            (Resp.Error (Printf.sprintf "unsupported geodist unit: %s" unit))
      | Some multiplier -> (
          match Store.geodist ~now_ms state.store ~key ~member1 ~member2 with
          | Ok None -> immediate (Resp.Bulk_string None)
          | Ok (Some distance_meters) ->
              immediate
                (Resp.Bulk_string
                   (Some (score_to_string (distance_meters *. multiplier))))
          | Error `Wrong_type ->
              immediate
                (Resp.Prefixed_error
                   { prefix = "WRONGTYPE"; message = wrong_type_message })))
  | Geosearch { key; longitude; latitude; radius; unit } -> (
      match unit_multiplier unit with
      | None ->
          immediate
            (Resp.Error (Printf.sprintf "unsupported geosearch unit: %s" unit))
      | Some multiplier -> (
          match
            Store.geosearch ~now_ms state.store ~key ~longitude ~latitude
              ~radius_meters:(radius /. multiplier)
          with
          | Ok members ->
              immediate
                (Resp.Array
                   (List.map
                      (fun member -> Resp.Bulk_string (Some member))
                      members))
          | Error `Wrong_type ->
              immediate
                (Resp.Prefixed_error
                   { prefix = "WRONGTYPE"; message = wrong_type_message })
          | Error (`Invalid_center (longitude, latitude)) ->
              immediate
                (Resp.Error
                   (Printf.sprintf "invalid longitude,latitude pair %f,%f"
                      longitude latitude))
          | Error (`Invalid_radius _) ->
              immediate (Resp.Error "radius cannot be negative")))
  | Blpop { key; timeout_ms } -> (
      match Store.lpop ~now_ms state.store ~key with
      | Ok (Some value) ->
          immediate
            (Resp.Array
               [ Resp.Bulk_string (Some key); Resp.Bulk_string (Some value) ])
      | Ok None -> block_list key timeout_ms
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message }))
  | Xadd { key; id_spec; fields } -> (
      match Store.xadd ~now_ms state.store ~key ~id_spec fields with
      | Ok id ->
          immediate ~wake_keys:[ key ]
            (Resp.Bulk_string (Some (Store.stream_id_to_string id)))
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message })
      | Error (`Invalid_stream_id raw) ->
          immediate
            (Resp.Error
               (Printf.sprintf
                  "Invalid stream ID specified as stream command argument: %s"
                  raw))
      | Error `Id_not_greater ->
          immediate (Resp.Error stream_id_not_greater_message)
      | Error `Id_too_small ->
          immediate (Resp.Error stream_id_too_small_message))
  | Xrange { key; start_spec; end_spec } -> (
      match Store.xrange ~now_ms state.store ~key ~start_spec ~end_spec with
      | Ok entries ->
          immediate (Resp.Array (List.map encode_stream_entry entries))
      | Error `Wrong_type ->
          immediate
            (Resp.Prefixed_error
               { prefix = "WRONGTYPE"; message = wrong_type_message })
      | Error (`Invalid_stream_id raw) ->
          immediate
            (Resp.Error
               (Printf.sprintf
                  "Invalid stream ID specified as stream command argument: %s"
                  raw)))
  | Xread { block_ms; queries } -> handle_xread ~now_ms state ~block_ms ~queries
  | Info _ | Replconf _ | Psync _ | Wait _ | Auth _ | AclWhoami | AclGetuser _
  | Subscribe _ | Unsubscribe _ | Publish _ | Multi | Exec | Discard | Watch _
  | Unwatch ->
      immediate (Resp.Error "server error: transaction command reached engine")

let wake_blpop ~now_ms state key =
  match Store.lpop ~now_ms state.store ~key with
  | Ok (Some value) -> Some (key, value)
  | Ok None -> None
  | Error `Wrong_type -> None

let wake_xread ~now_ms state ~queries =
  match Store.xread ~now_ms state.store ~queries with
  | Ok streams when streams = [] -> None
  | Ok streams -> Some (encode_xread_reply streams)
  | Error _ -> None
