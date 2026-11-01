type set_expiry = Relative_ms of int64 | Absolute_ms of int64

type t =
  | Ping of string option
  | Echo of string
  | Info of string option
  | Type of string
  | Get of string
  | Set of { key : string; value : string; expiry : set_expiry option }
  | Rpush of { key : string; values : string list }
  | Lpush of { key : string; values : string list }
  | Lrange of { key : string; start_index : int64; stop_index : int64 }
  | Llen of string
  | Lrem of { key : string; count : int64; element : string }
  | Lpop of string
  | Blpop of { key : string; timeout_ms : int64 option }
  | Incr of string
  | Multi
  | Exec
  | Discard
  | Watch of string list
  | Unwatch
  | Replconf of string list
  | Psync of { replid : string; offset : string }
  | Wait of { numreplicas : int64; timeout_ms : int64 option }
  | Auth of string
  | AclWhoami
  | AclGetuser of string
  | Subscribe of string list
  | Unsubscribe of string list
  | Publish of { channel : string; message : string }
  | Zadd of { key : string; score_members : (float * string) list }
  | Zrank of { key : string; member : string }
  | Zrange of { key : string; start_index : int64; stop_index : int64 }
  | Zcard of string
  | Zscore of { key : string; member : string }
  | Zrem of { key : string; members : string list }
  | Geoadd of { key : string; locations : (float * float * string) list }
  | Geopos of { key : string; members : string list }
  | Geodist of {
      key : string;
      member1 : string;
      member2 : string;
      unit : string option;
    }
  | Geosearch of {
      key : string;
      longitude : float;
      latitude : float;
      radius : float;
      unit : string;
    }
  | Xadd of { key : string; id_spec : string; fields : (string * string) list }
  | Xrange of { key : string; start_spec : string; end_spec : string }
  | Xread of { block_ms : int64 option; queries : (string * string) list }

type error =
  | Unknown_command of string
  | Wrong_arity of string
  | Syntax_error of string
  | Invalid_integer of string
  | Invalid_expiry of string
  | Invalid_timeout of string
  | Invalid_float of string

let wrong_arity command = Wrong_arity command

let parse_int64 value =
  match Integer.of_string_opt value with
  | Some parsed -> Ok parsed
  | None -> Error (Invalid_integer value)

let valid_float_syntax value =
  let length = String.length value in
  let index = ref 0 in
  if !index < length && (value.[!index] = '+' || value.[!index] = '-') then
    incr index;
  let consume_digits () =
    let start = !index in
    while !index < length && value.[!index] >= '0' && value.[!index] <= '9' do
      incr index
    done;
    !index - start
  in
  let before = consume_digits () in
  let after =
    if !index < length && value.[!index] = '.' then (
      incr index;
      consume_digits ())
    else 0
  in
  if before = 0 && after = 0 then false
  else (
    if !index < length && (value.[!index] = 'e' || value.[!index] = 'E') then (
      incr index;
      if !index < length && (value.[!index] = '+' || value.[!index] = '-') then
        incr index;
      if consume_digits () = 0 then index := length + 1);
    !index = length)

let parse_float value =
  if not (valid_float_syntax value) then Error (Invalid_float value)
  else
    match float_of_string_opt value with
    | Some parsed
      when classify_float parsed <> FP_nan
           && classify_float parsed <> FP_infinite ->
        Ok parsed
    | _ -> Error (Invalid_float value)

let parse_bounded_expiry multiplier constructor raw =
  match Integer.of_string_opt raw with
  | None -> Error (Invalid_integer raw)
  | Some value when value <= 0L -> Error (Invalid_expiry raw)
  | Some value when Int64.compare value (Int64.div Int64.max_int multiplier) > 0
    ->
      Error (Invalid_expiry raw)
  | Some value -> Ok (constructor (Int64.mul value multiplier))

let parse_set arguments =
  match arguments with
  | [ key; value ] -> Ok (Set { key; value; expiry = None })
  | [ key; value; option_name; option_value ] ->
      let parse_relative multiplier raw =
        parse_bounded_expiry multiplier (fun expiry -> Relative_ms expiry) raw
      in
      let parse_absolute multiplier raw =
        parse_bounded_expiry multiplier (fun expiry -> Absolute_ms expiry) raw
      in
      let result =
        match String.uppercase_ascii option_name with
        | "EX" -> parse_relative 1000L option_value
        | "PX" -> parse_relative 1L option_value
        | "EXAT" -> parse_absolute 1000L option_value
        | "PXAT" -> parse_absolute 1L option_value
        | _ -> Error (Syntax_error "unsupported SET option")
      in
      Result.map (fun expiry -> Set { key; value; expiry = Some expiry }) result
  | _ -> Error (wrong_arity "set")

let parse_nonempty_values constructor command_name = function
  | key :: values when values <> [] -> Ok (constructor key values)
  | _ -> Error (wrong_arity command_name)

let parse_indexed_range constructor command_name = function
  | [ key; start_index; stop_index ] ->
      Result.bind (parse_int64 start_index) @@ fun start_index ->
      Result.map
        (fun stop_index -> constructor key start_index stop_index)
        (parse_int64 stop_index)
  | _ -> Error (wrong_arity command_name)

let parse_range =
  parse_indexed_range
    (fun key start_index stop_index -> Lrange { key; start_index; stop_index })
    "lrange"

let parse_lrem = function
  | [ key; count; element ] ->
      Result.map (fun count -> Lrem { key; count; element }) (parse_int64 count)
  | _ -> Error (wrong_arity "lrem")

let parse_blpop = function
  | [ key; timeout ] -> (
      match parse_int64 timeout with
      | Error _ as error -> error
      | Ok seconds when seconds < 0L -> Error (Invalid_timeout timeout)
      | Ok 0L -> Ok (Blpop { key; timeout_ms = None })
      | Ok seconds
        when Int64.compare seconds (Int64.div Int64.max_int 1000L) > 0 ->
          Error (Invalid_timeout timeout)
      | Ok seconds ->
          Ok (Blpop { key; timeout_ms = Some (Int64.mul seconds 1000L) }))
  | _ -> Error (wrong_arity "blpop")

let parse_watch = function
  | [] -> Error (wrong_arity "watch")
  | keys -> Ok (Watch keys)

let parse_subscribe command_name constructor = function
  | [] -> Error (wrong_arity command_name)
  | channels -> Ok (constructor channels)

let parse_no_args command_name constructor = function
  | [] -> Ok constructor
  | _ -> Error (wrong_arity command_name)

let pair_up_fields command_name items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | field :: value :: rest -> loop ((field, value) :: acc) rest
    | _ -> Error (wrong_arity command_name)
  in
  loop [] items

let pair_up_scores command_name items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | score_raw :: member :: rest ->
        Result.bind (parse_float score_raw) (fun score ->
            loop ((score, member) :: acc) rest)
    | _ -> Error (wrong_arity command_name)
  in
  loop [] items

let triple_up_locations command_name items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | lon_raw :: lat_raw :: member :: rest ->
        Result.bind (parse_float lon_raw) (fun longitude ->
            Result.bind (parse_float lat_raw) (fun latitude ->
                loop ((longitude, latitude, member) :: acc) rest))
    | _ -> Error (wrong_arity command_name)
  in
  loop [] items

let parse_xadd = function
  | key :: id_spec :: fields ->
      if fields = [] then Error (wrong_arity "xadd")
      else
        Result.map
          (fun fields -> Xadd { key; id_spec; fields })
          (pair_up_fields "xadd" fields)
  | _ -> Error (wrong_arity "xadd")

let parse_zadd = function
  | key :: score_members when score_members <> [] ->
      Result.map
        (fun score_members -> Zadd { key; score_members })
        (pair_up_scores "zadd" score_members)
  | _ -> Error (wrong_arity "zadd")

let parse_key_member command_name constructor = function
  | [ key; member ] -> Ok (constructor key member)
  | _ -> Error (wrong_arity command_name)

let parse_zrank =
  parse_key_member "zrank" (fun key member -> Zrank { key; member })

let parse_zrange =
  parse_indexed_range
    (fun key start_index stop_index -> Zrange { key; start_index; stop_index })
    "zrange"

let parse_zscore =
  parse_key_member "zscore" (fun key member -> Zscore { key; member })

let parse_zrem = function
  | key :: members when members <> [] -> Ok (Zrem { key; members })
  | _ -> Error (wrong_arity "zrem")

let parse_geoadd = function
  | key :: locations when locations <> [] ->
      Result.map
        (fun locations -> Geoadd { key; locations })
        (triple_up_locations "geoadd" locations)
  | _ -> Error (wrong_arity "geoadd")

let parse_geopos = function
  | key :: members when members <> [] -> Ok (Geopos { key; members })
  | _ -> Error (wrong_arity "geopos")

let parse_geodist = function
  | [ key; member1; member2 ] ->
      Ok (Geodist { key; member1; member2; unit = None })
  | [ key; member1; member2; unit ] ->
      Ok (Geodist { key; member1; member2; unit = Some unit })
  | _ -> Error (wrong_arity "geodist")

let parse_geosearch = function
  | [ key; from; lon_raw; lat_raw; by; radius_raw; unit ]
    when String.uppercase_ascii from = "FROMLONLAT"
         && String.uppercase_ascii by = "BYRADIUS" ->
      Result.bind (parse_float lon_raw) (fun longitude ->
          Result.bind (parse_float lat_raw) (fun latitude ->
              Result.map
                (fun radius ->
                  Geosearch { key; longitude; latitude; radius; unit })
                (parse_float radius_raw)))
  | _ -> Error (wrong_arity "geosearch")

let parse_xrange = function
  | [ key; start_spec; end_spec ] -> Ok (Xrange { key; start_spec; end_spec })
  | _ -> Error (wrong_arity "xrange")

let parse_xread arguments =
  let rec parse_options block_ms = function
    | token :: timeout_raw :: rest when String.uppercase_ascii token = "BLOCK"
      -> (
        match parse_int64 timeout_raw with
        | Error _ as error -> error
        | Ok timeout when timeout < 0L -> Error (Invalid_timeout timeout_raw)
        | Ok 0L -> parse_options (Some (-1L)) rest
        | Ok timeout -> parse_options (Some timeout) rest)
    | token :: rest when String.uppercase_ascii token = "STREAMS" ->
        Ok (block_ms, rest)
    | _ -> Error (Syntax_error "expected STREAMS in XREAD")
  in
  match parse_options None arguments with
  | Error _ as error -> error
  | Ok (block_ms, rest) ->
      let count = List.length rest in
      if count = 0 || count mod 2 <> 0 then Error (wrong_arity "xread")
      else
        let half = count / 2 in
        let rec split n left right =
          if n = 0 then (List.rev left, right)
          else
            match right with
            | item :: rest -> split (n - 1) (item :: left) rest
            | [] -> assert false
        in
        let keys, ids = split half [] rest in
        Ok (Xread { block_ms; queries = List.combine keys ids })

let parse_wait = function
  | [ numreplicas_raw; timeout_raw ] -> (
      match (parse_int64 numreplicas_raw, parse_int64 timeout_raw) with
      | (Error _ as error), _ -> error
      | _, (Error _ as error) -> error
      | Ok numreplicas, Ok _ when numreplicas < 0L ->
          Error (Invalid_timeout numreplicas_raw)
      | Ok _, Ok timeout_ms when timeout_ms < 0L ->
          Error (Invalid_timeout timeout_raw)
      | Ok numreplicas, Ok 0L -> Ok (Wait { numreplicas; timeout_ms = None })
      | Ok numreplicas, Ok timeout_ms ->
          Ok (Wait { numreplicas; timeout_ms = Some timeout_ms }))
  | _ -> Error (wrong_arity "wait")

let parse_info = function
  | [] -> Ok (Info None)
  | [ section ] -> Ok (Info (Some section))
  | _ -> Error (wrong_arity "info")

let parse_replconf = function
  | [] -> Error (wrong_arity "replconf")
  | args -> Ok (Replconf args)

let parse_psync = function
  | [ replid; offset ] -> Ok (Psync { replid; offset })
  | _ -> Error (wrong_arity "psync")

let parse_auth = function
  | [ password ] -> Ok (Auth password)
  | _ -> Error (wrong_arity "auth")

let parse_acl = function
  | [ subcommand ] when String.uppercase_ascii subcommand = "WHOAMI" ->
      Ok AclWhoami
  | [ subcommand; user ] when String.uppercase_ascii subcommand = "GETUSER" ->
      Ok (AclGetuser user)
  | _ -> Error (wrong_arity "acl")

let parse arguments =
  match arguments with
  | [] -> Error (Syntax_error "empty command")
  | command_name :: rest -> (
      let upper = String.uppercase_ascii command_name in
      match (upper, rest) with
      | "PING", [] -> Ok (Ping None)
      | "PING", [ message ] -> Ok (Ping (Some message))
      | "ECHO", [ value ] -> Ok (Echo value)
      | "INFO", rest -> parse_info rest
      | "TYPE", [ key ] -> Ok (Type key)
      | "GET", [ key ] -> Ok (Get key)
      | "SET", rest -> parse_set rest
      | "RPUSH", rest ->
          parse_nonempty_values
            (fun key values -> Rpush { key; values })
            "rpush" rest
      | "LPUSH", rest ->
          parse_nonempty_values
            (fun key values -> Lpush { key; values })
            "lpush" rest
      | "LRANGE", rest -> parse_range rest
      | "LLEN", [ key ] -> Ok (Llen key)
      | "LREM", rest -> parse_lrem rest
      | "LPOP", [ key ] -> Ok (Lpop key)
      | "BLPOP", rest -> parse_blpop rest
      | "INCR", [ key ] -> Ok (Incr key)
      | "MULTI", rest -> parse_no_args "multi" Multi rest
      | "EXEC", rest -> parse_no_args "exec" Exec rest
      | "DISCARD", rest -> parse_no_args "discard" Discard rest
      | "WATCH", rest -> parse_watch rest
      | "UNWATCH", rest -> parse_no_args "unwatch" Unwatch rest
      | "REPLCONF", rest -> parse_replconf rest
      | "PSYNC", rest -> parse_psync rest
      | "WAIT", rest -> parse_wait rest
      | "AUTH", rest -> parse_auth rest
      | "ACL", rest -> parse_acl rest
      | "SUBSCRIBE", rest ->
          parse_subscribe "subscribe" (fun channels -> Subscribe channels) rest
      | "UNSUBSCRIBE", rest -> Ok (Unsubscribe rest)
      | "PUBLISH", [ channel; message ] -> Ok (Publish { channel; message })
      | "ZADD", rest -> parse_zadd rest
      | "ZRANK", rest -> parse_zrank rest
      | "ZRANGE", rest -> parse_zrange rest
      | "ZCARD", [ key ] -> Ok (Zcard key)
      | "ZSCORE", rest -> parse_zscore rest
      | "ZREM", rest -> parse_zrem rest
      | "GEOADD", rest -> parse_geoadd rest
      | "GEOPOS", rest -> parse_geopos rest
      | "GEODIST", rest -> parse_geodist rest
      | "GEOSEARCH", rest -> parse_geosearch rest
      | "XADD", rest -> parse_xadd rest
      | "XRANGE", rest -> parse_xrange rest
      | "XREAD", rest -> parse_xread rest
      | "PING", _ -> Error (wrong_arity "ping")
      | "ECHO", _ -> Error (wrong_arity "echo")
      | "TYPE", _ -> Error (wrong_arity "type")
      | "GET", _ -> Error (wrong_arity "get")
      | "LLEN", _ -> Error (wrong_arity "llen")
      | "LPOP", _ -> Error (wrong_arity "lpop")
      | "INCR", _ -> Error (wrong_arity "incr")
      | "PUBLISH", _ -> Error (wrong_arity "publish")
      | "ZCARD", _ -> Error (wrong_arity "zcard")
      | _ -> Error (Unknown_command command_name))

let describe_value value =
  let maximum = 64 in
  if String.length value <= maximum then value
  else String.sub value 0 maximum ^ "..."

let to_error_message = function
  | Unknown_command command ->
      Printf.sprintf "unknown command '%s'" (describe_value command)
  | Wrong_arity command ->
      Printf.sprintf "wrong number of arguments for '%s' command"
        (describe_value command)
  | Syntax_error message -> Printf.sprintf "syntax error: %s" message
  | Invalid_integer value ->
      Printf.sprintf "value is not an integer or out of range: %s"
        (describe_value value)
  | Invalid_expiry value ->
      Printf.sprintf "invalid expire time in command: %s" (describe_value value)
  | Invalid_timeout value ->
      Printf.sprintf "invalid timeout in command: %s" (describe_value value)
  | Invalid_float value ->
      Printf.sprintf "value is not a valid float: %s" (describe_value value)

let format_score score =
  let fractional, _ = modf score in
  if fractional = 0. then Printf.sprintf "%.0f" score
  else Printf.sprintf "%.17g" score

let to_arguments = function
  | Ping None -> Some [ "PING" ]
  | Ping (Some message) -> Some [ "PING"; message ]
  | Echo value -> Some [ "ECHO"; value ]
  | Info None -> Some [ "INFO" ]
  | Info (Some section) -> Some [ "INFO"; section ]
  | Type key -> Some [ "TYPE"; key ]
  | Get key -> Some [ "GET"; key ]
  | Set { key; value; expiry = None } -> Some [ "SET"; key; value ]
  | Set { key; value; expiry = Some (Relative_ms expiry_ms) } ->
      Some [ "SET"; key; value; "PX"; Int64.to_string expiry_ms ]
  | Set { key; value; expiry = Some (Absolute_ms expires_at_ms) } ->
      Some [ "SET"; key; value; "PXAT"; Int64.to_string expires_at_ms ]
  | Rpush { key; values } -> Some ("RPUSH" :: key :: values)
  | Lpush { key; values } -> Some ("LPUSH" :: key :: values)
  | Lrange { key; start_index; stop_index } ->
      Some
        [
          "LRANGE"; key; Int64.to_string start_index; Int64.to_string stop_index;
        ]
  | Llen key -> Some [ "LLEN"; key ]
  | Lrem { key; count; element } ->
      Some [ "LREM"; key; Int64.to_string count; element ]
  | Lpop key -> Some [ "LPOP"; key ]
  | Blpop { key; timeout_ms = None } -> Some [ "BLPOP"; key; "0" ]
  | Blpop { key; timeout_ms = Some timeout_ms } ->
      Some [ "BLPOP"; key; Int64.to_string (Int64.div timeout_ms 1000L) ]
  | Incr key -> Some [ "INCR"; key ]
  | Multi -> Some [ "MULTI" ]
  | Exec -> Some [ "EXEC" ]
  | Discard -> Some [ "DISCARD" ]
  | Watch keys -> Some ("WATCH" :: keys)
  | Unwatch -> Some [ "UNWATCH" ]
  | Replconf args -> Some ("REPLCONF" :: args)
  | Psync { replid; offset } -> Some [ "PSYNC"; replid; offset ]
  | Wait { numreplicas; timeout_ms = None } ->
      Some [ "WAIT"; Int64.to_string numreplicas; "0" ]
  | Wait { numreplicas; timeout_ms = Some timeout_ms } ->
      Some [ "WAIT"; Int64.to_string numreplicas; Int64.to_string timeout_ms ]
  | Auth password -> Some [ "AUTH"; password ]
  | AclWhoami -> Some [ "ACL"; "WHOAMI" ]
  | AclGetuser user -> Some [ "ACL"; "GETUSER"; user ]
  | Subscribe channels -> Some ("SUBSCRIBE" :: channels)
  | Unsubscribe channels -> Some ("UNSUBSCRIBE" :: channels)
  | Publish { channel; message } -> Some [ "PUBLISH"; channel; message ]
  | Zadd { key; score_members } ->
      Some
        ("ZADD" :: key
        :: List.concat_map
             (fun (score, member) -> [ format_score score; member ])
             score_members)
  | Zrank { key; member } -> Some [ "ZRANK"; key; member ]
  | Zrange { key; start_index; stop_index } ->
      Some
        [
          "ZRANGE"; key; Int64.to_string start_index; Int64.to_string stop_index;
        ]
  | Zcard key -> Some [ "ZCARD"; key ]
  | Zscore { key; member } -> Some [ "ZSCORE"; key; member ]
  | Zrem { key; members } -> Some ("ZREM" :: key :: members)
  | Geoadd { key; locations } ->
      Some
        ("GEOADD" :: key
        :: List.concat_map
             (fun (longitude, latitude, member) ->
               [ format_score longitude; format_score latitude; member ])
             locations)
  | Geopos { key; members } -> Some ("GEOPOS" :: key :: members)
  | Geodist { key; member1; member2; unit = None } ->
      Some [ "GEODIST"; key; member1; member2 ]
  | Geodist { key; member1; member2; unit = Some unit } ->
      Some [ "GEODIST"; key; member1; member2; unit ]
  | Geosearch { key; longitude; latitude; radius; unit } ->
      Some
        [
          "GEOSEARCH";
          key;
          "FROMLONLAT";
          format_score longitude;
          format_score latitude;
          "BYRADIUS";
          format_score radius;
          unit;
        ]
  | Xadd { key; id_spec; fields } ->
      Some
        ("XADD" :: key :: id_spec
        :: List.concat_map (fun (f, v) -> [ f; v ]) fields)
  | Xrange { key; start_spec; end_spec } ->
      Some [ "XRANGE"; key; start_spec; end_spec ]
  | Xread { block_ms; queries } ->
      let keys = List.map fst queries in
      let ids = List.map snd queries in
      let block_part =
        match block_ms with
        | None -> []
        | Some ms -> [ "BLOCK"; Int64.to_string ms ]
      in
      Some (block_part @ [ "STREAMS" ] @ keys @ ids)

let is_write = function
  | Set _ | Rpush _ | Lpush _ | Lrem _ | Lpop _ | Blpop _ | Incr _ | Zadd _
  | Zrem _ | Geoadd _ | Xadd _ ->
      true
  | _ -> false
