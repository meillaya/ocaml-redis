type entry = { key : string; value : string; expires_at_ms : int64 option }
type cursor = { data : bytes; mutable pos : int; len : int }
type len_encoding = Plain_len of int64 | Encoded of int

let failf fmt = Printf.ksprintf (fun message -> Error message) fmt

let int64_to_int_opt value =
  if
    Int64.compare value (Int64.of_int max_int) > 0
    || Int64.compare value (Int64.of_int min_int) < 0
  then None
  else Some (Int64.to_int value)

let create_cursor data =
  let data = Bytes.of_string data in
  { data; pos = 0; len = Bytes.length data }

let remaining cursor = cursor.len - cursor.pos

let read_exact cursor n =
  if n < 0 || remaining cursor < n then None
  else
    let value = Bytes.sub_string cursor.data cursor.pos n in
    cursor.pos <- cursor.pos + n;
    Some value

let read_byte cursor =
  match read_exact cursor 1 with
  | Some value -> Some (Char.code value.[0])
  | None -> None

let read_u32_be cursor =
  match read_exact cursor 4 with
  | None -> None
  | Some value ->
      let open Int64 in
      Some
        (logor
           (shift_left (of_int (Char.code value.[0])) 24)
           (logor
              (shift_left (of_int (Char.code value.[1])) 16)
              (logor
                 (shift_left (of_int (Char.code value.[2])) 8)
                 (of_int (Char.code value.[3])))))

let read_u32_le cursor =
  match read_exact cursor 4 with
  | None -> None
  | Some value ->
      let open Int64 in
      Some
        (logor
           (of_int (Char.code value.[0]))
           (logor
              (shift_left (of_int (Char.code value.[1])) 8)
              (logor
                 (shift_left (of_int (Char.code value.[2])) 16)
                 (shift_left (of_int (Char.code value.[3])) 24))))

let read_u64_le cursor =
  match read_exact cursor 8 with
  | None -> None
  | Some value ->
      let open Int64 in
      let byte index shift =
        shift_left (of_int (Char.code value.[index])) shift
      in
      Some
        (logor (byte 0 0)
           (logor (byte 1 8)
              (logor (byte 2 16)
                 (logor (byte 3 24)
                    (logor (byte 4 32)
                       (logor (byte 5 40) (logor (byte 6 48) (byte 7 56))))))))

let read_len cursor =
  match read_byte cursor with
  | None -> None
  | Some first ->
      let top = first lsr 6 in
      if top = 0 then Some (Plain_len (Int64.of_int (first land 0x3F)))
      else if top = 1 then
        match read_byte cursor with
        | None -> None
        | Some second ->
            Some
              (Plain_len (Int64.of_int (((first land 0x3F) lsl 8) lor second)))
      else if first = 0x80 then
        Option.map (fun value -> Plain_len value) (read_u32_be cursor)
      else if first = 0x81 then
        match read_exact cursor 8 with
        | None -> None
        | Some value ->
            let rec loop acc index =
              if index = 8 then acc
              else
                loop
                  Int64.(
                    logor (shift_left acc 8) (of_int (Char.code value.[index])))
                  (index + 1)
            in
            Some (Plain_len (loop 0L 0))
      else Some (Encoded (first land 0x3F))

let read_string cursor =
  match read_len cursor with
  | None -> failf "unexpected EOF while reading string length"
  | Some (Plain_len len) -> (
      match int64_to_int_opt len with
      | None -> failf "string length out of range: %Ld" len
      | Some len when len < 0 -> failf "negative string length: %d" len
      | Some len -> (
          match read_exact cursor len with
          | Some value -> Ok value
          | None -> failf "unexpected EOF while reading string payload"))
  | Some (Encoded 0) -> (
      match read_byte cursor with
      | Some value ->
          Ok
            (Int64.to_string
               Int64.(
                 of_int (if value land 0x80 <> 0 then value - 0x100 else value)))
      | None -> failf "unexpected EOF while reading int8 encoded string")
  | Some (Encoded 1) -> (
      match read_exact cursor 2 with
      | None -> failf "unexpected EOF while reading int16 encoded string"
      | Some value ->
          let raw = Char.code value.[0] lor (Char.code value.[1] lsl 8) in
          let signed = if raw land 0x8000 <> 0 then raw - 0x10000 else raw in
          Ok (string_of_int signed))
  | Some (Encoded 2) -> (
      match read_exact cursor 4 with
      | None -> failf "unexpected EOF while reading int32 encoded string"
      | Some value ->
          let open Int64 in
          let raw =
            logor
              (of_int (Char.code value.[0]))
              (logor
                 (shift_left (of_int (Char.code value.[1])) 8)
                 (logor
                    (shift_left (of_int (Char.code value.[2])) 16)
                    (shift_left (of_int (Char.code value.[3])) 24)))
          in
          let signed =
            if logand raw 0x8000_0000L <> 0L then sub raw 0x1_0000_0000L
            else raw
          in
          Ok (Int64.to_string signed))
  | Some (Encoded 3) -> failf "LZF-compressed RDB strings are not supported"
  | Some (Encoded encoding) ->
      failf "unsupported RDB string encoding kind: %d" encoding

let read_version header =
  if
    String.length header <> 9 || not (String.starts_with ~prefix:"REDIS" header)
  then failf "invalid RDB header"
  else
    match int_of_string_opt (String.sub header 5 4) with
    | Some version -> Ok version
    | None -> failf "invalid RDB version"

let max_file_size = 256 * 1024 * 1024

let load_file path =
  try
    let stats = Unix.stat path in
    if stats.Unix.st_size > max_file_size then
      failf "RDB file is larger than the supported %d-byte limit" max_file_size
    else
      let data =
        Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all
      in
      let cursor = create_cursor data in
      match read_exact cursor 9 with
      | None -> failf "RDB file is too short"
      | Some header -> (
          match read_version header with
          | Error _ as error -> error
          | Ok version ->
              let rec loop current_db pending_expiry acc =
                match read_byte cursor with
                | None -> failf "unexpected EOF before RDB EOF opcode"
                | Some 255 ->
                    if remaining cursor >= 8 then ignore (read_exact cursor 8);
                    Ok (List.rev acc)
                | Some 250 -> (
                    match (read_string cursor, read_string cursor) with
                    | Ok _, Ok _ -> loop current_db pending_expiry acc
                    | (Error _ as error), _ -> error
                    | _, (Error _ as error) -> error)
                | Some 251 -> (
                    match (read_len cursor, read_len cursor) with
                    | Some _, Some _ -> loop current_db pending_expiry acc
                    | _ -> failf "unexpected EOF in RESIZEDB payload")
                | Some 252 -> (
                    match read_u64_le cursor with
                    | Some expires_at_ms ->
                        loop current_db (Some expires_at_ms) acc
                    | None -> failf "unexpected EOF in EXPIRETIME_MS payload")
                | Some 253 -> (
                    match read_u32_le cursor with
                    | Some expires_at_s ->
                        loop current_db
                          (Some Int64.(mul expires_at_s 1000L))
                          acc
                    | None -> failf "unexpected EOF in EXPIRETIME payload")
                | Some 254 -> (
                    match read_len cursor with
                    | Some (Plain_len db) -> (
                        match int64_to_int_opt db with
                        | Some db -> loop db pending_expiry acc
                        | None -> failf "database index too large: %Ld" db)
                    | Some (Encoded _) ->
                        failf "encoded SELECTDB length is not supported"
                    | None -> failf "unexpected EOF in SELECTDB payload")
                | Some 0 -> (
                    match (read_string cursor, read_string cursor) with
                    | Ok key, Ok value ->
                        let acc =
                          if current_db = 0 then
                            { key; value; expires_at_ms = pending_expiry }
                            :: acc
                          else acc
                        in
                        loop current_db None acc
                    | (Error _ as error), _ -> error
                    | _, (Error _ as error) -> error)
                | Some opcode ->
                    failf "unsupported RDB opcode/type: %d (version %d)" opcode
                      version
              in
              loop 0 None [])
  with
  | Sys_error message -> Error message
  | Unix.Unix_error (error, operation, argument) ->
      Error
        (Printf.sprintf "%s %S: %s" operation argument
           (Unix.error_message error))
