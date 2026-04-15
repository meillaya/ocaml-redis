type reply =
  | Simple_string of string
  | Error of string
  | Prefixed_error of { prefix : string; message : string }
  | Integer of int64
  | Bulk_string of string option
  | Array of reply list
  | Null_array

type frame =
  | Simple_string_frame of string
  | Error_frame of string
  | Bulk_string_frame of string option
  | Command_frame of string list

let rec encode_reply = function
  | Simple_string value -> "+" ^ value ^ "\r\n"
  | Error message -> "-ERR " ^ message ^ "\r\n"
  | Prefixed_error { prefix; message } ->
      Printf.sprintf "-%s %s\r\n" prefix message
  | Integer value -> ":" ^ Int64.to_string value ^ "\r\n"
  | Bulk_string None -> "$-1\r\n"
  | Bulk_string (Some value) ->
      Printf.sprintf "$%d\r\n%s\r\n" (String.length value) value
  | Array values ->
      let encoded_values =
        values |> List.map encode_reply |> String.concat ""
      in
      Printf.sprintf "*%d\r\n%s" (List.length values) encoded_values
  | Null_array -> "*-1\r\n"

let encode_command arguments =
  arguments |> List.map (fun arg -> Bulk_string (Some arg)) |> fun items ->
  Array items |> encode_reply

let find_crlf input ~start =
  let rec loop index =
    if index + 1 >= String.length input then None
    else if input.[index] = '\r' && input.[index + 1] = '\n' then Some index
    else loop (index + 1)
  in
  loop start

let parse_decimal_int label value =
  match int_of_string_opt value with
  | Some parsed -> Stdlib.Ok parsed
  | None ->
      Stdlib.Error (Printf.sprintf "protocol error: invalid %s %S" label value)

let parse_line input ~start =
  match find_crlf input ~start with
  | None -> None
  | Some stop ->
      let line = String.sub input start (stop - start) in
      Some (line, stop + 2)

let parse_bulk_string input ~start =
  match parse_line input ~start with
  | None -> None
  | Some (length_line, after_length) ->
      Some
        (match parse_decimal_int "bulk length" length_line with
        | Stdlib.Error _ as error -> error
        | Stdlib.Ok length when length < 0 -> Stdlib.Ok (None, after_length)
        | Stdlib.Ok length ->
            let needed = after_length + length + 2 in
            if String.length input < needed then raise Exit
            else if
              input.[after_length + length] <> '\r'
              || input.[after_length + length + 1] <> '\n'
            then
              Stdlib.Error "protocol error: bulk string missing CRLF terminator"
            else
              let payload = String.sub input after_length length in
              Stdlib.Ok (Some payload, needed))

let parse_request input =
  if String.length input = 0 then None
  else if input.[0] <> '*' then
    Some (Stdlib.Error "protocol error: expected array request")
  else
    match parse_line input ~start:1 with
    | None -> None
    | Some (arity_line, cursor_after_arity) -> (
        let parsed () =
          match parse_decimal_int "array length" arity_line with
          | Stdlib.Error _ as error -> error
          | Stdlib.Ok arity when arity < 0 ->
              Stdlib.Error "protocol error: negative array length"
          | Stdlib.Ok arity ->
              let rec consume count cursor acc =
                if count = 0 then Stdlib.Ok (List.rev acc, cursor)
                else if cursor >= String.length input then raise Exit
                else if input.[cursor] <> '$' then
                  Stdlib.Error "protocol error: expected bulk string argument"
                else
                  match parse_bulk_string input ~start:(cursor + 1) with
                  | None -> raise Exit
                  | Some (Stdlib.Error _ as error) -> error
                  | Some (Stdlib.Ok (None, _)) ->
                      Stdlib.Error
                        "protocol error: null bulk strings are not supported"
                  | Some (Stdlib.Ok (Some payload, next_cursor)) ->
                      consume (count - 1) next_cursor (payload :: acc)
              in
              consume arity cursor_after_arity []
        in
        try Some (parsed ()) with Exit -> None)

let parse_frame input =
  if String.length input = 0 then None
  else
    match input.[0] with
    | '*' ->
        Option.map
          (Result.map (fun (args, consumed) -> (Command_frame args, consumed)))
          (parse_request input)
    | '+' -> (
        match parse_line input ~start:1 with
        | None -> None
        | Some (line, consumed) ->
            Some (Stdlib.Ok (Simple_string_frame line, consumed)))
    | '-' -> (
        match parse_line input ~start:1 with
        | None -> None
        | Some (line, consumed) -> Some (Stdlib.Ok (Error_frame line, consumed))
        )
    | '$' -> (
        match parse_bulk_string input ~start:1 with
        | None -> None
        | Some (Stdlib.Error _ as error) -> Some error
        | Some (Stdlib.Ok (payload, consumed)) ->
            Some (Stdlib.Ok (Bulk_string_frame payload, consumed)))
    | _ -> Some (Stdlib.Error "protocol error: unsupported frame type")
