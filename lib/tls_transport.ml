(* Nonblocking TLS transport.

   The event loop owns raw sockets, so this module drives [Tls.Engine] directly
   instead of using the blocking [Tls_unix] wrapper. Ciphertext goes into the
   client's ordinary output queue; plaintext is handed back to the RESP parser. *)

type server = Tls.Config.server
type t = { mutable state : Tls.Engine.state; pending_plaintext : Buffer.t }
type feed_result = { plaintext : string; response : string; eof : bool }

let read_file path = In_channel.with_open_bin path In_channel.input_all

let decode_error = function
  | Ok value -> Ok value
  | Error (`Msg message) -> Error message

let server ~certificate_file ~key_file =
  try
    match
      ( decode_error
          (X509.Certificate.decode_pem_multiple (read_file certificate_file)),
        decode_error (X509.Private_key.decode_pem (read_file key_file)) )
    with
    | Ok [], _ -> Error "TLS certificate file contains no certificates"
    | Ok certificates, Ok key -> (
        match
          Tls.Config.server ~certificates:(`Single (certificates, key)) ()
        with
        | Ok server -> Ok server
        | Error (`Msg message) -> Error message)
    | Error message, _ | _, Error message -> Error message
  with Sys_error message -> Error message

let create server =
  { state = Tls.Engine.server server; pending_plaintext = Buffer.create 0 }

let handshake_in_progress t = Tls.Engine.handshake_in_progress t.state

let send t plaintext =
  if String.length plaintext = 0 then Ok ""
  else if Tls.Engine.handshake_in_progress t.state then (
    Buffer.add_string t.pending_plaintext plaintext;
    Ok "")
  else
    let messages =
      if Buffer.length t.pending_plaintext = 0 then [ plaintext ]
      else [ Buffer.contents t.pending_plaintext; plaintext ]
    in
    match Tls.Engine.send_application_data t.state messages with
    | None -> Error "TLS session is not ready for application data"
    | Some (state, ciphertext) ->
        t.state <- state;
        Buffer.clear t.pending_plaintext;
        Ok ciphertext

let feed t ciphertext =
  match Tls.Engine.handle_tls t.state ciphertext with
  | Error (failure, `Response response) ->
      Error (Tls.Engine.string_of_failure failure, response)
  | Ok (state, eof, response, data) ->
      t.state <- state;
      let response =
        match response with
        | `Response None -> ""
        | `Response (Some value) -> value
      in
      let plaintext =
        match data with `Data None -> "" | `Data (Some value) -> value
      in
      let response =
        if Tls.Engine.handshake_in_progress t.state then response
        else if Buffer.length t.pending_plaintext = 0 then response
        else
          match
            Tls.Engine.send_application_data t.state
              [ Buffer.contents t.pending_plaintext ]
          with
          | None -> response
          | Some (state, ciphertext) ->
              t.state <- state;
              Buffer.clear t.pending_plaintext;
              response ^ ciphertext
      in
      Ok { plaintext; response; eof = Option.is_some eof }

let close_notify t =
  let state, ciphertext = Tls.Engine.send_close_notify t.state in
  t.state <- state;
  ciphertext
