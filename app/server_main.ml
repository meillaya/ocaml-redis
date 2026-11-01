let port = ref 6379
let replicaof = ref None
let rdb_file = ref None
let requirepass = ref None
let masterauth = ref None
let max_clients = ref 1024
let max_output_buffer = ref (64 * 1024 * 1024)
let maxmemory = ref 268_435_456L
let tls_certificate = ref None
let tls_key = ref None
let kb = 1024L
let mb = Int64.mul kb kb
let gb = Int64.mul mb kb

let parse_memory_bytes option_name value =
  let suffix_number suffix multiplier =
    let suffix_length = String.length suffix in
    let length = String.length value in
    if
      length > suffix_length
      && String.lowercase_ascii
           (String.sub value (length - suffix_length) suffix_length)
         = suffix
    then Some (String.sub value 0 (length - suffix_length), multiplier)
    else None
  in
  let number, multiplier =
    match suffix_number "kb" kb with
    | Some parsed -> parsed
    | None -> (
        match suffix_number "mb" mb with
        | Some parsed -> parsed
        | None -> (
            match suffix_number "gb" gb with
            | Some parsed -> parsed
            | None -> (value, 1L)))
  in
  match Redis.Integer.of_string_opt number with
  | Some bytes
    when bytes >= 0L
         && Int64.compare bytes (Int64.div Int64.max_int multiplier) <= 0 ->
      Int64.mul bytes multiplier
  | _ ->
      raise
        (Arg.Bad
           (option_name
          ^ " expects a non-negative byte count with an optional kb, mb, or \
             gb              suffix"))

let split_host_port value =
  if String.length value > 0 && value.[0] = '[' then
    match String.index_opt value ']' with
    | Some closing
      when closing + 1 < String.length value && value.[closing + 1] = ':' ->
        let host = String.sub value 1 (closing - 1) in
        let port_text =
          String.sub value (closing + 2) (String.length value - closing - 2)
        in
        Some (host, port_text)
    | _ -> None
  else
    match String.rindex_opt value ':' with
    | Some separator ->
        Some
          ( String.sub value 0 separator,
            String.sub value (separator + 1)
              (String.length value - separator - 1) )
    | None -> None

let set_replicaof value =
  match split_host_port value with
  | Some (host, port_text) -> (
      match int_of_string_opt port_text with
      | Some upstream_port when upstream_port > 0 && upstream_port <= 65535 ->
          replicaof := Some (host, upstream_port)
      | _ -> raise (Arg.Bad "--replicaof expects HOST:PORT with a valid port"))
  | None -> raise (Arg.Bad "--replicaof expects HOST:PORT")

let speclist =
  [
    ("--port", Arg.Set_int port, "TCP port to bind (default: 6379)");
    ( "--replicaof",
      Arg.String set_replicaof,
      "Configure replica upstream as HOST:PORT" );
    ( "--masterauth",
      Arg.String (fun password -> masterauth := Some password),
      "Password to AUTH against the upstream master" );
    ( "--rdb-file",
      Arg.String (fun path -> rdb_file := Some path),
      "Load a standalone RDB file on startup" );
    ( "--requirepass",
      Arg.String (fun password -> requirepass := Some password),
      "Require clients to authenticate with AUTH <password>" );
    ( "--maxclients",
      Arg.Set_int max_clients,
      "Maximum number of simultaneous clients (default: 1024)" );
    ( "--max-output-buffer",
      Arg.String
        (fun value ->
          let bytes = parse_memory_bytes "--max-output-buffer" value in
          if Int64.compare bytes (Int64.of_int max_int) > 0 then
            raise (Arg.Bad "--max-output-buffer is too large");
          max_output_buffer := Int64.to_int bytes),
      "Per-client output buffer ceiling (default: 64mb)" );
    ( "--maxmemory",
      Arg.String
        (fun value -> maxmemory := parse_memory_bytes "--maxmemory" value),
      "Approximate memory ceiling; zero disables it (default: 256mb)" );
    ( "--tls-cert",
      Arg.String (fun path -> tls_certificate := Some path),
      "PEM certificate chain; enables TLS for client connections" );
    ( "--tls-key",
      Arg.String (fun path -> tls_key := Some path),
      "PEM private key for --tls-cert" );
  ]

let () =
  Arg.parse speclist
    (fun _ -> ())
    "ocaml-redis-server [--port PORT] [--replicaof HOST:PORT] [--masterauth \
     PASSWORD] [--rdb-file PATH] [--requirepass PASSWORD] [--maxclients \
     N]      [--max-output-buffer BYTES] [--maxmemory BYTES] [--tls-cert PATH \
     --tls-key PATH]";
  if !port < 1 || !port > 65535 then (
    prerr_endline "ocaml-redis-server: --port must be between 1 and 65535";
    exit 2);
  if !max_clients <= 0 then (
    prerr_endline "ocaml-redis-server: --maxclients must be greater than zero";
    exit 2);
  if !max_output_buffer <= 0 then (
    prerr_endline
      "ocaml-redis-server: --max-output-buffer must be greater than zero";
    exit 2);
  if Option.is_some !tls_certificate <> Option.is_some !tls_key then (
    prerr_endline
      "ocaml-redis-server: --tls-cert and --tls-key must be provided together";
    exit 2);
  try
    Redis.Server.run ~port:!port ~replicaof:!replicaof ~rdb_file:!rdb_file
      ~requirepass:!requirepass ~masterauth:!masterauth
      ~max_clients:!max_clients ~max_output_buffer:!max_output_buffer
      ~maxmemory:!maxmemory ~tls_certificate:!tls_certificate ~tls_key:!tls_key
  with
  | Failure message ->
      prerr_endline ("ocaml-redis-server: " ^ message);
      exit 1
  | Unix.Unix_error (error, operation, argument) ->
      prerr_endline
        (Printf.sprintf "ocaml-redis-server: %s failed on %s: %s" operation
           argument (Unix.error_message error));
      exit 1
  | caught ->
      prerr_endline ("ocaml-redis-server: " ^ Printexc.to_string caught);
      exit 1
