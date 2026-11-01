let port = ref 6379
let replicaof = ref None
let rdb_file = ref None
let requirepass = ref None
let masterauth = ref None

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
  ]

let () =
  Arg.parse speclist
    (fun _ -> ())
    "ocaml-redis-server [--port PORT] [--replicaof HOST:PORT] [--masterauth \
     PASSWORD] [--rdb-file PATH] [--requirepass PASSWORD]";
  if !port < 1 || !port > 65535 then (
    prerr_endline "ocaml-redis-server: --port must be between 1 and 65535";
    exit 2);
  try
    Redis.Server.run ~port:!port ~replicaof:!replicaof ~rdb_file:!rdb_file
      ~requirepass:!requirepass ~masterauth:!masterauth
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
