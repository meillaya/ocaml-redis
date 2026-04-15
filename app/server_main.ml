let port = ref 6379
let replicaof = ref None
let rdb_file = ref None
let requirepass = ref None

let set_replicaof value =
  match String.split_on_char ':' value with
  | [ host; port_text ] -> (
      match int_of_string_opt port_text with
      | Some port -> replicaof := Some (host, port)
      | None -> raise (Arg.Bad "--replicaof expects HOST:PORT"))
  | _ -> raise (Arg.Bad "--replicaof expects HOST:PORT")

let speclist =
  [
    ("--port", Arg.Set_int port, "TCP port to bind (default: 6379)");
    ( "--replicaof",
      Arg.String set_replicaof,
      "Configure replica upstream as HOST:PORT" );
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
    "ocaml-redis-server [--port PORT] [--replicaof HOST:PORT] [--rdb-file \
     PATH] [--requirepass PASSWORD]";
  Redis.Server.run ~port:!port ~replicaof:!replicaof ~rdb_file:!rdb_file
    ~requirepass:!requirepass
