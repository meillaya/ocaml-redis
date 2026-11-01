type blocked_state =
  | Blocked_list_pop of { key : string; deadline_ms : int64 option }
  | Blocked_stream_read of {
      queries : (string * string) list;
      keys : string list;
      deadline_ms : int64 option;
    }
  | Blocked_wait of {
      required_replicas : int64;
      offset : int64;
      deadline_ms : int64 option;
    }

type tx_state = { mutable queue_rev : Command.t list; mutable dirty : bool }

type client = {
  fd : Unix.file_descr;
  mutable buffer : string;
  mutable output : Buffer.t;
  mutable blocked : blocked_state option;
  mutable tx : tx_state option;
  mutable authenticated : bool;
  watched : (string, int64) Hashtbl.t;
  subscriptions : (string, unit) Hashtbl.t;
  mutable subscription_order_rev : string list;
  mutable replica_peer : bool;
  mutable replica_ack_offset : int64;
  mutable last_write_offset : int64;
  max_output_buffer : int;
  mutable tls : Tls_transport.t option;
  mutable handshake_deadline : int64 option;
  mutable closed : bool;
}

type replica_upstream = {
  host : string;
  port : int;
  fd : Unix.file_descr;
  mutable buffer : string;
  mutable replid : string;
  mutable offset : int64;
  mutable link_status : string;
  mutable output : Buffer.t;
  max_output_buffer : int;
  mutable tx_queue : Command.t list option;
}

type source = External_client | Master_stream

type sync_result =
  | Full_resync of replica_upstream * Rdb.entry list
  | Partial_resync of replica_upstream

(* A single request may legitimately be large, but the daemon must not buffer
   an unbounded stream from a client that never completes a frame. 128 MiB is
   twice the per-bulk ceiling, leaving room for framing and pipelining. *)
let max_client_buffer = 128 * 1024 * 1024
let tls_handshake_timeout_ms = 10_000L
let max_rdb_payload = 64 * 1024 * 1024

let write_all fd data =
  let rec loop offset =
    if offset < String.length data then
      match
        Unix.write_substring fd data offset (String.length data - offset)
      with
      | 0 -> raise End_of_file
      | written -> loop (offset + written)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop offset
  in
  loop 0

let close_client client =
  if not client.closed then (
    client.closed <- true;
    (match client.tls with
    | Some tls -> (
        try
          let ciphertext = Tls_transport.close_notify tls in
          if ciphertext <> "" then
            ignore
              (Unix.write_substring client.fd ciphertext 0
                 (String.length ciphertext))
        with Unix.Unix_error _ | _ -> ())
    | None -> ());
    Buffer.clear client.output;
    try Unix.close client.fd with Unix.Unix_error _ -> ())

(* Flush as much queued output as the socket accepts. Any remainder stays at
   the front of the buffer for the next write-ready event. *)
let flush_client client =
  if (not client.closed) && Buffer.length client.output > 0 then (
    let data = Buffer.contents client.output in
    Buffer.clear client.output;
    let rec write offset =
      if offset < String.length data then
        match
          Unix.write_substring client.fd data offset
            (String.length data - offset)
        with
        | 0 -> close_client client
        | written -> write (offset + written)
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> write offset
        | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
            Buffer.add_substring client.output data offset
              (String.length data - offset)
        | exception Unix.Unix_error _ -> close_client client
        | exception End_of_file -> close_client client
    in
    write 0)

let queue_raw client data =
  if not client.closed then
    if
      String.length data > client.max_output_buffer
      || Buffer.length client.output + String.length data
         > client.max_output_buffer
    then close_client client
    else (
      Buffer.add_string client.output data;
      flush_client client)

let send_raw client data =
  match client.tls with
  | None -> queue_raw client data
  | Some tls -> (
      match Tls_transport.send tls data with
      | Ok ciphertext -> queue_raw client ciphertext
      | Error message ->
          prerr_endline ("ocaml-redis: TLS send failed: " ^ message);
          close_client client
      | exception exception_ ->
          prerr_endline
            ("ocaml-redis: TLS send raised " ^ Printexc.to_string exception_);
          close_client client)

let send_reply client reply =
  if not client.closed then send_raw client (Resp.encode_reply reply)

let current_time_ms clock = Clock.System.now_ms clock
let clear_watches client = Hashtbl.reset client.watched
let clear_transaction client = client.tx <- None
let transaction_error client message = send_reply client (Resp.Error message)
let subscription_count client = Hashtbl.length client.subscriptions
let client_is_subscribed client = subscription_count client > 0
let noauth_message = "Authentication required."
let max_password_length = 1024
let wrongpass_message = "invalid username-password pair or user is disabled."
let auth_is_configured requirepass = Option.is_some requirepass

let auth_exempt_command = function
  | Command.Auth _ | Ping _ -> true
  | _ -> false

let auth_rejected ~source ~requirepass client command =
  match source with
  | Master_stream -> false
  | External_client ->
      auth_is_configured requirepass
      && (not client.authenticated)
      && not (auth_exempt_command command)

let acl_getuser_reply ~requirepass =
  let flags, passwords =
    match requirepass with
    | None ->
        ([ Resp.Bulk_string (Some "on"); Resp.Bulk_string (Some "nopass") ], [])
    | Some password ->
        ( [ Resp.Bulk_string (Some "on") ],
          [ Resp.Bulk_string (Some (Sha256.hex password)) ] )
  in
  Resp.Array
    [
      Resp.Bulk_string (Some "flags");
      Resp.Array flags;
      Resp.Bulk_string (Some "passwords");
      Resp.Array passwords;
    ]

let constant_time_equal left right =
  if String.length left <> String.length right then false
  else
    let difference = ref 0 in
    for index = 0 to String.length left - 1 do
      difference :=
        !difference lor (Char.code left.[index] lxor Char.code right.[index])
    done;
    !difference = 0

let password_matches ~expected provided =
  (* Comparing fixed-width digests keeps both the password contents and its
     length out of the comparison's control flow. *)
  constant_time_equal (Sha256.hex expected) (Sha256.hex provided)

let handle_auth_command ~requirepass client password =
  match requirepass with
  | None ->
      send_reply client
        (Resp.Error
           "AUTH <password> called without any password configured for the \
            default user")
  | Some expected_password ->
      if
        String.length password <= max_password_length
        && password_matches ~expected:expected_password password
      then (
        client.authenticated <- true;
        send_reply client (Resp.Simple_string "OK"))
      else
        send_reply client
          (Resp.Prefixed_error
             { prefix = "WRONGPASS"; message = wrongpass_message })

let handle_acl_command ~requirepass client = function
  | Command.AclWhoami -> send_reply client (Resp.Bulk_string (Some "default"))
  | Command.AclGetuser user ->
      if user = "default" then
        send_reply client (acl_getuser_reply ~requirepass)
      else send_reply client (Resp.Bulk_string None)
  | _ -> assert false

let mark_transaction_dirty client message =
  (match client.tx with Some tx -> tx.dirty <- true | None -> ());
  transaction_error client message

let queueable_in_transaction = function
  | Command.Multi | Exec | Discard | Watch _ | Unwatch -> false
  | Blpop _ | Xread _ | Wait _ -> false
  | Replconf _ | Psync _ -> false
  | Auth _ | AclWhoami | AclGetuser _ -> false
  | Subscribe _ | Unsubscribe _ -> false
  | _ -> true

let generate_replid () =
  let hex = "0123456789abcdef" in
  String.init 40 (fun _ -> hex.[Random.int (String.length hex)])

let read_frame_blocking fd buffer_ref =
  let rec loop () =
    match Resp.parse_frame !buffer_ref with
    | Some (Ok (frame, consumed)) ->
        buffer_ref :=
          String.sub !buffer_ref consumed (String.length !buffer_ref - consumed);
        frame
    | Some (Error message) -> failwith message
    | None -> (
        let chunk = Bytes.create 4096 in
        match Unix.read fd chunk 0 (Bytes.length chunk) with
        | 0 -> failwith "unexpected EOF while reading replication handshake"
        | read ->
            buffer_ref := !buffer_ref ^ Bytes.sub_string chunk 0 read;
            if String.length !buffer_ref > max_client_buffer then
              failwith "replication handshake response exceeds buffer limit";
            loop ())
  in
  loop ()

let find_crlf input ~start =
  let rec loop index =
    if index + 1 >= String.length input then None
    else if input.[index] = '\r' && input.[index + 1] = '\n' then Some index
    else loop (index + 1)
  in
  loop start

let read_rdb_payload_blocking fd buffer_ref =
  let rec read_more () =
    let chunk = Bytes.create 4096 in
    match Unix.read fd chunk 0 (Bytes.length chunk) with
    | 0 -> failwith "unexpected EOF while reading full-resync RDB payload"
    | read -> buffer_ref := !buffer_ref ^ Bytes.sub_string chunk 0 read
  in
  let rec read_length_line () =
    match find_crlf !buffer_ref ~start:0 with
    | Some stop ->
        let line = String.sub !buffer_ref 0 stop in
        buffer_ref :=
          String.sub !buffer_ref (stop + 2)
            (String.length !buffer_ref - stop - 2);
        line
    | None ->
        if String.length !buffer_ref > 64 then
          failwith "invalid RDB length line after FULLRESYNC";
        read_more ();
        read_length_line ()
  in
  let length_line = read_length_line () in
  if String.length length_line = 0 || length_line.[0] <> '$' then
    failwith "expected RDB bulk length after FULLRESYNC";
  let payload_length =
    match
      Integer.to_int_opt
        (String.sub length_line 1 (String.length length_line - 1))
    with
    | Some length when length >= 0 && length <= max_rdb_payload -> length
    | Some length when length > max_rdb_payload ->
        failwith "RDB payload after FULLRESYNC exceeds the supported limit"
    | _ -> failwith "invalid RDB bulk length after FULLRESYNC"
  in
  let rec ensure_payload () =
    if String.length !buffer_ref < payload_length then (
      read_more ();
      ensure_payload ())
  in
  ensure_payload ();
  let payload = String.sub !buffer_ref 0 payload_length in
  buffer_ref :=
    String.sub !buffer_ref payload_length
      (String.length !buffer_ref - payload_length);
  if
    String.length !buffer_ref >= 2
    && !buffer_ref.[0] = '\r'
    && !buffer_ref.[1] = '\n'
  then buffer_ref := String.sub !buffer_ref 2 (String.length !buffer_ref - 2);
  payload

let resolve_master_addresses host port =
  try
    Unix.getaddrinfo host (string_of_int port)
      [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
    |> List.map (fun address -> address.Unix.ai_addr)
  with Unix.Unix_error _ | Not_found -> []

let connect_to_master ~host ~port ~listening_port ~masterauth ~max_output_buffer
    ~replid ~offset =
  let addresses = resolve_master_addresses host port in
  if addresses = [] then
    failwith
      (Printf.sprintf "could not resolve replica upstream %s:%d" host port);
  let deadline = Unix.gettimeofday () +. 5.0 in
  let try_addresses addresses =
    let rec loop = function
      | [] -> None
      | address :: rest -> (
          let fd =
            Unix.socket (Unix.domain_of_sockaddr address) Unix.SOCK_STREAM 0
          in
          match Unix.connect fd address with
          | () -> Some fd
          | exception Unix.Unix_error _ ->
              Unix.close fd;
              loop rest)
    in
    loop addresses
  in
  let rec connect_with_retry () =
    match try_addresses addresses with
    | Some fd -> fd
    | None ->
        if Unix.gettimeofday () >= deadline then
          failwith "replica could not connect to master in time"
        else (
          Unix.sleepf 0.05;
          connect_with_retry ())
  in
  let fd = connect_with_retry () in
  let send_command arguments = write_all fd (Resp.encode_command arguments) in
  let handshake_buffer = ref "" in
  let expect_simple expected =
    match read_frame_blocking fd handshake_buffer with
    | Resp.Simple_string_frame value when value = expected -> ()
    | Resp.Simple_string_frame value ->
        failwith (Printf.sprintf "unexpected handshake reply: %s" value)
    | _ -> failwith "unexpected non-simple-string handshake reply"
  in
  let make_upstream replid offset =
    Unix.set_nonblock fd;
    {
      host;
      port;
      fd;
      buffer = !handshake_buffer;
      replid;
      offset;
      link_status = "up";
      output = Buffer.create 256;
      max_output_buffer;
      tx_queue = None;
    }
  in
  send_command [ "PING" ];
  expect_simple "PONG";
  Option.iter
    (fun password ->
      send_command [ "AUTH"; password ];
      expect_simple "OK")
    masterauth;
  send_command [ "REPLCONF"; "listening-port"; string_of_int listening_port ];
  expect_simple "OK";
  send_command [ "REPLCONF"; "capa"; "psync2" ];
  expect_simple "OK";
  send_command [ "PSYNC"; replid; Int64.to_string offset ];
  match read_frame_blocking fd handshake_buffer with
  | Resp.Simple_string_frame line -> (
      match String.split_on_char ' ' line with
      | [ "FULLRESYNC"; new_replid; offset_text ] ->
          let new_offset =
            match Int64.of_string_opt offset_text with
            | Some offset -> offset
            | None -> failwith "invalid FULLRESYNC offset"
          in
          let payload = read_rdb_payload_blocking fd handshake_buffer in
          let entries =
            if String.length payload = 0 then []
            else
              match Rdb.parse payload with
              | Ok entries -> entries
              | Error message ->
                  failwith
                    (Printf.sprintf
                       "failed to import full-resync RDB payload: %s" message)
          in
          Full_resync (make_upstream new_replid new_offset, entries)
      | [ "CONTINUE" ] -> Partial_resync (make_upstream replid offset)
      | [ "CONTINUE"; new_replid ] ->
          Partial_resync (make_upstream new_replid offset)
      | _ -> failwith (Printf.sprintf "unexpected PSYNC reply: %s" line))
  | _ -> failwith "expected FULLRESYNC or CONTINUE"

let run ~port ~replicaof ~rdb_file ~requirepass ~masterauth ~max_clients
    ~max_output_buffer ~maxmemory ~tls_certificate ~tls_key =
  Random.self_init ();
  Mirage_crypto_rng_unix.use_default ();
  if max_clients <= 0 then failwith "--maxclients must be greater than zero";
  if max_output_buffer <= 0 then
    failwith "--max-output-buffer must be greater than zero";
  if Int64.compare maxmemory 0L < 0 then
    failwith "--maxmemory must be zero or greater";
  let tls_server =
    match (tls_certificate, tls_key) with
    | None, None -> None
    | Some certificate_file, Some key_file -> (
        match Tls_transport.server ~certificate_file ~key_file with
        | Ok server -> Some server
        | Error message -> failwith ("TLS configuration failed: " ^ message))
    | _ -> failwith "--tls-cert and --tls-key must be provided together"
  in
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let validate_password option_name = function
    | Some password when String.length password > max_password_length ->
        failwith
          (Printf.sprintf "%s is longer than the %d-byte limit" option_name
             max_password_length)
    | Some _ | None -> ()
  in
  validate_password "--requirepass" requirepass;
  validate_password "--masterauth" masterauth;
  let stop = ref false in
  let () =
    Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> stop := true));
    Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> stop := true))
  in
  let clock = Clock.System.create () in
  let engine = Engine.create () in
  let () =
    match rdb_file with
    | None -> ()
    | Some path -> (
        match Rdb.load_file path with
        | Ok entries ->
            List.iter
              (fun entry ->
                Engine.load_string engine ~key:entry.Rdb.key ~value:entry.value
                  ~expires_at_ms:entry.expires_at_ms)
              entries
        | Error message ->
            failwith
              (Printf.sprintf "failed to load RDB file %S: %s" path message))
  in
  let server_fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server_fd Unix.SO_REUSEADDR true;
  Unix.bind server_fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  Unix.listen server_fd 64;
  let is_replica = Option.is_some replicaof in
  let master_replid = generate_replid () in
  let master_offset = ref 0L in
  let master_backlog = Buffer.create 65536 in
  let master_backlog_start = ref 0L in
  let master_backlog_limit = 4 * 1024 * 1024 in
  let upstream : replica_upstream option ref = ref None in
  let apply_full_resync entries =
    Engine.clear engine;
    List.iter
      (fun entry ->
        Engine.load_string engine ~key:entry.Rdb.key ~value:entry.value
          ~expires_at_ms:entry.expires_at_ms)
      entries
  in
  let connect_once () =
    match replicaof with
    | None -> ()
    | Some (host, master_port) -> (
        let old = !upstream in
        let replid, offset =
          match old with
          | Some current -> (current.replid, current.offset)
          | None -> ("?", -1L)
        in
        (match old with
        | Some current -> (
            try Unix.close current.fd with Unix.Unix_error _ -> ())
        | None -> ());
        upstream := None;
        match
          connect_to_master ~host ~port:master_port ~listening_port:port
            ~masterauth ~max_output_buffer ~replid ~offset
        with
        | Full_resync (new_upstream, entries) ->
            apply_full_resync entries;
            upstream := Some new_upstream
        | Partial_resync new_upstream -> upstream := Some new_upstream)
  in
  let next_reconnect_at = ref 0.0 in
  let reconnect_if_needed () =
    if
      is_replica
      &&
      match !upstream with
      | Some current -> current.link_status <> "up"
      | None -> true
    then
      let now = Unix.gettimeofday () in
      if now >= !next_reconnect_at then (
        next_reconnect_at := now +. 1.0;
        try connect_once ()
        with Failure message ->
          prerr_endline ("ocaml-redis: replica reconnect failed: " ^ message))
  in
  (try connect_once ()
   with Failure message ->
     prerr_endline ("ocaml-redis: initial replica sync failed: " ^ message);
     next_reconnect_at := Unix.gettimeofday () +. 1.0);
  let clients : (Unix.file_descr, client) Hashtbl.t = Hashtbl.create 16 in
  let channel_subscribers : (string, client list) Hashtbl.t =
    Hashtbl.create 16
  in
  let list_waiters : (string, client Queue.t) Hashtbl.t = Hashtbl.create 16 in
  let stream_waiters : (string, client Queue.t) Hashtbl.t = Hashtbl.create 16 in
  let add_channel_subscription channel client =
    if not (Hashtbl.mem client.subscriptions channel) then (
      let subscribers =
        Hashtbl.find_opt channel_subscribers channel |> Option.value ~default:[]
      in
      Hashtbl.replace client.subscriptions channel ();
      client.subscription_order_rev <- channel :: client.subscription_order_rev;
      Hashtbl.replace channel_subscribers channel (client :: subscribers))
  in
  let remove_channel_subscription channel client =
    Hashtbl.remove client.subscriptions channel;
    client.subscription_order_rev <-
      List.filter
        (fun subscribed -> subscribed <> channel)
        client.subscription_order_rev;
    match Hashtbl.find_opt channel_subscribers channel with
    | None -> ()
    | Some subscribers ->
        let filtered =
          List.filter
            (fun (subscriber : client) ->
              subscriber.fd <> client.fd && not subscriber.closed)
            subscribers
        in
        if filtered = [] then Hashtbl.remove channel_subscribers channel
        else Hashtbl.replace channel_subscribers channel filtered
  in
  let send_subscribe_ack client channel =
    send_reply client
      (Resp.Array
         [
           Resp.Bulk_string (Some "subscribe");
           Resp.Bulk_string (Some channel);
           Resp.Integer (Int64.of_int (subscription_count client));
         ])
  in
  let send_unsubscribe_ack client channel_opt =
    send_reply client
      (Resp.Array
         [
           Resp.Bulk_string (Some "unsubscribe");
           Resp.Bulk_string channel_opt;
           Resp.Integer (Int64.of_int (subscription_count client));
         ])
  in
  let send_pubsub_ping client message =
    send_reply client
      (Resp.Array
         [
           Resp.Bulk_string (Some "pong");
           Resp.Bulk_string (Some (Option.value ~default:"" message));
         ])
  in
  let subscribed_channels client = List.rev client.subscription_order_rev in
  let channels_to_unsubscribe client channels =
    if channels = [] then subscribed_channels client else channels
  in
  let handle_subscribe client channels =
    List.iter
      (fun channel ->
        add_channel_subscription channel client;
        send_subscribe_ack client channel)
      channels
  in
  let handle_unsubscribe client channels =
    let channels = channels_to_unsubscribe client channels in
    if channels = [] then send_unsubscribe_ack client None
    else
      List.iter
        (fun channel ->
          remove_channel_subscription channel client;
          send_unsubscribe_ack client (Some channel))
        channels
  in
  let publish_message channel message =
    match Hashtbl.find_opt channel_subscribers channel with
    | None -> 0
    | Some subscribers ->
        let active =
          List.filter
            (fun (subscriber : client) ->
              (not subscriber.closed)
              && Hashtbl.mem subscriber.subscriptions channel)
            subscribers
        in
        if active = [] then Hashtbl.remove channel_subscribers channel
        else Hashtbl.replace channel_subscribers channel active;
        List.iter
          (fun subscriber ->
            send_reply subscriber
              (Resp.Array
                 [
                   Resp.Bulk_string (Some "message");
                   Resp.Bulk_string (Some channel);
                   Resp.Bulk_string (Some message);
                 ]))
          active;
        List.length active
  in
  let enqueue_waiter table key client =
    let queue =
      match Hashtbl.find_opt table key with
      | Some queue -> queue
      | None ->
          let queue = Queue.create () in
          Hashtbl.add table key queue;
          queue
    in
    Queue.push client queue
  in
  let info_replication () =
    let payload =
      if not is_replica then
        let connected_slaves =
          Hashtbl.to_seq_values clients
          |> Seq.filter (fun client -> client.replica_peer && not client.closed)
          |> Seq.length
        in
        String.concat "\r\n"
          [
            "role:master";
            Printf.sprintf "connected_slaves:%d" connected_slaves;
            Printf.sprintf "master_replid:%s" master_replid;
            Printf.sprintf "master_repl_offset:%Ld" !master_offset;
            "";
          ]
      else
        let host, port =
          match replicaof with
          | Some (host, port) -> (host, port)
          | None -> ("", 0)
        in
        match !upstream with
        | Some upstream ->
            String.concat "\r\n"
              [
                "role:slave";
                Printf.sprintf "master_host:%s" upstream.host;
                Printf.sprintf "master_port:%d" upstream.port;
                Printf.sprintf "master_link_status:%s" upstream.link_status;
                Printf.sprintf "master_replid:%s" upstream.replid;
                Printf.sprintf "master_repl_offset:%Ld" upstream.offset;
                "";
              ]
        | None ->
            String.concat "\r\n"
              [
                "role:slave";
                Printf.sprintf "master_host:%s" host;
                Printf.sprintf "master_port:%d" port;
                "master_link_status:down";
                "master_replid:?";
                "master_repl_offset:0";
                "";
              ]
    in
    Resp.Bulk_string (Some payload)
  in
  let blocked_client_count () =
    Hashtbl.to_seq_values clients
    |> Seq.filter (fun client -> Option.is_some client.blocked)
    |> Seq.length
  in
  let info_memory () =
    let payload =
      String.concat "\r\n"
        [
          Printf.sprintf "used_memory:%Ld" (Engine.memory_usage engine);
          Printf.sprintf "maxmemory:%Ld" maxmemory;
          "maxmemory_policy:noeviction";
          Printf.sprintf "connected_clients:%d" (Hashtbl.length clients);
          Printf.sprintf "blocked_clients:%d" (blocked_client_count ());
          "";
        ]
    in
    Resp.Bulk_string (Some payload)
  in
  let memory_limit_reached () =
    Int64.compare maxmemory 0L > 0
    && Int64.compare (Engine.memory_usage engine) maxmemory >= 0
  in
  let oom_message = "OOM command not allowed when used memory > 'maxmemory'." in
  let propagate_raw payload =
    master_offset :=
      Int64.add !master_offset (Int64.of_int (String.length payload));
    Buffer.add_string master_backlog payload;
    let excess = Buffer.length master_backlog - master_backlog_limit in
    if excess > 0 then (
      let data = Buffer.contents master_backlog in
      Buffer.clear master_backlog;
      Buffer.add_substring master_backlog data excess
        (String.length data - excess);
      master_backlog_start :=
        Int64.add !master_backlog_start (Int64.of_int excess));
    Hashtbl.iter
      (fun _ client ->
        if client.replica_peer && not client.closed then send_raw client payload)
      clients;
    !master_offset
  in
  let propagate_command command =
    match Command.to_arguments command with
    | Some arguments -> Some (propagate_raw (Resp.encode_command arguments))
    | None -> None
  in
  let resume_client = ref (fun (_ : client) -> ()) in
  let wake_list_waiters_for_key key =
    let woken = ref [] in
    let rec serve () =
      match Hashtbl.find_opt list_waiters key with
      | None -> ()
      | Some queue when Queue.is_empty queue -> Hashtbl.remove list_waiters key
      | Some queue -> (
          let rec next_active_waiter () =
            if Queue.is_empty queue then None
            else
              let client = Queue.pop queue in
              match client.blocked with
              | Some (Blocked_list_pop blocked)
                when blocked.key = key && not client.closed ->
                  Some client
              | _ -> next_active_waiter ()
          in
          match next_active_waiter () with
          | None -> Hashtbl.remove list_waiters key
          | Some client -> (
              match
                Engine.wake_blpop ~now_ms:(current_time_ms clock) engine key
              with
              | None -> enqueue_waiter list_waiters key client
              | Some (key, value) ->
                  if not is_replica then
                    client.last_write_offset <-
                      propagate_raw (Resp.encode_command [ "LPOP"; key ]);
                  client.blocked <- None;
                  send_reply client
                    (Resp.Array
                       [
                         Resp.Bulk_string (Some key);
                         Resp.Bulk_string (Some value);
                       ]);
                  woken := client :: !woken;
                  serve ()))
    in
    serve ();
    List.iter (fun client -> !resume_client client) (List.rev !woken)
  in
  let wake_stream_waiters_for_key key =
    match Hashtbl.find_opt stream_waiters key with
    | None -> ()
    | Some queue ->
        let attempts = Queue.length queue in
        let woken = ref [] in
        for _ = 1 to attempts do
          if not (Queue.is_empty queue) then
            let client = Queue.pop queue in
            if not client.closed then
              match client.blocked with
              | Some (Blocked_stream_read blocked)
                when List.mem key blocked.keys -> (
                  match
                    Engine.wake_xread ~now_ms:(current_time_ms clock) engine
                      ~queries:blocked.queries
                  with
                  | Some reply ->
                      client.blocked <- None;
                      send_reply client reply;
                      woken := client :: !woken
                  | None -> Queue.push client queue)
              | _ -> ()
        done;
        if Queue.is_empty queue then Hashtbl.remove stream_waiters key;
        List.iter (fun client -> !resume_client client) (List.rev !woken)
  in
  let wake_waiters_for_keys keys =
    let keys = List.sort_uniq String.compare keys in
    List.iter wake_list_waiters_for_key keys;
    List.iter wake_stream_waiters_for_key keys
  in
  let process_timeouts () =
    let now_ms = current_time_ms clock in
    let expired = ref [] in
    Hashtbl.iter
      (fun _ client ->
        (match client.handshake_deadline with
        | Some deadline when Int64.compare now_ms deadline >= 0 ->
            close_client client
        | Some _ | None -> ());
        match client.blocked with
        | Some (Blocked_list_pop { deadline_ms = Some deadline_ms; _ })
        | Some (Blocked_stream_read { deadline_ms = Some deadline_ms; _ })
          when Int64.compare now_ms deadline_ms >= 0 ->
            client.blocked <- None;
            send_reply client Resp.Null_array;
            expired := client :: !expired
        | Some
            (Blocked_wait
               { required_replicas = _; offset; deadline_ms = Some deadline_ms })
          when Int64.compare now_ms deadline_ms >= 0 ->
            let count =
              Hashtbl.to_seq_values clients
              |> Seq.filter (fun client ->
                  client.replica_peer && (not client.closed)
                  && Int64.compare client.replica_ack_offset offset >= 0)
              |> Seq.length
            in
            client.blocked <- None;
            send_reply client (Resp.Integer (Int64.of_int count));
            expired := client :: !expired
        | _ -> ())
      clients;
    List.iter (fun client -> !resume_client client) (List.rev !expired)
  in
  let count_replicas_at_offset offset =
    Hashtbl.to_seq_values clients
    |> Seq.filter (fun client ->
        client.replica_peer && (not client.closed)
        && Int64.compare client.replica_ack_offset offset >= 0)
    |> Seq.length
  in
  let wake_wait_clients () =
    let woken = ref [] in
    Hashtbl.iter
      (fun _ client ->
        match client.blocked with
        | Some (Blocked_wait { required_replicas; offset; _ }) ->
            let count = count_replicas_at_offset offset in
            if Int64.compare (Int64.of_int count) required_replicas >= 0 then (
              client.blocked <- None;
              send_reply client (Resp.Integer (Int64.of_int count));
              woken := client :: !woken)
        | _ -> ())
      clients;
    List.iter (fun client -> !resume_client client) (List.rev !woken)
  in
  let apply_engine_command ~now_ms command =
    Engine.handle ~now_ms engine command
  in
  let propagation_command ~now_ms command (outcome : Engine.outcome) =
    match (command, outcome.Engine.reply) with
    | Command.Blpop { key; _ }, Some _ -> Some (Command.Lpop key)
    | Command.Blpop _, None -> None
    | Command.Xadd { key; fields; _ }, Some (Resp.Bulk_string (Some id)) ->
        Some (Command.Xadd { key; id_spec = id; fields })
    | ( Command.Set
          { key; value; expiry = Some (Command.Relative_ms relative_ms) },
        _ ) ->
        Some
          (Command.Set
             {
               key;
               value;
               expiry =
                 Some
                   (Command.Absolute_ms
                      (Expiry.add_saturating now_ms relative_ms));
             })
    | _ -> if Command.is_write command then Some command else None
  in
  let write_failed (outcome : Engine.outcome) =
    match outcome.reply with
    | Some (Resp.Error _ | Prefixed_error _) -> true
    | Some _ | None -> false
  in
  let handle_immediate_command ~source client command =
    if Command.is_write command && memory_limit_reached () then
      send_reply client (Resp.Error oom_message)
    else
      match command with
      | Command.Wait { numreplicas; timeout_ms } ->
          let count = count_replicas_at_offset client.last_write_offset in
          if
            Int64.compare (Int64.of_int count) numreplicas >= 0
            || source = Master_stream
          then send_reply client (Resp.Integer (Int64.of_int count))
          else
            let now_ms = current_time_ms clock in
            let deadline_ms =
              Option.map (Expiry.add_saturating now_ms) timeout_ms
            in
            client.blocked <-
              Some
                (Blocked_wait
                   {
                     required_replicas = numreplicas;
                     offset = client.last_write_offset;
                     deadline_ms;
                   })
      | _ ->
          let now_ms = current_time_ms clock in
          let outcome = apply_engine_command ~now_ms command in
          (if
             source = External_client && (not is_replica)
             && not (write_failed outcome)
           then
             match propagation_command ~now_ms command outcome with
             | Some propagated -> (
                 match propagate_command propagated with
                 | Some offset -> client.last_write_offset <- offset
                 | None -> ())
             | None -> ());
          Option.iter (send_reply client) outcome.reply;
          Option.iter
            (function
              | Engine.List_pop { key; timeout_ms } ->
                  let deadline_ms =
                    Option.map (Expiry.add_saturating now_ms) timeout_ms
                  in
                  client.blocked <- Some (Blocked_list_pop { key; deadline_ms });
                  enqueue_waiter list_waiters key client
              | Engine.Stream_read { queries; timeout_ms } ->
                  let deadline_ms =
                    Option.map (Expiry.add_saturating now_ms) timeout_ms
                  in
                  let keys = List.map fst queries in
                  client.blocked <-
                    Some (Blocked_stream_read { queries; keys; deadline_ms });
                  List.iter
                    (fun key -> enqueue_waiter stream_waiters key client)
                    keys)
            outcome.block;
          wake_waiters_for_keys outcome.wake_keys;
          if source = External_client && not is_replica then
            wake_wait_clients ()
  in
  let watched_keys_dirty client =
    Hashtbl.to_seq client.watched
    |> Seq.exists (fun (key, version) ->
        Int64.compare
          (Engine.watch_version ~now_ms:(current_time_ms clock) engine key)
          version
        <> 0)
  in
  let exec_transaction ~source client tx =
    if tx.dirty then (
      clear_transaction client;
      clear_watches client;
      send_reply client
        (Resp.Prefixed_error
           {
             prefix = "EXECABORT";
             message = "Transaction discarded because of previous errors.";
           }))
    else if watched_keys_dirty client then (
      clear_transaction client;
      clear_watches client;
      send_reply client Resp.Null_array)
    else
      let commands = List.rev tx.queue_rev in
      clear_transaction client;
      clear_watches client;
      let now_ms = current_time_ms clock in
      let execute_queued command : Engine.outcome =
        match command with
        | Command.Publish { channel; message } ->
            {
              reply =
                Some
                  (Resp.Integer (Int64.of_int (publish_message channel message)));
              block = None;
              wake_keys = [];
            }
        | Command.Info None ->
            { reply = Some (info_replication ()); block = None; wake_keys = [] }
        | Command.Info (Some section) -> (
            match String.lowercase_ascii section with
            | "replication" ->
                {
                  reply = Some (info_replication ());
                  block = None;
                  wake_keys = [];
                }
            | "memory" ->
                { reply = Some (info_memory ()); block = None; wake_keys = [] }
            | _ ->
                {
                  reply = Some (Resp.Bulk_string (Some ""));
                  block = None;
                  wake_keys = [];
                })
        | _ when Command.is_write command && memory_limit_reached () ->
            {
              reply = Some (Resp.Error oom_message);
              block = None;
              wake_keys = [];
            }
        | _ -> apply_engine_command ~now_ms command
      in
      let outcomes =
        List.map (fun command -> (command, execute_queued command)) commands
      in
      let replies =
        List.map
          (fun (_, (outcome : Engine.outcome)) ->
            match outcome.reply with
            | Some reply -> reply
            | None ->
                Resp.Error "blocking commands are not supported inside MULTI")
          outcomes
      in
      send_reply client (Resp.Array replies);
      (if source = External_client && not is_replica then
         let propagated =
           List.filter_map
             (fun (command, outcome) ->
               if write_failed outcome then None
               else propagation_command ~now_ms command outcome)
             outcomes
         in
         if propagated <> [] then (
           ignore (propagate_raw (Resp.encode_command [ "MULTI" ]));
           List.iter
             (fun propagated ->
               match propagate_command propagated with
               | Some offset -> client.last_write_offset <- offset
               | None -> ())
             propagated;
           let offset = propagate_raw (Resp.encode_command [ "EXEC" ]) in
           client.last_write_offset <- offset));
      List.iter
        (fun (_, (outcome : Engine.outcome)) ->
          wake_waiters_for_keys outcome.wake_keys)
        outcomes;
      wake_wait_clients ()
  in
  let readonly_message = "You can't write against a read only replica." in
  let rec handle_command ~source client command =
    let readonly_rejected () =
      match source with
      | Master_stream -> false
      | External_client ->
          is_replica
          && (Command.is_write command || command = Multi || command = Exec
            || command = Discard)
    in
    if auth_rejected ~source ~requirepass client command then
      send_reply client
        (Resp.Prefixed_error { prefix = "NOAUTH"; message = noauth_message })
    else if client_is_subscribed client then
      match command with
      | Command.Subscribe channels -> handle_subscribe client channels
      | Unsubscribe channels -> handle_unsubscribe client channels
      | Ping message -> send_pubsub_ping client message
      | _ ->
          transaction_error client
            "only SUBSCRIBE, UNSUBSCRIBE, and PING are allowed in this context"
    else if readonly_rejected () then
      send_reply client
        (Resp.Prefixed_error { prefix = "READONLY"; message = readonly_message })
    else
      match (client.tx, command) with
      | None, Command.Auth password ->
          handle_auth_command ~requirepass client password
      | None, (Command.AclWhoami | Command.AclGetuser _) ->
          handle_acl_command ~requirepass client command
      | None, Info None -> send_reply client (info_replication ())
      | None, Info (Some section) -> (
          match String.lowercase_ascii section with
          | "replication" -> send_reply client (info_replication ())
          | "memory" -> send_reply client (info_memory ())
          | _ -> send_reply client (Resp.Bulk_string (Some "")))
      | None, Replconf (kind :: rest) -> (
          match String.uppercase_ascii kind with
          | "ACK" when client.replica_peer ->
              (match rest with
              | offset_value :: _ -> (
                  match Integer.of_string_opt offset_value with
                  | Some offset -> client.replica_ack_offset <- offset
                  | None -> ())
              | [] -> ());
              wake_wait_clients ()
          | "ACK" -> send_reply client (Resp.Simple_string "OK")
          | "GETACK" when source = Master_stream -> (
              match !upstream with
              | Some upstream ->
                  write_all upstream.fd
                    (Resp.encode_command
                       [ "REPLCONF"; "ACK"; Int64.to_string upstream.offset ])
              | None -> ())
          | _ -> send_reply client (Resp.Simple_string "OK"))
      | None, Psync _ when is_replica ->
          send_reply client
            (Resp.Error "PSYNC is not supported from replica role")
      | None, Psync { replid; offset } -> (
          let requested =
            match Integer.of_string_opt offset with
            | Some value when value >= 0L -> Some value
            | Some _ | None -> None
          in
          let can_continue =
            match requested with
            | Some value ->
                replid = master_replid
                && Int64.compare value !master_backlog_start >= 0
                && Int64.compare value !master_offset <= 0
            | None -> false
          in
          match (requested, can_continue) with
          | Some value, true ->
              send_raw client (Printf.sprintf "+CONTINUE %s\r\n" master_replid);
              let backlog = Buffer.contents master_backlog in
              let skip = Int64.to_int (Int64.sub value !master_backlog_start) in
              send_raw client
                (String.sub backlog skip (String.length backlog - skip));
              client.replica_peer <- true;
              client.replica_ack_offset <- value
          | _ ->
              send_raw client
                (Printf.sprintf "+FULLRESYNC %s %Ld\r\n$0\r\n\r\n" master_replid
                   !master_offset);
              client.replica_peer <- true;
              client.replica_ack_offset <- !master_offset)
      | Some _, Command.Multi ->
          mark_transaction_dirty client "MULTI calls can not be nested"
      | Some _, Command.Watch _ ->
          mark_transaction_dirty client "WATCH inside MULTI is not allowed"
      | Some _, Command.Unwatch ->
          mark_transaction_dirty client "UNWATCH inside MULTI is not allowed"
      | Some _, (Command.Blpop _ | Xread _) ->
          mark_transaction_dirty client
            "blocking commands are not allowed inside MULTI"
      | Some tx, Command.Exec -> exec_transaction ~source client tx
      | Some _, Command.Discard ->
          clear_transaction client;
          clear_watches client;
          send_reply client (Resp.Simple_string "OK")
      | Some tx, command ->
          if queueable_in_transaction command then (
            tx.queue_rev <- command :: tx.queue_rev;
            send_reply client (Resp.Simple_string "QUEUED"))
          else
            mark_transaction_dirty client "command is not allowed inside MULTI"
      | None, Command.Multi ->
          client.tx <- Some { queue_rev = []; dirty = false };
          send_reply client (Resp.Simple_string "OK")
      | None, Command.Exec -> transaction_error client "EXEC without MULTI"
      | None, Command.Discard ->
          transaction_error client "DISCARD without MULTI"
      | None, Command.Watch keys ->
          List.iter
            (fun key ->
              Hashtbl.replace client.watched key
                (Engine.watch_version ~now_ms:(current_time_ms clock) engine key))
            keys;
          send_reply client (Resp.Simple_string "OK")
      | None, Command.Unwatch ->
          clear_watches client;
          send_reply client (Resp.Simple_string "OK")
      | None, Command.Subscribe channels -> handle_subscribe client channels
      | None, Command.Unsubscribe channels -> handle_unsubscribe client channels
      | None, Command.Publish { channel; message } ->
          send_reply client
            (Resp.Integer (Int64.of_int (publish_message channel message)))
      | None, command -> handle_immediate_command ~source client command
  in
  let process_parsed_command ~source client arguments =
    match Command.parse arguments with
    | Ok command -> handle_command ~source client command
    | Error error ->
        if Option.is_some client.tx then
          mark_transaction_dirty client (Command.to_error_message error)
        else send_reply client (Resp.Error (Command.to_error_message error))
  in
  let rec process_buffer (client : client) =
    match Resp.parse_request client.buffer with
    | None -> ()
    | Some (Error message) ->
        send_reply client (Resp.Error message);
        close_client client
    | Some (Ok (arguments, consumed)) ->
        let remaining =
          String.sub client.buffer consumed
            (String.length client.buffer - consumed)
        in
        client.buffer <- remaining;
        process_parsed_command ~source:External_client client arguments;
        if (not client.closed) && Option.is_none client.blocked then
          process_buffer client
  in
  let () =
    resume_client :=
      fun client ->
        if (not client.closed) && Option.is_none client.blocked then
          process_buffer client
  in
  let flush_upstream (upstream : replica_upstream) =
    if upstream.link_status = "up" && Buffer.length upstream.output > 0 then (
      let data = Buffer.contents upstream.output in
      Buffer.clear upstream.output;
      let rec write offset =
        if offset < String.length data then
          match
            Unix.write_substring upstream.fd data offset
              (String.length data - offset)
          with
          | 0 -> upstream.link_status <- "down"
          | written -> write (offset + written)
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> write offset
          | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _)
            ->
              Buffer.add_substring upstream.output data offset
                (String.length data - offset)
          | exception Unix.Unix_error _ -> upstream.link_status <- "down"
          | exception End_of_file -> upstream.link_status <- "down"
      in
      write 0)
  in
  let queue_upstream (upstream : replica_upstream) data =
    if upstream.link_status = "up" then
      if
        String.length data > upstream.max_output_buffer
        || Buffer.length upstream.output + String.length data
           > upstream.max_output_buffer
      then upstream.link_status <- "down"
      else (
        Buffer.add_string upstream.output data;
        flush_upstream upstream)
  in
  let ack_upstream () =
    match !upstream with
    | None -> ()
    | Some upstream ->
        queue_upstream upstream
          (Resp.encode_command
             [ "REPLCONF"; "ACK"; Int64.to_string upstream.offset ])
  in
  let apply_replicated_write command =
    if Command.is_write command then (
      let now_ms = current_time_ms clock in
      let outcome = apply_engine_command ~now_ms command in
      (match outcome.reply with
      | Some (Resp.Error _ | Prefixed_error _) ->
          prerr_endline
            "ocaml-redis: warning: replicated write failed; replica state may \
             diverge"
      | Some _ | None -> ());
      outcome.wake_keys)
    else []
  in
  let replica_apply arguments =
    match Command.parse arguments with
    | Ok command -> (
        (match command with
        | Command.Replconf (kind :: _)
          when String.uppercase_ascii kind = "GETACK" ->
            ack_upstream ()
        | _ -> ());
        match !upstream with
        | None -> ()
        | Some upstream -> (
            match (upstream.tx_queue, command) with
            | Some _, Command.Multi -> ()
            | Some _, Command.Discard -> upstream.tx_queue <- None
            | Some commands, Command.Exec ->
                upstream.tx_queue <- None;
                let wake_keys =
                  List.concat_map apply_replicated_write (List.rev commands)
                in
                wake_waiters_for_keys wake_keys;
                ack_upstream ()
            | Some commands, _ -> upstream.tx_queue <- Some (command :: commands)
            | None, Command.Multi -> upstream.tx_queue <- Some []
            | None, (Command.Exec | Command.Discard) -> ()
            | None, _ ->
                let wake_keys = apply_replicated_write command in
                wake_waiters_for_keys wake_keys;
                if Command.is_write command then ack_upstream ()))
    | Error _ -> (
        match !upstream with
        | Some upstream -> upstream.link_status <- "down"
        | None -> ())
  in
  let process_upstream_buffer (upstream : replica_upstream) =
    let rec loop () =
      match Resp.parse_frame upstream.buffer with
      | None -> ()
      | Some (Error _) -> upstream.link_status <- "down"
      | Some (Ok (frame, consumed)) ->
          upstream.buffer <-
            String.sub upstream.buffer consumed
              (String.length upstream.buffer - consumed);
          (match frame with
          | Resp.Command_frame arguments ->
              upstream.offset <-
                Int64.add upstream.offset (Int64.of_int consumed);
              replica_apply arguments
          | _ -> ());
          loop ()
    in
    loop ()
  in
  let reject_over_capacity client_fd =
    let message = "-ERR max number of clients reached\r\n" in
    (try
       ignore (Unix.write_substring client_fd message 0 (String.length message))
     with Unix.Unix_error _ -> ());
    try Unix.close client_fd with Unix.Unix_error _ -> ()
  in
  let accept_client () =
    match Unix.accept server_fd with
    | client_fd, _ ->
        Unix.set_nonblock client_fd;
        if Hashtbl.length clients >= max_clients then
          reject_over_capacity client_fd
        else
          let client =
            {
              fd = client_fd;
              buffer = "";
              output = Buffer.create 256;
              blocked = None;
              tx = None;
              authenticated = Option.is_none requirepass;
              watched = Hashtbl.create 8;
              subscriptions = Hashtbl.create 8;
              subscription_order_rev = [];
              replica_peer = false;
              replica_ack_offset = 0L;
              last_write_offset = 0L;
              max_output_buffer;
              tls = Option.map Tls_transport.create tls_server;
              handshake_deadline =
                (match tls_server with
                | Some _ ->
                    Some
                      (Int64.add (current_time_ms clock)
                         tls_handshake_timeout_ms)
                | None -> None);
              closed = false;
            }
          in
          Hashtbl.replace clients client_fd client
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
    | exception Unix.Unix_error _ -> ()
  in
  let handle_client (client : client) =
    if client.closed then ()
    else
      let chunk = Bytes.create 4096 in
      match Unix.read client.fd chunk 0 (Bytes.length chunk) with
      | 0 -> close_client client
      | read -> (
          let data = Bytes.sub_string chunk 0 read in
          match client.tls with
          | None ->
              client.buffer <- client.buffer ^ data;
              if String.length client.buffer > max_client_buffer then (
                send_reply client
                  (Resp.Error "protocol error: request buffer limit exceeded");
                close_client client)
              else process_buffer client
          | Some tls -> (
              match Tls_transport.feed tls data with
              | Error (message, response) ->
                  queue_raw client response;
                  prerr_endline ("ocaml-redis: TLS handshake failed: " ^ message);
                  close_client client
              | Ok result ->
                  queue_raw client result.response;
                  client.buffer <- client.buffer ^ result.plaintext;
                  if not (Tls_transport.handshake_in_progress tls) then
                    client.handshake_deadline <- None;
                  if result.eof then close_client client
                  else if String.length client.buffer > max_client_buffer then (
                    send_reply client
                      (Resp.Error
                         "protocol error: request buffer limit exceeded");
                    close_client client)
                  else process_buffer client))
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
      | exception Unix.Unix_error _ -> close_client client
  in
  let handle_upstream upstream =
    let chunk = Bytes.create 4096 in
    match Unix.read upstream.fd chunk 0 (Bytes.length chunk) with
    | 0 -> upstream.link_status <- "down"
    | read ->
        upstream.buffer <- upstream.buffer ^ Bytes.sub_string chunk 0 read;
        if String.length upstream.buffer > max_client_buffer then
          upstream.link_status <- "down"
        else process_upstream_buffer upstream
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
    | exception Unix.Unix_error _ -> upstream.link_status <- "down"
  in
  let rec loop () =
    reconnect_if_needed ();
    (match !upstream with
    | Some upstream
      when upstream.link_status = "up" && String.length upstream.buffer > 0 ->
        process_upstream_buffer upstream
    | Some _ | None -> ());
    Hashtbl.filter_map_inplace
      (fun _ (client : client) -> if client.closed then None else Some client)
      clients;
    process_timeouts ();
    if not !stop then (
      let readable_fds = ref [] in
      let writable_fds = ref [] in
      Hashtbl.iter
        (fun _ (client : client) ->
          if not client.closed then (
            if Option.is_none client.blocked then
              readable_fds := client.fd :: !readable_fds;
            if Buffer.length client.output > 0 then
              writable_fds := client.fd :: !writable_fds))
        clients;
      let upstream_read_fd, upstream_write_fd =
        match !upstream with
        | Some upstream when upstream.link_status = "up" ->
            ( [ upstream.fd ],
              if Buffer.length upstream.output > 0 then [ upstream.fd ] else []
            )
        | _ -> ([], [])
      in
      let read_fds = (server_fd :: upstream_read_fd) @ !readable_fds in
      let write_fds = !writable_fds @ upstream_write_fd in
      let ready_read, ready_write =
        match Unix.select read_fds write_fds [] 0.1 with
        | ready_read, ready_write, _ -> (ready_read, ready_write)
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> ([], [])
      in
      List.iter
        (fun fd ->
          match !upstream with
          | Some upstream when fd = upstream.fd -> flush_upstream upstream
          | _ -> (
              match Hashtbl.find_opt clients fd with
              | Some client when not client.closed -> flush_client client
              | Some _ | None -> ()))
        ready_write;
      List.iter
        (fun fd ->
          if fd = server_fd then accept_client ()
          else
            match !upstream with
            | Some upstream when fd = upstream.fd -> handle_upstream upstream
            | _ -> (
                match Hashtbl.find_opt clients fd with
                | Some client when not client.closed -> handle_client client
                | Some _ | None -> ()))
        ready_read;
      loop ())
  in
  Fun.protect
    ~finally:(fun () ->
      Hashtbl.iter (fun _ client -> close_client client) clients;
      Option.iter (fun upstream -> Unix.close upstream.fd) !upstream;
      Unix.close server_fd)
    loop
