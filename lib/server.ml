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
  mutable blocked : blocked_state option;
  mutable tx : tx_state option;
  mutable authenticated : bool;
  watched : (string, int64) Hashtbl.t;
  subscriptions : (string, unit) Hashtbl.t;
  mutable subscription_order_rev : string list;
  mutable replica_peer : bool;
  mutable replica_ack_offset : int64;
  mutable last_write_offset : int64;
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
}

type source = External_client | Master_stream

let write_all fd data =
  let rec loop offset =
    if offset < String.length data then
      let written =
        Unix.write_substring fd data offset (String.length data - offset)
      in
      if written = 0 then raise End_of_file else loop (offset + written)
  in
  loop 0

let send_raw client data =
  if not client.closed then
    match write_all client.fd data with
    | () -> ()
    | exception Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) ->
        client.closed <- true

let send_reply client reply =
  if not client.closed then send_raw client (Resp.encode_reply reply)

let close_client client =
  if not client.closed then (
    client.closed <- true;
    Unix.close client.fd)

let current_time_ms clock = Clock.System.now_ms clock
let clear_watches client = Hashtbl.reset client.watched
let clear_transaction client = client.tx <- None
let transaction_error client message = send_reply client (Resp.Error message)
let subscription_count client = Hashtbl.length client.subscriptions
let client_is_subscribed client = subscription_count client > 0
let noauth_message = "Authentication required."
let wrongpass_message = "invalid username-password pair or user is disabled."
let auth_is_configured requirepass = Option.is_some requirepass

let auth_exempt_command = function
  | Command.Auth _ | Ping _ | Replconf _ | Psync _ -> true
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
        ([ Resp.Bulk_string (Some "on") ], [ Resp.Bulk_string (Some password) ])
  in
  Resp.Array
    [
      Resp.Bulk_string (Some "flags");
      Resp.Array flags;
      Resp.Bulk_string (Some "passwords");
      Resp.Array passwords;
    ]

let handle_auth_command ~requirepass client password =
  match requirepass with
  | None ->
      send_reply client
        (Resp.Error
           "AUTH <password> called without any password configured for the \
            default user")
  | Some expected_password ->
      if password = expected_password then (
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
  | Info _ | Replconf _ | Psync _ -> false
  | _ -> true

let command_is_write = function
  | Command.Set _ | Rpush _ | Lpush _ | Lrem _ | Blpop _ | Incr _ | Zadd _
  | Zrem _ | Geoadd _ | Xadd _ ->
      true
  | _ -> false

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
            loop ())
  in
  loop ()

let connect_to_master ~host ~port ~listening_port =
  let inet = Unix.inet_addr_of_string host in
  let deadline = Unix.gettimeofday () +. 5.0 in
  let rec connect_with_retry () =
    let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    match Unix.connect fd (Unix.ADDR_INET (inet, port)) with
    | () -> fd
    | exception Unix.Unix_error _ ->
        Unix.close fd;
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
  send_command [ "PING" ];
  expect_simple "PONG";
  send_command [ "REPLCONF"; "listening-port"; string_of_int listening_port ];
  expect_simple "OK";
  send_command [ "REPLCONF"; "capa"; "psync2" ];
  expect_simple "OK";
  send_command [ "PSYNC"; "?"; "-1" ];
  let replid, offset =
    match read_frame_blocking fd handshake_buffer with
    | Resp.Simple_string_frame line -> (
        match String.split_on_char ' ' line with
        | [ "FULLRESYNC"; replid; offset ] ->
            ( replid,
              match Int64.of_string_opt offset with
              | Some offset -> offset
              | None -> failwith "invalid FULLRESYNC offset" )
        | _ -> failwith "unexpected FULLRESYNC line")
    | _ -> failwith "expected FULLRESYNC simple string"
  in
  (match read_frame_blocking fd handshake_buffer with
  | Resp.Bulk_string_frame _ -> ()
  | _ -> failwith "expected RDB bulk payload after FULLRESYNC");
  { host; port; fd; buffer = ""; replid; offset; link_status = "up" }

let run ~port ~replicaof ~rdb_file ~requirepass =
  Random.self_init ();
  if Option.is_some requirepass && Option.is_some replicaof then
    failwith "--requirepass cannot be combined with --replicaof in this subset";
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
  let master_replid = generate_replid () in
  let master_offset = ref 0L in
  let upstream =
    match replicaof with
    | None -> None
    | Some (host, master_port) ->
        Some (connect_to_master ~host ~port:master_port ~listening_port:port)
  in
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
  let subscribed_channels client = client.subscription_order_rev in
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
      match upstream with
      | None ->
          let connected_slaves =
            Hashtbl.to_seq_values clients
            |> Seq.filter (fun client ->
                client.replica_peer && not client.closed)
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
    in
    Resp.Bulk_string (Some payload)
  in
  let propagate_raw payload =
    master_offset :=
      Int64.add !master_offset (Int64.of_int (String.length payload));
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
  let rec wake_list_waiters_for_key key =
    match Hashtbl.find_opt list_waiters key with
    | None -> ()
    | Some queue when Queue.is_empty queue -> ()
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
        | None -> ()
        | Some client -> (
            match
              Engine.wake_blpop ~now_ms:(current_time_ms clock) engine key
            with
            | None -> enqueue_waiter list_waiters key client
            | Some (key, value) ->
                client.blocked <- None;
                send_reply client
                  (Resp.Array
                     [
                       Resp.Bulk_string (Some key);
                       Resp.Bulk_string (Some value);
                     ]);
                wake_list_waiters_for_key key))
  in
  let rec wake_stream_waiters_for_key key =
    match Hashtbl.find_opt stream_waiters key with
    | None -> ()
    | Some queue when Queue.is_empty queue -> ()
    | Some queue -> (
        let rec next_active_waiter () =
          if Queue.is_empty queue then None
          else
            let client = Queue.pop queue in
            match client.blocked with
            | Some (Blocked_stream_read blocked)
              when List.mem key blocked.keys && not client.closed ->
                Some client
            | _ -> next_active_waiter ()
        in
        match next_active_waiter () with
        | None -> ()
        | Some client -> (
            match client.blocked with
            | Some (Blocked_stream_read blocked) -> (
                match
                  Engine.wake_xread ~now_ms:(current_time_ms clock) engine
                    ~queries:blocked.queries
                with
                | Some reply ->
                    client.blocked <- None;
                    send_reply client reply
                | None -> enqueue_waiter stream_waiters key client)
            | _ -> ()))
  in
  let wake_waiters_for_keys keys =
    let keys = List.sort_uniq String.compare keys in
    List.iter wake_list_waiters_for_key keys;
    List.iter wake_stream_waiters_for_key keys
  in
  let process_timeouts () =
    let now_ms = current_time_ms clock in
    Hashtbl.iter
      (fun _ client ->
        match client.blocked with
        | Some (Blocked_list_pop { deadline_ms = Some deadline_ms; _ })
        | Some (Blocked_stream_read { deadline_ms = Some deadline_ms; _ })
          when Int64.compare now_ms deadline_ms >= 0 ->
            client.blocked <- None;
            send_reply client Resp.Null_array
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
            send_reply client (Resp.Integer (Int64.of_int count))
        | _ -> ())
      clients
  in
  let count_replicas_at_offset offset =
    Hashtbl.to_seq_values clients
    |> Seq.filter (fun client ->
        client.replica_peer && (not client.closed)
        && Int64.compare client.replica_ack_offset offset >= 0)
    |> Seq.length
  in
  let wake_wait_clients () =
    Hashtbl.iter
      (fun _ client ->
        match client.blocked with
        | Some (Blocked_wait { required_replicas; offset; _ }) ->
            let count = count_replicas_at_offset offset in
            if count >= Int64.to_int required_replicas then (
              client.blocked <- None;
              send_reply client (Resp.Integer (Int64.of_int count)))
        | _ -> ())
      clients
  in
  let apply_engine_command command =
    let outcome =
      Engine.handle ~now_ms:(current_time_ms clock) engine command
    in
    wake_waiters_for_keys outcome.wake_keys;
    outcome
  in
  let handle_immediate_command ~source client command =
    match command with
    | Command.Wait { numreplicas; timeout_ms } ->
        let count = count_replicas_at_offset client.last_write_offset in
        if count >= Int64.to_int numreplicas || source = Master_stream then
          send_reply client (Resp.Integer (Int64.of_int count))
        else
          let deadline_ms =
            Option.map (Int64.add (current_time_ms clock)) timeout_ms
          in
          client.blocked <-
            Some
              (Blocked_wait
                 {
                   required_replicas = numreplicas;
                   offset = client.last_write_offset;
                   deadline_ms;
                 })
    | _ -> (
        let outcome = apply_engine_command command in
        Option.iter (send_reply client) outcome.reply;
        Option.iter
          (function
            | Engine.List_pop { key; timeout_ms } ->
                let deadline_ms =
                  Option.map (Int64.add (current_time_ms clock)) timeout_ms
                in
                client.blocked <- Some (Blocked_list_pop { key; deadline_ms });
                enqueue_waiter list_waiters key client
            | Engine.Stream_read { queries; timeout_ms } ->
                let deadline_ms =
                  Option.map (Int64.add (current_time_ms clock)) timeout_ms
                in
                let keys = List.map fst queries in
                client.blocked <-
                  Some (Blocked_stream_read { queries; keys; deadline_ms });
                List.iter
                  (fun key -> enqueue_waiter stream_waiters key client)
                  keys)
          outcome.block;
        if
          source = External_client && upstream = None
          && Command.is_write command
        then
          match propagate_command command with
          | Some offset -> client.last_write_offset <- offset
          | None ->
              ();
              wake_wait_clients ())
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
      let replies_rev =
        List.fold_left
          (fun replies_rev command ->
            let outcome = apply_engine_command command in
            let reply =
              match outcome.reply with
              | Some reply -> reply
              | None ->
                  Resp.Error "blocking commands are not supported inside MULTI"
            in
            reply :: replies_rev)
          [] commands
      in
      send_reply client (Resp.Array (List.rev replies_rev));
      if source = External_client && upstream = None then
        List.iter
          (fun command ->
            if Command.is_write command then
              match propagate_command command with
              | Some offset -> client.last_write_offset <- offset
              | None -> ())
          commands;
      wake_wait_clients ()
  in
  let readonly_message = "You can't write against a read only replica." in
  let rec handle_command ~source client command =
    let readonly_rejected () =
      match source with
      | Master_stream -> false
      | External_client ->
          upstream <> None
          && (command_is_write command || command = Multi || command = Exec
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
      | _, Command.Auth password ->
          handle_auth_command ~requirepass client password
      | _, (Command.AclWhoami | Command.AclGetuser _) ->
          handle_acl_command ~requirepass client command
      | _, Info None | _, Info (Some "replication") ->
          send_reply client (info_replication ())
      | _, Info _ -> send_reply client (Resp.Bulk_string (Some ""))
      | _, Replconf [ "ACK"; offset ] when client.replica_peer ->
          (match Int64.of_string_opt offset with
          | Some offset -> client.replica_ack_offset <- offset
          | None -> ());
          wake_wait_clients ()
      | _, Replconf [ "GETACK"; "*" ] when source = Master_stream -> (
          match upstream with
          | Some upstream ->
              write_all upstream.fd
                (Resp.encode_command
                   [ "REPLCONF"; "ACK"; Int64.to_string upstream.offset ])
          | None -> ())
      | _, Replconf _ -> send_reply client (Resp.Simple_string "OK")
      | _, Psync _ when upstream <> None ->
          send_reply client
            (Resp.Error "PSYNC is not supported from replica role")
      | _, Psync _ ->
          send_raw client
            (Printf.sprintf "+FULLRESYNC %s %Ld\r\n$0\r\n\r\n" master_replid
               !master_offset);
          client.replica_peer <- true;
          client.replica_ack_offset <- 0L
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
  let replica_apply arguments =
    match Command.parse arguments with
    | Ok command ->
        if Command.is_write command then (
          let outcome = apply_engine_command command in
          ignore outcome;
          Option.iter
            (fun upstream ->
              write_all upstream.fd
                (Resp.encode_command
                   [ "REPLCONF"; "ACK"; Int64.to_string upstream.offset ]))
            upstream)
    | Error _ -> ()
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
  let accept_client () =
    let client_fd, _ = Unix.accept server_fd in
    let client =
      {
        fd = client_fd;
        buffer = "";
        blocked = None;
        tx = None;
        authenticated = Option.is_none requirepass;
        watched = Hashtbl.create 8;
        subscriptions = Hashtbl.create 8;
        subscription_order_rev = [];
        replica_peer = false;
        replica_ack_offset = 0L;
        last_write_offset = 0L;
        closed = false;
      }
    in
    Hashtbl.replace clients client_fd client
  in
  let handle_client (client : client) =
    let chunk = Bytes.create 4096 in
    match Unix.read client.fd chunk 0 (Bytes.length chunk) with
    | 0 -> close_client client
    | read ->
        let data = Bytes.sub_string chunk 0 read in
        client.buffer <- client.buffer ^ data;
        process_buffer client
    | exception Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) ->
        close_client client
  in
  let handle_upstream upstream =
    let chunk = Bytes.create 4096 in
    match Unix.read upstream.fd chunk 0 (Bytes.length chunk) with
    | 0 -> upstream.link_status <- "down"
    | read ->
        upstream.buffer <- upstream.buffer ^ Bytes.sub_string chunk 0 read;
        process_upstream_buffer upstream
    | exception Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) ->
        upstream.link_status <- "down"
  in
  let rec loop () =
    Hashtbl.filter_map_inplace
      (fun _ (client : client) -> if client.closed then None else Some client)
      clients;
    process_timeouts ();
    if not !stop then (
      let client_fds =
        Hashtbl.to_seq_values clients
        |> List.of_seq
        |> List.filter (fun (client : client) -> Option.is_none client.blocked)
        |> List.map (fun (client : client) -> client.fd)
      in
      let upstream_fd =
        match upstream with
        | Some upstream when upstream.link_status = "up" -> [ upstream.fd ]
        | _ -> []
      in
      let ready =
        match
          Unix.select ((server_fd :: upstream_fd) @ client_fds) [] [] 0.1
        with
        | ready, _, _ -> ready
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> []
      in
      List.iter
        (fun fd ->
          if fd = server_fd then accept_client ()
          else
            match upstream with
            | Some upstream when fd = upstream.fd -> handle_upstream upstream
            | _ -> (
                match Hashtbl.find_opt clients fd with
                | Some client -> handle_client client
                | None -> ()))
        ready;
      loop ())
  in
  Fun.protect
    ~finally:(fun () ->
      Hashtbl.iter (fun _ client -> close_client client) clients;
      Option.iter (fun upstream -> Unix.close upstream.fd) upstream;
      Unix.close server_fd)
    loop
