let fail message = raise (Failure message)
let next_test_port = ref 41000

let assert_int64_equal ~expected ~actual ~context =
  if expected <> actual then
    fail (Printf.sprintf "%s: expected %Ld but got %Ld" context expected actual)

let assert_string_equal ~expected ~actual ~context =
  if expected <> actual then
    fail (Printf.sprintf "%s: expected %S but got %S" context expected actual)

let wrongtype_reply =
  "-WRONGTYPE Operation against a key holding the wrong kind of value\r\n"

let noauth_reply = "-NOAUTH Authentication required.\r\n"

let subscribed_mode_only_reply =
  "-ERR only SUBSCRIBE, UNSUBSCRIBE, and PING are allowed in this context\r\n"

let string_contains haystack needle =
  let hay_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop index =
    if needle_len = 0 then true
    else if index + needle_len > hay_len then false
    else if String.sub haystack index needle_len = needle then true
    else loop (index + 1)
  in
  loop 0

let assert_contains ~needle ~haystack ~context =
  if not (string_contains haystack needle) then
    fail
      (Printf.sprintf "%s: expected to find %S in %S" context needle haystack)

let assert_bool value ~context = if not value then fail context

let assert_wrongtype ~actual ~context =
  assert_string_equal ~expected:wrongtype_reply ~actual ~context

let assert_noauth ~actual ~context =
  assert_string_equal ~expected:noauth_reply ~actual ~context

let assert_none value ~context =
  match value with None -> () | Some _ -> fail (context ^ ": expected None")

let assert_some_int64_equal value ~expected ~context =
  match value with
  | Some actual -> assert_int64_equal ~expected ~actual ~context
  | None -> fail (context ^ ": expected Some value")

let with_server ?replicaof ?rdb_file ?requirepass test_fn =
  let discover_port () =
    let rec loop () =
      let port = !next_test_port in
      incr next_test_port;
      let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      match
        Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, port))
      with
      | () ->
          Unix.close socket;
          port
      | exception Unix.Unix_error _ ->
          Unix.close socket;
          loop ()
    in
    loop ()
  in
  let connect_with_retry port =
    let deadline = Unix.gettimeofday () +. 5.0 in
    let rec loop () =
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      match
        Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port))
      with
      | () -> fd
      | exception Unix.Unix_error _ ->
          Unix.close fd;
          if Unix.gettimeofday () >= deadline then
            fail "server did not accept connections in time"
          else (
            Unix.sleepf 0.05;
            loop ())
    in
    loop ()
  in
  let port = discover_port () in
  let executable = "../app/server_main.exe" in
  let argv =
    let base = [ executable; "--port"; string_of_int port ] in
    let with_replica =
      match replicaof with
      | None -> base
      | Some (host, upstream_port) ->
          base @ [ "--replicaof"; Printf.sprintf "%s:%d" host upstream_port ]
    in
    let with_rdb =
      match rdb_file with
      | None -> with_replica
      | Some path -> with_replica @ [ "--rdb-file"; path ]
    in
    let with_auth =
      match requirepass with
      | None -> with_rdb
      | Some password -> with_rdb @ [ "--requirepass"; password ]
    in
    Array.of_list with_auth
  in
  let pid =
    Unix.create_process executable argv Unix.stdin Unix.stdout Unix.stderr
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.kill pid Sys.sigterm;
      ignore (Unix.waitpid [] pid))
    (fun () -> test_fn ~port ~connect:(fun () -> connect_with_retry port))

let read_reply fd =
  let buffer = Bytes.create 8192 in
  let bytes_read = Unix.read fd buffer 0 (Bytes.length buffer) in
  if bytes_read = 0 then fail "expected reply but connection closed";
  Bytes.sub_string buffer 0 bytes_read

let read_until_quiet fd =
  let buffer = Buffer.create 256 in
  let first = read_reply fd in
  Buffer.add_string buffer first;
  let rec loop () =
    let ready, _, _ = Unix.select [ fd ] [] [] 0.05 in
    match ready with
    | [] -> Buffer.contents buffer
    | _ ->
        Buffer.add_string buffer (read_reply fd);
        loop ()
  in
  loop ()

let send_command fd arguments =
  let payload = Redis.Resp.encode_command arguments in
  let rec loop offset =
    if offset < String.length payload then
      let written =
        Unix.write_substring fd payload offset (String.length payload - offset)
      in
      loop (offset + written)
  in
  loop 0;
  read_reply fd

let parse_bulk_float_reply reply =
  match String.split_on_char '\r' reply with
  | _len :: value :: _ -> (
      match float_of_string_opt value with
      | Some value -> value
      | None -> fail ("expected float bulk reply, got " ^ reply))
  | _ -> fail ("unexpected bulk reply shape: " ^ reply)

let retry_until timeout_seconds f =
  let deadline = Unix.gettimeofday () +. timeout_seconds in
  let rec loop () =
    match f () with
    | Some value -> value
    | None ->
        if Unix.gettimeofday () >= deadline then
          fail "timed out waiting for condition"
        else (
          Unix.sleepf 0.05;
          loop ())
  in
  loop ()

let with_spawned_server argv test_fn =
  let executable = "../app/server_main.exe" in
  let pid =
    Unix.create_process executable
      (Array.of_list (executable :: argv))
      Unix.stdin Unix.stdout Unix.stderr
  in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.kill pid Sys.sigterm with _ -> ());
      ignore (Unix.waitpid [] pid))
    (fun () -> test_fn pid)

let with_temp_rdb entries test_fn =
  let write_len oc len =
    if len < 64 then output_byte oc len
    else fail "test fixture only supports small RDB strings"
  in
  let write_string oc value =
    write_len oc (String.length value);
    output_string oc value
  in
  let write_u64_le oc value =
    let open Int64 in
    for shift = 0 to 7 do
      output_byte oc
        (to_int (logand (shift_right_logical value (8 * shift)) 0xffL))
    done
  in
  let path = Filename.temp_file "ocaml-redis" ".rdb" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let oc = open_out_bin path in
      output_string oc "REDIS0009";
      output_byte oc 254;
      output_byte oc 0;
      List.iter
        (fun (key, value, expires_at_ms) ->
          Option.iter
            (fun expires_at_ms ->
              output_byte oc 252;
              write_u64_le oc expires_at_ms)
            expires_at_ms;
          output_byte oc 0;
          write_string oc key;
          write_string oc value)
        entries;
      output_byte oc 255;
      for _ = 1 to 8 do
        output_byte oc 0
      done;
      close_out oc;
      test_fn path)

let test_manual_clock_initial () =
  let clock = Redis.Clock.Manual.create ~now_ms:1_234L in
  assert_int64_equal ~expected:1_234L
    ~actual:(Redis.Clock.Manual.now_ms clock)
    ~context:"manual clock initial instant"

let test_manual_clock_advance () =
  let clock = Redis.Clock.Manual.create ~now_ms:50L in
  Redis.Clock.Manual.advance_ms clock 25L;
  assert_int64_equal ~expected:75L
    ~actual:(Redis.Clock.Manual.now_ms clock)
    ~context:"manual advance"

let test_manual_clock_set () =
  let clock = Redis.Clock.Manual.create ~now_ms:99L in
  Redis.Clock.Manual.set_ms clock 10_000L;
  assert_int64_equal ~expected:10_000L
    ~actual:(Redis.Clock.Manual.now_ms clock)
    ~context:"manual set"

let test_expiry_without_real_sleeps () =
  let clock = Redis.Clock.Manual.create ~now_ms:1_000L in
  let expires_at_ms = 1_500L in
  assert_bool
    (not
       (Redis.Expiry.is_expired
          ~now_ms:(Redis.Clock.Manual.now_ms clock)
          ~expires_at_ms))
    ~context:"key should not be expired before deadline";
  assert_some_int64_equal
    (Redis.Expiry.remaining_ms
       ~now_ms:(Redis.Clock.Manual.now_ms clock)
       ~expires_at_ms)
    ~expected:500L ~context:"remaining time before deadline";
  Redis.Clock.Manual.advance_ms clock 500L;
  assert_bool
    (Redis.Expiry.is_expired
       ~now_ms:(Redis.Clock.Manual.now_ms clock)
       ~expires_at_ms)
    ~context:"key should be expired at deadline";
  assert_none
    (Redis.Expiry.remaining_ms
       ~now_ms:(Redis.Clock.Manual.now_ms clock)
       ~expires_at_ms)
    ~context:"remaining time after expiry"

let test_engine_expiry () =
  let engine = Redis.Engine.create () in
  ignore
    (Redis.Engine.handle ~now_ms:1_000L engine
       (Redis.Command.Set
          { key = "greeting"; value = "hello"; expiry_ms = Some 500L }));
  let value_before_expiry =
    match
      (Redis.Engine.handle ~now_ms:1_200L engine (Redis.Command.Get "greeting"))
        .reply
    with
    | Some (Redis.Resp.Bulk_string (Some value)) -> value
    | _ -> fail "expected bulk string reply before expiry"
  in
  assert_string_equal ~expected:"hello" ~actual:value_before_expiry
    ~context:"value before expiry";
  assert_bool
    (match
       (Redis.Engine.handle ~now_ms:1_500L engine (Redis.Command.Get "greeting"))
         .reply
     with
    | Some (Redis.Resp.Bulk_string None) -> true
    | _ -> false)
    ~context:"expired value should be absent"

let test_engine_list_operations () =
  let engine = Redis.Engine.create () in
  let reply_of command =
    (Redis.Engine.handle ~now_ms:0L engine command).reply
  in
  assert_bool
    (match
       reply_of (Redis.Command.Rpush { key = "letters"; values = [ "a"; "b" ] })
     with
    | Some (Redis.Resp.Integer 2L) -> true
    | _ -> false)
    ~context:"rpush returns list length";
  assert_bool
    (match
       reply_of (Redis.Command.Lpush { key = "letters"; values = [ "z" ] })
     with
    | Some (Redis.Resp.Integer 3L) -> true
    | _ -> false)
    ~context:"lpush returns list length";
  assert_bool
    (match
       reply_of
         (Redis.Command.Lrange
            { key = "letters"; start_index = 0L; stop_index = -1L })
     with
    | Some
        (Redis.Resp.Array
           [
             Bulk_string (Some "z");
             Bulk_string (Some "a");
             Bulk_string (Some "b");
           ]) ->
        true
    | _ -> false)
    ~context:"lrange returns full list with negative stop";
  assert_bool
    (match reply_of (Redis.Command.Llen "letters") with
    | Some (Redis.Resp.Integer 3L) -> true
    | _ -> false)
    ~context:"llen returns list length";
  assert_bool
    (match
       reply_of
         (Redis.Command.Lrem { key = "letters"; count = 1L; element = "a" })
     with
    | Some (Redis.Resp.Integer 1L) -> true
    | _ -> false)
    ~context:"lrem removes one element"

let test_engine_sorted_set_operations () =
  let engine = Redis.Engine.create () in
  let reply_of command =
    (Redis.Engine.handle ~now_ms:0L engine command).reply
  in
  assert_bool
    (match
       reply_of
         (Redis.Command.Zadd
            { key = "scores"; score_members = [ (2., "bravo"); (1., "alpha") ] })
     with
    | Some (Redis.Resp.Integer 2L) -> true
    | _ -> false)
    ~context:"zadd creates set and counts new members";
  assert_bool
    (match
       reply_of
         (Redis.Command.Zadd
            { key = "scores"; score_members = [ (1., "charlie") ] })
     with
    | Some (Redis.Resp.Integer 1L) -> true
    | _ -> false)
    ~context:"zadd counts later inserted member";
  assert_bool
    (match
       reply_of
         (Redis.Command.Zrange
            { key = "scores"; start_index = 0L; stop_index = -1L })
     with
    | Some
        (Redis.Resp.Array
           [
             Bulk_string (Some "alpha");
             Bulk_string (Some "charlie");
             Bulk_string (Some "bravo");
           ]) ->
        true
    | _ -> false)
    ~context:"zrange uses score then lexicographic ordering";
  assert_bool
    (match
       reply_of (Redis.Command.Zrank { key = "scores"; member = "charlie" })
     with
    | Some (Redis.Resp.Integer 1L) -> true
    | _ -> false)
    ~context:"zrank returns zero-based rank"

let test_server_ping () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal ~expected:"+PONG\r\n"
            ~actual:(send_command client [ "PING" ])
            ~context:"first ping";
          assert_string_equal ~expected:"+PONG\r\n"
            ~actual:(send_command client [ "PING" ])
            ~context:"second ping"))

let test_server_echo () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal ~expected:"$5\r\nhello\r\n"
            ~actual:(send_command client [ "ECHO"; "hello" ])
            ~context:"echo reply"))

let test_server_shared_store_across_clients () =
  with_server (fun ~port:_ ~connect ->
      let client_a = connect () in
      let client_b = connect () in
      Fun.protect
        ~finally:(fun () ->
          Unix.close client_a;
          Unix.close client_b)
        (fun () ->
          assert_string_equal ~expected:"+OK\r\n"
            ~actual:(send_command client_a [ "SET"; "city"; "Toronto" ])
            ~context:"set from client A";
          assert_string_equal ~expected:"$7\r\nToronto\r\n"
            ~actual:(send_command client_b [ "GET"; "city" ])
            ~context:"get from client B";
          assert_string_equal ~expected:":2\r\n"
            ~actual:(send_command client_a [ "RPUSH"; "letters"; "a"; "b" ])
            ~context:"create list from client A";
          assert_string_equal ~expected:"*2\r\n$1\r\na\r\n$1\r\nb\r\n"
            ~actual:(send_command client_b [ "LRANGE"; "letters"; "0"; "-1" ])
            ~context:"list visible to client B"))

let test_server_blocking_list_pop () =
  with_server (fun ~port:_ ~connect ->
      let blocked_client = connect () in
      let producer = connect () in
      Fun.protect
        ~finally:(fun () ->
          Unix.close blocked_client;
          Unix.close producer)
        (fun () ->
          let request = Redis.Resp.encode_command [ "BLPOP"; "queue"; "1" ] in
          ignore
            (Unix.write_substring blocked_client request 0
               (String.length request));
          Unix.sleepf 0.1;
          assert_string_equal ~expected:":1\r\n"
            ~actual:(send_command producer [ "RPUSH"; "queue"; "job-1" ])
            ~context:"producer push wakes waiter";
          assert_string_equal ~expected:"*2\r\n$5\r\nqueue\r\n$5\r\njob-1\r\n"
            ~actual:(read_reply blocked_client)
            ~context:"blocked client receives popped value"))

let test_server_blocking_list_pop_timeout () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal ~expected:"*-1\r\n"
            ~actual:(send_command client [ "BLPOP"; "missing"; "1" ])
            ~context:"blpop timeout returns null array"))

let test_server_incr () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal ~expected:":1\r\n"
            ~actual:(send_command client [ "INCR"; "counter" ])
            ~context:"first incr initializes missing key";
          assert_string_equal ~expected:":2\r\n"
            ~actual:(send_command client [ "INCR"; "counter" ])
            ~context:"second incr increments existing key"))

let test_server_transactions_commit () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal ~expected:"+OK\r\n"
            ~actual:(send_command client [ "MULTI" ])
            ~context:"multi";
          assert_string_equal ~expected:"+QUEUED\r\n"
            ~actual:(send_command client [ "SET"; "counter"; "41" ])
            ~context:"queue set";
          assert_string_equal ~expected:"+QUEUED\r\n"
            ~actual:(send_command client [ "INCR"; "counter" ])
            ~context:"queue incr";
          assert_string_equal ~expected:"*2\r\n+OK\r\n:42\r\n"
            ~actual:(send_command client [ "EXEC" ])
            ~context:"exec replies";
          assert_string_equal ~expected:"$2\r\n42\r\n"
            ~actual:(send_command client [ "GET"; "counter" ])
            ~context:"value committed"))

let test_server_empty_transaction () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          ignore (send_command client [ "MULTI" ]);
          assert_string_equal ~expected:"*0\r\n"
            ~actual:(send_command client [ "EXEC" ])
            ~context:"empty exec returns empty array"))

let test_server_discard () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          ignore (send_command client [ "MULTI" ]);
          ignore (send_command client [ "SET"; "name"; "queued" ]);
          assert_string_equal ~expected:"+OK\r\n"
            ~actual:(send_command client [ "DISCARD" ])
            ~context:"discard reply";
          assert_string_equal ~expected:"$-1\r\n"
            ~actual:(send_command client [ "GET"; "name" ])
            ~context:"discarded writes are not applied"))

let test_server_transaction_runtime_error () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          ignore (send_command client [ "SET"; "counter"; "banana" ]);
          ignore (send_command client [ "MULTI" ]);
          ignore (send_command client [ "INCR"; "counter" ]);
          assert_string_equal
            ~expected:"*1\r\n-ERR value is not an integer or out of range\r\n"
            ~actual:(send_command client [ "EXEC" ])
            ~context:"runtime errors are returned inside exec array"))

let test_server_watch_abort_on_change () =
  with_server (fun ~port:_ ~connect ->
      let watcher = connect () in
      let modifier = connect () in
      Fun.protect
        ~finally:(fun () ->
          Unix.close watcher;
          Unix.close modifier)
        (fun () ->
          ignore (send_command watcher [ "WATCH"; "counter" ]);
          ignore (send_command watcher [ "MULTI" ]);
          ignore (send_command watcher [ "SET"; "counter"; "watcher" ]);
          ignore (send_command modifier [ "SET"; "counter"; "modifier" ]);
          assert_string_equal ~expected:"*-1\r\n"
            ~actual:(send_command watcher [ "EXEC" ])
            ~context:"watched modification aborts exec";
          assert_string_equal ~expected:"$8\r\nmodifier\r\n"
            ~actual:(send_command modifier [ "GET"; "counter" ])
            ~context:"modifier value remains"))

let test_server_unwatch_clears_abort_condition () =
  with_server (fun ~port:_ ~connect ->
      let watcher = connect () in
      let modifier = connect () in
      Fun.protect
        ~finally:(fun () ->
          Unix.close watcher;
          Unix.close modifier)
        (fun () ->
          ignore (send_command watcher [ "WATCH"; "counter" ]);
          assert_string_equal ~expected:"+OK\r\n"
            ~actual:(send_command watcher [ "UNWATCH" ])
            ~context:"unwatch reply";
          ignore (send_command watcher [ "MULTI" ]);
          ignore (send_command watcher [ "SET"; "counter"; "watcher" ]);
          ignore (send_command modifier [ "SET"; "counter"; "modifier" ]);
          assert_string_equal ~expected:"*1\r\n+OK\r\n"
            ~actual:(send_command watcher [ "EXEC" ])
            ~context:"exec succeeds after unwatch";
          assert_string_equal ~expected:"$7\r\nwatcher\r\n"
            ~actual:(send_command watcher [ "GET"; "counter" ])
            ~context:"transaction write wins after unwatch"))

let test_server_watch_inside_multi_errors () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          ignore (send_command client [ "MULTI" ]);
          assert_string_equal
            ~expected:"-ERR WATCH inside MULTI is not allowed\r\n"
            ~actual:(send_command client [ "WATCH"; "counter" ])
            ~context:"watch inside multi errors";
          assert_string_equal
            ~expected:
              "-EXECABORT Transaction discarded because of previous errors.\r\n"
            ~actual:(send_command client [ "EXEC" ])
            ~context:"exec aborts after transaction became dirty"))

let test_server_type_command () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal ~expected:"+none\r\n"
            ~actual:(send_command client [ "TYPE"; "missing" ])
            ~context:"missing key type";
          ignore (send_command client [ "SET"; "name"; "alice" ]);
          ignore (send_command client [ "RPUSH"; "letters"; "a" ]);
          ignore (send_command client [ "XADD"; "stream"; "1-*"; "f"; "v" ]);
          assert_string_equal ~expected:"+string\r\n"
            ~actual:(send_command client [ "TYPE"; "name" ])
            ~context:"string type";
          assert_string_equal ~expected:"+list\r\n"
            ~actual:(send_command client [ "TYPE"; "letters" ])
            ~context:"list type";
          assert_string_equal ~expected:"+stream\r\n"
            ~actual:(send_command client [ "TYPE"; "stream" ])
            ~context:"stream type"))

let test_server_sorted_set_commands () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal ~expected:":2\r\n"
            ~actual:
              (send_command client
                 [ "ZADD"; "scores"; "1"; "alpha"; "2"; "bravo" ])
            ~context:"zadd added count";
          assert_string_equal ~expected:":0\r\n"
            ~actual:(send_command client [ "ZADD"; "scores"; "3"; "alpha" ])
            ~context:"zadd update count";
          assert_string_equal ~expected:":2\r\n"
            ~actual:(send_command client [ "ZCARD"; "scores" ])
            ~context:"zcard after update";
          assert_string_equal ~expected:"$1\r\n3\r\n"
            ~actual:(send_command client [ "ZSCORE"; "scores"; "alpha" ])
            ~context:"zscore returns canonical integer string";
          assert_string_equal ~expected:"*2\r\n$5\r\nbravo\r\n$5\r\nalpha\r\n"
            ~actual:(send_command client [ "ZRANGE"; "scores"; "-2"; "-1" ])
            ~context:"zrange negative indexes";
          assert_string_equal ~expected:":1\r\n"
            ~actual:(send_command client [ "ZRANK"; "scores"; "alpha" ])
            ~context:"zrank on updated member";
          assert_string_equal ~expected:"$-1\r\n"
            ~actual:(send_command client [ "ZRANK"; "scores"; "missing" ])
            ~context:"zrank missing member";
          assert_string_equal ~expected:":1\r\n"
            ~actual:
              (send_command client [ "ZREM"; "scores"; "bravo"; "missing" ])
            ~context:"zrem removes existing only";
          assert_string_equal ~expected:"+zset\r\n"
            ~actual:(send_command client [ "TYPE"; "scores" ])
            ~context:"type zset";
          assert_string_equal ~expected:":0\r\n"
            ~actual:(send_command client [ "ZCARD"; "missing" ])
            ~context:"zcard missing key"))

let test_server_sorted_set_wrong_type_and_invalid_score () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          ignore (send_command client [ "SET"; "name"; "alice" ]);
          assert_string_equal
            ~expected:
              "-WRONGTYPE Operation against a key holding the wrong kind of \
               value\r\n"
            ~actual:(send_command client [ "ZADD"; "name"; "1"; "member" ])
            ~context:"zadd wrong type";
          assert_string_equal
            ~expected:"-ERR value is not a valid float: nope\r\n"
            ~actual:(send_command client [ "ZADD"; "scores"; "nope"; "member" ])
            ~context:"invalid zadd score"))

let test_server_xadd_and_xrange () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal ~expected:"$3\r\n1-0\r\n"
            ~actual:
              (send_command client
                 [ "XADD"; "mystream"; "1-*"; "name"; "alice" ])
            ~context:"partial auto id first entry";
          assert_string_equal ~expected:"$3\r\n1-1\r\n"
            ~actual:
              (send_command client [ "XADD"; "mystream"; "1-*"; "name"; "bob" ])
            ~context:"partial auto id second entry";
          assert_string_equal
            ~expected:
              "*2\r\n\
               *2\r\n\
               $3\r\n\
               1-0\r\n\
               *2\r\n\
               $4\r\n\
               name\r\n\
               $5\r\n\
               alice\r\n\
               *2\r\n\
               $3\r\n\
               1-1\r\n\
               *2\r\n\
               $4\r\n\
               name\r\n\
               $3\r\n\
               bob\r\n"
            ~actual:(send_command client [ "XRANGE"; "mystream"; "-"; "+" ])
            ~context:"xrange full stream"))

let test_server_xadd_rejects_non_increasing_ids () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          ignore (send_command client [ "XADD"; "mystream"; "1-0"; "f"; "v" ]);
          assert_string_equal
            ~expected:
              "-ERR The ID specified in XADD is equal or smaller than the \
               target stream top item\r\n"
            ~actual:
              (send_command client [ "XADD"; "mystream"; "1-0"; "f"; "w" ])
            ~context:"reject equal ids"))

let test_server_fully_auto_generated_stream_ids () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          let first =
            send_command client [ "XADD"; "mystream"; "*"; "f"; "v1" ]
          in
          let second =
            send_command client [ "XADD"; "mystream"; "*"; "f"; "v2" ]
          in
          assert_bool
            (String.starts_with ~prefix:"$" first)
            ~context:"first auto id bulk";
          assert_bool
            (String.starts_with ~prefix:"$" second)
            ~context:"second auto id bulk";
          assert_bool (first <> second) ~context:"auto ids differ"))

let test_server_xread_single_stream () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          ignore (send_command client [ "XADD"; "mystream"; "1-0"; "f"; "v1" ]);
          ignore (send_command client [ "XADD"; "mystream"; "2-0"; "f"; "v2" ]);
          assert_string_equal
            ~expected:
              "*1\r\n\
               *2\r\n\
               $8\r\n\
               mystream\r\n\
               *2\r\n\
               *2\r\n\
               $3\r\n\
               1-0\r\n\
               *2\r\n\
               $1\r\n\
               f\r\n\
               $2\r\n\
               v1\r\n\
               *2\r\n\
               $3\r\n\
               2-0\r\n\
               *2\r\n\
               $1\r\n\
               f\r\n\
               $2\r\n\
               v2\r\n"
            ~actual:
              (send_command client [ "XREAD"; "STREAMS"; "mystream"; "0-0" ])
            ~context:"xread single stream"))

let test_server_xread_multiple_streams () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          ignore (send_command client [ "XADD"; "s1"; "1-0"; "f"; "v1" ]);
          ignore (send_command client [ "XADD"; "s2"; "1-0"; "f"; "v2" ]);
          assert_string_equal
            ~expected:
              "*2\r\n\
               *2\r\n\
               $2\r\n\
               s1\r\n\
               *1\r\n\
               *2\r\n\
               $3\r\n\
               1-0\r\n\
               *2\r\n\
               $1\r\n\
               f\r\n\
               $2\r\n\
               v1\r\n\
               *2\r\n\
               $2\r\n\
               s2\r\n\
               *1\r\n\
               *2\r\n\
               $3\r\n\
               1-0\r\n\
               *2\r\n\
               $1\r\n\
               f\r\n\
               $2\r\n\
               v2\r\n"
            ~actual:
              (send_command client
                 [ "XREAD"; "STREAMS"; "s1"; "s2"; "0-0"; "0-0" ])
            ~context:"xread multiple streams"))

let test_server_blocking_xread_uses_dollar () =
  with_server (fun ~port:_ ~connect ->
      let reader = connect () in
      let writer = connect () in
      Fun.protect
        ~finally:(fun () ->
          Unix.close reader;
          Unix.close writer)
        (fun () ->
          ignore (send_command writer [ "XADD"; "mystream"; "1-0"; "f"; "old" ]);
          let request =
            Redis.Resp.encode_command
              [ "XREAD"; "BLOCK"; "1000"; "STREAMS"; "mystream"; "$" ]
          in
          ignore (Unix.write_substring reader request 0 (String.length request));
          Unix.sleepf 0.1;
          ignore (send_command writer [ "XADD"; "mystream"; "2-0"; "f"; "new" ]);
          assert_string_equal
            ~expected:
              "*1\r\n\
               *2\r\n\
               $8\r\n\
               mystream\r\n\
               *1\r\n\
               *2\r\n\
               $3\r\n\
               2-0\r\n\
               *2\r\n\
               $1\r\n\
               f\r\n\
               $3\r\n\
               new\r\n"
            ~actual:(read_reply reader)
            ~context:"blocking xread returns only new entry"))

let test_server_blocking_xread_timeout () =
  with_server (fun ~port:_ ~connect ->
      let reader = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close reader)
        (fun () ->
          assert_string_equal ~expected:"*-1\r\n"
            ~actual:
              (send_command reader
                 [ "XREAD"; "BLOCK"; "1000"; "STREAMS"; "mystream"; "$" ])
            ~context:"blocking xread timeout returns null array"))

let test_server_blocking_xread_zero_blocks_until_entry () =
  with_server (fun ~port:_ ~connect ->
      let reader = connect () in
      let writer = connect () in
      Fun.protect
        ~finally:(fun () ->
          Unix.close reader;
          Unix.close writer)
        (fun () ->
          let request =
            Redis.Resp.encode_command
              [ "XREAD"; "BLOCK"; "0"; "STREAMS"; "mystream"; "$" ]
          in
          ignore (Unix.write_substring reader request 0 (String.length request));
          Unix.sleepf 0.1;
          ignore (send_command writer [ "XADD"; "mystream"; "1-0"; "f"; "v1" ]);
          assert_string_equal
            ~expected:
              "*1\r\n\
               *2\r\n\
               $8\r\n\
               mystream\r\n\
               *1\r\n\
               *2\r\n\
               $3\r\n\
               1-0\r\n\
               *2\r\n\
               $1\r\n\
               f\r\n\
               $2\r\n\
               v1\r\n"
            ~actual:(read_reply reader)
            ~context:"xread block 0 should wait for a later entry"))

let test_replication_info_master () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          let reply = send_command client [ "INFO"; "replication" ] in
          assert_contains ~needle:"role:master" ~haystack:reply
            ~context:"master role";
          assert_contains ~needle:"master_replid:" ~haystack:reply
            ~context:"master replid";
          assert_contains ~needle:"master_repl_offset:0" ~haystack:reply
            ~context:"initial repl offset"))

let test_replication_handshake_protocol () =
  with_server (fun ~port ~connect ->
      let replica = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close replica)
        (fun () ->
          assert_string_equal ~expected:"+PONG\r\n"
            ~actual:(send_command replica [ "PING" ])
            ~context:"manual ping";
          assert_string_equal ~expected:"+OK\r\n"
            ~actual:
              (send_command replica [ "REPLCONF"; "listening-port"; "6381" ])
            ~context:"replconf listening-port";
          assert_string_equal ~expected:"+OK\r\n"
            ~actual:(send_command replica [ "REPLCONF"; "capa"; "psync2" ])
            ~context:"replconf capa";
          let psync_reply = send_command replica [ "PSYNC"; "?"; "-1" ] in
          assert_contains ~needle:"+FULLRESYNC " ~haystack:psync_reply
            ~context:"fullresync header";
          assert_contains ~needle:"$0\r\n\r\n" ~haystack:psync_reply
            ~context:"empty rdb payload"))

let test_single_replica_propagation () =
  with_server (fun ~port:master_port ~connect:connect_master ->
      with_server ~replicaof:("127.0.0.1", master_port)
        (fun ~port:_ ~connect:connect_replica ->
          let master = connect_master () in
          let replica = connect_replica () in
          Fun.protect
            ~finally:(fun () ->
              Unix.close master;
              Unix.close replica)
            (fun () ->
              ignore
                (retry_until 2.0 (fun () ->
                     let reply =
                       send_command replica [ "INFO"; "replication" ]
                     in
                     if string_contains reply "master_link_status:up" then
                       Some reply
                     else None));
              ignore (send_command master [ "SET"; "city"; "Toronto" ]);
              let get_reply =
                retry_until 2.0 (fun () ->
                    let reply = send_command replica [ "GET"; "city" ] in
                    if reply = "$7\r\nToronto\r\n" then Some reply else None)
              in
              assert_string_equal ~expected:"$7\r\nToronto\r\n"
                ~actual:get_reply ~context:"replicated get")))

let test_multi_replica_propagation () =
  with_server (fun ~port:master_port ~connect:connect_master ->
      with_server ~replicaof:("127.0.0.1", master_port)
        (fun ~port:_ ~connect:connect_replica_a ->
          with_server ~replicaof:("127.0.0.1", master_port)
            (fun ~port:_ ~connect:connect_replica_b ->
              let master = connect_master () in
              let replica_a = connect_replica_a () in
              let replica_b = connect_replica_b () in
              Fun.protect
                ~finally:(fun () ->
                  Unix.close master;
                  Unix.close replica_a;
                  Unix.close replica_b)
                (fun () ->
                  let wait_for_link fd =
                    retry_until 5.0 (fun () ->
                        let reply = send_command fd [ "INFO"; "replication" ] in
                        if string_contains reply "master_link_status:up" then
                          Some reply
                        else None)
                  in
                  ignore (wait_for_link replica_a);
                  ignore (wait_for_link replica_b);
                  ignore
                    (retry_until 5.0 (fun () ->
                         let reply =
                           send_command master [ "INFO"; "replication" ]
                         in
                         if string_contains reply "connected_slaves:2" then
                           Some reply
                         else None));
                  ignore (send_command master [ "SET"; "weather"; "sunny" ]);
                  let expect_replica fd =
                    retry_until 5.0 (fun () ->
                        let reply = send_command fd [ "GET"; "weather" ] in
                        if reply = "$5\r\nsunny\r\n" then Some reply else None)
                  in
                  ignore (expect_replica replica_a);
                  ignore (expect_replica replica_b)))))

let test_wait_with_no_replicas () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal ~expected:":0\r\n"
            ~actual:(send_command client [ "WAIT"; "1"; "100" ])
            ~context:"wait without replicas returns zero"))

let test_wait_with_no_commands () =
  with_server (fun ~port:master_port ~connect:connect_master ->
      with_server ~replicaof:("127.0.0.1", master_port)
        (fun ~port:_ ~connect:connect_replica ->
          let master = connect_master () in
          let replica = connect_replica () in
          Fun.protect
            ~finally:(fun () ->
              Unix.close master;
              Unix.close replica)
            (fun () ->
              ignore
                (retry_until 5.0 (fun () ->
                     let reply =
                       send_command replica [ "INFO"; "replication" ]
                     in
                     if string_contains reply "master_link_status:up" then
                       Some reply
                     else None));
              assert_string_equal ~expected:":1\r\n"
                ~actual:(send_command master [ "WAIT"; "1"; "1000" ])
                ~context:"wait with no commands uses initial replica ack")))

let test_wait_after_multiple_commands () =
  with_server (fun ~port:master_port ~connect:connect_master ->
      with_server ~replicaof:("127.0.0.1", master_port)
        (fun ~port:_ ~connect:connect_replica_a ->
          with_server ~replicaof:("127.0.0.1", master_port)
            (fun ~port:_ ~connect:connect_replica_b ->
              let master = connect_master () in
              let replica_a = connect_replica_a () in
              let replica_b = connect_replica_b () in
              Fun.protect
                ~finally:(fun () ->
                  Unix.close master;
                  Unix.close replica_a;
                  Unix.close replica_b)
                (fun () ->
                  let wait_for_link fd =
                    retry_until 5.0 (fun () ->
                        let reply = send_command fd [ "INFO"; "replication" ] in
                        if string_contains reply "master_link_status:up" then
                          Some reply
                        else None)
                  in
                  ignore (wait_for_link replica_a);
                  ignore (wait_for_link replica_b);
                  ignore (send_command master [ "SET"; "counter"; "1" ]);
                  ignore (send_command master [ "INCR"; "counter" ]);
                  assert_string_equal ~expected:":2\r\n"
                    ~actual:(send_command master [ "WAIT"; "2"; "5000" ])
                    ~context:"wait observes multiple replica acks after writes";
                  ignore
                    (retry_until 5.0 (fun () ->
                         let reply =
                           send_command replica_a [ "GET"; "counter" ]
                         in
                         if reply = "$1\r\n2\r\n" then Some reply else None));
                  ignore
                    (retry_until 5.0 (fun () ->
                         let reply =
                           send_command replica_b [ "GET"; "counter" ]
                         in
                         if reply = "$1\r\n2\r\n" then Some reply else None))))))

let test_rdb_load_single_key () =
  with_temp_rdb
    [ ("city", "Toronto", None) ]
    (fun path ->
      with_server ~rdb_file:path (fun ~port:_ ~connect ->
          let client = connect () in
          Fun.protect
            ~finally:(fun () -> Unix.close client)
            (fun () ->
              assert_string_equal ~expected:"$7\r\nToronto\r\n"
                ~actual:(send_command client [ "GET"; "city" ])
                ~context:"single key loaded from rdb")))

let test_rdb_load_multiple_keys () =
  with_temp_rdb
    [ ("city", "Toronto", None); ("food", "poutine", None) ]
    (fun path ->
      with_server ~rdb_file:path (fun ~port:_ ~connect ->
          let client = connect () in
          Fun.protect
            ~finally:(fun () -> Unix.close client)
            (fun () ->
              assert_string_equal ~expected:"$7\r\nToronto\r\n"
                ~actual:(send_command client [ "GET"; "city" ])
                ~context:"first rdb key";
              assert_string_equal ~expected:"$7\r\npoutine\r\n"
                ~actual:(send_command client [ "GET"; "food" ])
                ~context:"second rdb key")))

let test_rdb_load_expiry () =
  let now_ms = Int64.of_float (Unix.gettimeofday () *. 1000.) in
  let future = Int64.add now_ms 5_000L in
  let past = Int64.sub now_ms 1_000L in
  with_temp_rdb
    [ ("fresh", "value", Some future); ("stale", "old", Some past) ]
    (fun path ->
      with_server ~rdb_file:path (fun ~port:_ ~connect ->
          let client = connect () in
          Fun.protect
            ~finally:(fun () -> Unix.close client)
            (fun () ->
              assert_string_equal ~expected:"$5\r\nvalue\r\n"
                ~actual:(send_command client [ "GET"; "fresh" ])
                ~context:"future expiry key is loaded";
              assert_string_equal ~expected:"$-1\r\n"
                ~actual:(send_command client [ "GET"; "stale" ])
                ~context:"expired rdb key is ignored at read time")))

let test_pubsub_subscribe_and_ping () =
  with_server (fun ~port:_ ~connect ->
      let subscriber = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close subscriber)
        (fun () ->
          assert_string_equal
            ~expected:"*3\r\n$9\r\nsubscribe\r\n$5\r\nnews!\r\n:1\r\n"
            ~actual:(send_command subscriber [ "SUBSCRIBE"; "news!" ])
            ~context:"subscribe ack";
          assert_string_equal ~expected:"*2\r\n$4\r\npong\r\n$5\r\nhello\r\n"
            ~actual:(send_command subscriber [ "PING"; "hello" ])
            ~context:"ping in subscribed mode"))

let test_pubsub_subscribe_multiple_channels () =
  with_server (fun ~port:_ ~connect ->
      let subscriber = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close subscriber)
        (fun () ->
          let payload =
            Redis.Resp.encode_command [ "SUBSCRIBE"; "alpha"; "beta" ]
          in
          ignore
            (Unix.write_substring subscriber payload 0 (String.length payload));
          assert_string_equal
            ~expected:
              "*3\r\n\
               $9\r\n\
               subscribe\r\n\
               $5\r\n\
               alpha\r\n\
               :1\r\n\
               *3\r\n\
               $9\r\n\
               subscribe\r\n\
               $4\r\n\
               beta\r\n\
               :2\r\n"
            ~actual:(read_until_quiet subscriber)
            ~context:"subscribe multiple channels"))

let test_pubsub_publish_and_unsubscribe () =
  with_server (fun ~port:_ ~connect ->
      let subscriber = connect () in
      let publisher = connect () in
      Fun.protect
        ~finally:(fun () ->
          Unix.close subscriber;
          Unix.close publisher)
        (fun () ->
          ignore (send_command subscriber [ "SUBSCRIBE"; "updates" ]);
          assert_string_equal ~expected:":1\r\n"
            ~actual:(send_command publisher [ "PUBLISH"; "updates"; "payload" ])
            ~context:"publish recipient count";
          assert_string_equal
            ~expected:
              "*3\r\n$7\r\nmessage\r\n$7\r\nupdates\r\n$7\r\npayload\r\n"
            ~actual:(read_reply subscriber)
            ~context:"subscriber receives payload";
          assert_string_equal
            ~expected:"*3\r\n$11\r\nunsubscribe\r\n$7\r\nupdates\r\n:0\r\n"
            ~actual:(send_command subscriber [ "UNSUBSCRIBE"; "updates" ])
            ~context:"unsubscribe ack";
          assert_string_equal ~expected:":0\r\n"
            ~actual:
              (send_command publisher [ "PUBLISH"; "updates"; "payload-2" ])
            ~context:"publish after unsubscribe reaches nobody"))

let test_pubsub_unsubscribe_all_and_ping_edges () =
  with_server (fun ~port:_ ~connect ->
      let subscriber = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close subscriber)
        (fun () ->
          assert_string_equal
            ~expected:"*3\r\n$11\r\nunsubscribe\r\n$-1\r\n:0\r\n"
            ~actual:(send_command subscriber [ "UNSUBSCRIBE" ])
            ~context:"unsubscribe without subscriptions returns nil channel";
          let subscribe_payload =
            Redis.Resp.encode_command [ "SUBSCRIBE"; "first"; "second" ]
          in
          ignore
            (Unix.write_substring subscriber subscribe_payload 0
               (String.length subscribe_payload));
          ignore (read_until_quiet subscriber);
          assert_string_equal ~expected:"*2\r\n$4\r\npong\r\n$0\r\n\r\n"
            ~actual:(send_command subscriber [ "PING" ])
            ~context:"bare ping in subscribed mode uses empty payload";
          let payload = Redis.Resp.encode_command [ "UNSUBSCRIBE" ] in
          ignore
            (Unix.write_substring subscriber payload 0 (String.length payload));
          assert_string_equal
            ~expected:
              "*3\r\n\
               $11\r\n\
               unsubscribe\r\n\
               $6\r\n\
               second\r\n\
               :1\r\n\
               *3\r\n\
               $11\r\n\
               unsubscribe\r\n\
               $5\r\n\
               first\r\n\
               :0\r\n"
            ~actual:(read_until_quiet subscriber)
            ~context:"unsubscribe all unwinds subscriptions in reverse order";
          assert_string_equal ~expected:"+PONG\r\n"
            ~actual:(send_command subscriber [ "PING" ])
            ~context:"after final unsubscribe client leaves subscribed mode"))

let test_server_geo_commands () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal ~expected:":2\r\n"
            ~actual:
              (send_command client
                 [ "GEOADD"; "places"; "0"; "0"; "origin"; "0"; "1"; "north" ])
            ~context:"geoadd initial count";
          assert_string_equal ~expected:":0\r\n"
            ~actual:
              (send_command client [ "GEOADD"; "places"; "0"; "0"; "origin" ])
            ~context:"geoadd update count";
          assert_string_equal ~expected:"+zset\r\n"
            ~actual:(send_command client [ "TYPE"; "places" ])
            ~context:"type geo key";
          assert_string_equal
            ~expected:
              "*3\r\n\
               *2\r\n\
               $1\r\n\
               0\r\n\
               $1\r\n\
               0\r\n\
               *2\r\n\
               $1\r\n\
               0\r\n\
               $1\r\n\
               1\r\n\
               *-1\r\n"
            ~actual:
              (send_command client
                 [ "GEOPOS"; "places"; "origin"; "north"; "missing" ])
            ~context:"geopos returns coordinates and null entry";
          let dist_km =
            parse_bulk_float_reply
              (send_command client
                 [ "GEODIST"; "places"; "origin"; "north"; "km" ])
          in
          assert_bool
            (dist_km > 111. && dist_km < 112.)
            ~context:"geodist km within tolerance";
          assert_string_equal ~expected:"*2\r\n$5\r\nnorth\r\n$6\r\norigin\r\n"
            ~actual:
              (send_command client
                 [
                   "GEOSEARCH";
                   "places";
                   "FROMLONLAT";
                   "0";
                   "0";
                   "BYRADIUS";
                   "120";
                   "km";
                 ])
            ~context:"geosearch radius includes boundary-near members"))

let test_server_geo_wrong_type_and_validation () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          ignore
            (send_command client [ "GEOADD"; "places"; "0"; "0"; "origin" ]);
          ignore (send_command client [ "ZADD"; "scores"; "1"; "alpha" ]);
          assert_string_equal
            ~expected:"-ERR invalid coordinates for member: bad\r\n"
            ~actual:
              (send_command client [ "GEOADD"; "oops"; "200"; "0"; "bad" ])
            ~context:"invalid longitude rejected";
          assert_wrongtype
            ~actual:(send_command client [ "ZRANGE"; "places"; "0"; "-1" ])
            ~context:"zrange on geo key wrongtype";
          assert_wrongtype
            ~actual:(send_command client [ "GEOPOS"; "scores"; "alpha" ])
            ~context:"geopos on plain zset wrongtype"))

let test_server_wrong_type_matrix_smoke () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          ignore (send_command client [ "SET"; "s"; "value" ]);
          ignore (send_command client [ "RPUSH"; "l"; "a" ]);
          ignore (send_command client [ "XADD"; "st"; "1-0"; "f"; "v" ]);
          ignore (send_command client [ "ZADD"; "zs"; "1"; "alpha" ]);
          ignore (send_command client [ "GEOADD"; "geo"; "0"; "0"; "origin" ]);
          let assert_wrongtype_command context command =
            assert_wrongtype ~actual:(send_command client command) ~context
          in
          assert_wrongtype_command "get on list" [ "GET"; "l" ];
          assert_wrongtype_command "get on stream" [ "GET"; "st" ];
          assert_wrongtype_command "get on zset" [ "GET"; "zs" ];
          assert_wrongtype_command "get on geo" [ "GET"; "geo" ];
          assert_wrongtype_command "llen on string" [ "LLEN"; "s" ];
          assert_wrongtype_command "llen on stream" [ "LLEN"; "st" ];
          assert_wrongtype_command "llen on zset" [ "LLEN"; "zs" ];
          assert_wrongtype_command "llen on geo" [ "LLEN"; "geo" ];
          assert_wrongtype_command "xrange on string"
            [ "XRANGE"; "s"; "-"; "+" ];
          assert_wrongtype_command "xrange on list" [ "XRANGE"; "l"; "-"; "+" ];
          assert_wrongtype_command "xrange on zset" [ "XRANGE"; "zs"; "-"; "+" ];
          assert_wrongtype_command "xrange on geo" [ "XRANGE"; "geo"; "-"; "+" ]))

let test_server_missing_key_reply_shapes () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal ~expected:"*0\r\n"
            ~actual:(send_command client [ "LRANGE"; "missing"; "0"; "-1" ])
            ~context:"lrange missing returns empty array";
          assert_string_equal ~expected:"*0\r\n"
            ~actual:(send_command client [ "ZRANGE"; "missing"; "0"; "-1" ])
            ~context:"zrange missing returns empty array";
          assert_string_equal ~expected:"*0\r\n"
            ~actual:(send_command client [ "XRANGE"; "missing"; "-"; "+" ])
            ~context:"xrange missing returns empty array";
          assert_string_equal ~expected:"*2\r\n*-1\r\n*-1\r\n"
            ~actual:(send_command client [ "GEOPOS"; "missing"; "a"; "b" ])
            ~context:"geopos all-missing returns null entries";
          assert_string_equal ~expected:"*0\r\n"
            ~actual:
              (send_command client
                 [
                   "GEOSEARCH";
                   "missing";
                   "FROMLONLAT";
                   "0";
                   "0";
                   "BYRADIUS";
                   "1";
                   "km";
                 ])
            ~context:"geosearch missing returns empty array"))

let test_auth_gate_matrix () =
  with_server ~requirepass:"secret" (fun ~port:_ ~connect ->
      let unauth = connect () in
      let authd = connect () in
      Fun.protect
        ~finally:(fun () ->
          Unix.close unauth;
          Unix.close authd)
        (fun () ->
          let assert_noauth_command command context =
            assert_noauth ~actual:(send_command unauth command) ~context
          in
          assert_noauth_command [ "SUBSCRIBE"; "alerts" ]
            "subscribe denied pre-auth";
          assert_noauth_command
            [ "PUBLISH"; "alerts"; "msg" ]
            "publish denied pre-auth";
          assert_noauth_command [ "MULTI" ] "multi denied pre-auth";
          assert_noauth_command
            [ "XREAD"; "STREAMS"; "mystream"; "0-0" ]
            "xread denied pre-auth";
          assert_noauth_command [ "WAIT"; "1"; "1000" ] "wait denied pre-auth";
          assert_noauth_command [ "INFO"; "replication" ]
            "info replication denied pre-auth";
          ignore (send_command authd [ "AUTH"; "secret" ]);
          assert_string_equal
            ~expected:"*3\r\n$9\r\nsubscribe\r\n$6\r\nalerts\r\n:1\r\n"
            ~actual:(send_command authd [ "SUBSCRIBE"; "alerts" ])
            ~context:"subscribe allowed after auth";
          assert_string_equal ~expected:subscribed_mode_only_reply
            ~actual:(send_command authd [ "GET"; "k" ])
            ~context:"subscribed mode takes precedence after auth"))

let test_startup_config_validation_bundle () =
  let executable = "../app/server_main.exe" in
  let run argv =
    let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
    let pid =
      Unix.create_process executable
        (Array.of_list (executable :: argv))
        Unix.stdin devnull devnull
    in
    let _, status = Unix.waitpid [] pid in
    Unix.close devnull;
    status
  in
  let malformed_path = Filename.temp_file "ocaml-redis-bad" ".rdb" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists malformed_path then Sys.remove malformed_path)
    (fun () ->
      let oc = open_out_bin malformed_path in
      output_string oc "NOTRDB";
      close_out oc;
      assert_bool
        (match run [ "--replicaof"; "bad-format" ] with
        | Unix.WEXITED 0 -> false
        | _ -> true)
        ~context:"invalid replicaof format fails";
      assert_bool
        (match run [ "--rdb-file"; "/definitely/missing.rdb" ] with
        | Unix.WEXITED 0 -> false
        | _ -> true)
        ~context:"missing rdb file fails";
      assert_bool
        (match run [ "--rdb-file"; malformed_path ] with
        | Unix.WEXITED 0 -> false
        | _ -> true)
        ~context:"malformed rdb file fails")

let test_replication_propagates_zset_and_geo_writes () =
  with_server (fun ~port:master_port ~connect:connect_master ->
      with_server ~replicaof:("127.0.0.1", master_port)
        (fun ~port:_ ~connect:connect_replica ->
          let master = connect_master () in
          let replica = connect_replica () in
          Fun.protect
            ~finally:(fun () ->
              Unix.close master;
              Unix.close replica)
            (fun () ->
              ignore
                (retry_until 5.0 (fun () ->
                     let reply =
                       send_command replica [ "INFO"; "replication" ]
                     in
                     if string_contains reply "master_link_status:up" then
                       Some reply
                     else None));
              ignore (send_command master [ "ZADD"; "scores"; "1"; "alpha" ]);
              ignore
                (send_command master [ "GEOADD"; "places"; "0"; "0"; "origin" ]);
              ignore
                (retry_until 5.0 (fun () ->
                     let reply = send_command replica [ "ZCARD"; "scores" ] in
                     if reply = ":1\r\n" then Some reply else None));
              ignore
                (retry_until 5.0 (fun () ->
                     let reply =
                       send_command replica [ "GEOPOS"; "places"; "origin" ]
                     in
                     if string_contains reply "$1\r\n0\r\n$1\r\n0\r\n" then
                       Some reply
                     else None)))))

let test_auth_startup_rejects_replica_mode () =
  let port = !next_test_port in
  incr next_test_port;
  let upstream_port = !next_test_port in
  incr next_test_port;
  let executable = "../app/server_main.exe" in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  let pid =
    Unix.create_process executable
      [|
        executable;
        "--port";
        string_of_int port;
        "--replicaof";
        Printf.sprintf "127.0.0.1:%d" upstream_port;
        "--requirepass";
        "secret";
      |]
      Unix.stdin devnull devnull
  in
  let _, status = Unix.waitpid [] pid in
  Unix.close devnull;
  assert_bool
    (match status with Unix.WEXITED 0 -> false | _ -> true)
    ~context:"requirepass+replicaof should fail fast"

let test_auth_enabled_master_allows_replica_replication () =
  with_server ~requirepass:"secret"
    (fun ~port:master_port ~connect:connect_master ->
      with_server ~replicaof:("127.0.0.1", master_port)
        (fun ~port:_ ~connect:connect_replica ->
          let master_auth = connect_master () in
          let master_unauth = connect_master () in
          let replica = connect_replica () in
          Fun.protect
            ~finally:(fun () ->
              Unix.close master_auth;
              Unix.close master_unauth;
              Unix.close replica)
            (fun () ->
              assert_noauth
                ~actual:
                  (send_command master_unauth [ "SET"; "city"; "Toronto" ])
                ~context:"normal unauthenticated client still gated";
              ignore
                (retry_until 5.0 (fun () ->
                     let reply =
                       send_command replica [ "INFO"; "replication" ]
                     in
                     if string_contains reply "master_link_status:up" then
                       Some reply
                     else None));
              assert_string_equal ~expected:"+OK\r\n"
                ~actual:(send_command master_auth [ "AUTH"; "secret" ])
                ~context:"authenticate writing client";
              ignore (send_command master_auth [ "SET"; "city"; "Toronto" ]);
              let replicated =
                retry_until 5.0 (fun () ->
                    let reply = send_command replica [ "GET"; "city" ] in
                    if reply = "$7\r\nToronto\r\n" then Some reply else None)
              in
              assert_string_equal ~expected:"$7\r\nToronto\r\n"
                ~actual:replicated
                ~context:"replica still syncs from auth-enabled master")))

let test_auth_and_acl_subset () =
  with_server ~requirepass:"secret" (fun ~port:_ ~connect ->
      let client = connect () in
      let other = connect () in
      Fun.protect
        ~finally:(fun () ->
          Unix.close client;
          Unix.close other)
        (fun () ->
          assert_string_equal ~expected:noauth_reply
            ~actual:(send_command client [ "GET"; "key" ])
            ~context:"pre-auth get denied";
          assert_string_equal ~expected:"+PONG\r\n"
            ~actual:(send_command client [ "PING" ])
            ~context:"pre-auth ping allowed";
          assert_string_equal
            ~expected:
              "-WRONGPASS invalid username-password pair or user is disabled.\r\n"
            ~actual:(send_command client [ "AUTH"; "wrong" ])
            ~context:"wrong password";
          assert_string_equal ~expected:"+OK\r\n"
            ~actual:(send_command client [ "AUTH"; "secret" ])
            ~context:"correct password";
          assert_string_equal ~expected:"$7\r\ndefault\r\n"
            ~actual:(send_command client [ "ACL"; "WHOAMI" ])
            ~context:"acl whoami";
          assert_string_equal
            ~expected:
              "*4\r\n\
               $5\r\n\
               flags\r\n\
               *1\r\n\
               $2\r\n\
               on\r\n\
               $9\r\n\
               passwords\r\n\
               *1\r\n\
               $6\r\n\
               secret\r\n"
            ~actual:(send_command client [ "ACL"; "GETUSER"; "default" ])
            ~context:"acl getuser default exact shape";
          assert_string_equal ~expected:"$-1\r\n"
            ~actual:(send_command client [ "ACL"; "GETUSER"; "other" ])
            ~context:"acl getuser other";
          assert_string_equal ~expected:noauth_reply
            ~actual:(send_command other [ "SET"; "k"; "v" ])
            ~context:"other client still unauthenticated";
          assert_string_equal ~expected:"+OK\r\n"
            ~actual:(send_command client [ "SET"; "k"; "v" ])
            ~context:"authenticated client can write";
          assert_string_equal ~expected:"+OK\r\n"
            ~actual:(send_command client [ "AUTH"; "secret" ])
            ~context:"re-auth works predictably"))

let test_auth_nopass_acl_shape () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal ~expected:"+OK\r\n"
            ~actual:(send_command client [ "SET"; "k"; "v" ])
            ~context:"no requirepass means no auth gate";
          assert_string_equal
            ~expected:
              "*4\r\n\
               $5\r\n\
               flags\r\n\
               *2\r\n\
               $2\r\n\
               on\r\n\
               $6\r\n\
               nopass\r\n\
               $9\r\n\
               passwords\r\n\
               *0\r\n"
            ~actual:(send_command client [ "ACL"; "GETUSER"; "default" ])
            ~context:"nopass acl getuser shape"))

let test_auth_precedence_and_pubsub_interaction () =
  with_server ~requirepass:"secret" (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal
            ~expected:"-ERR wrong number of arguments for 'get' command\r\n"
            ~actual:(send_command client [ "GET" ])
            ~context:"arity error before noauth";
          assert_string_equal ~expected:"+OK\r\n"
            ~actual:(send_command client [ "AUTH"; "secret" ])
            ~context:"auth before subscribe";
          assert_string_equal
            ~expected:"*3\r\n$9\r\nsubscribe\r\n$6\r\nalerts\r\n:1\r\n"
            ~actual:(send_command client [ "SUBSCRIBE"; "alerts" ])
            ~context:"subscribe after auth";
          assert_string_equal ~expected:"*2\r\n$4\r\npong\r\n$2\r\nhi\r\n"
            ~actual:(send_command client [ "PING"; "hi" ])
            ~context:"ping still works in subscribed mode after auth"))

let test_transaction_edge_shapes () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal ~expected:"-ERR EXEC without MULTI\r\n"
            ~actual:(send_command client [ "EXEC" ])
            ~context:"exec outside multi";
          assert_string_equal ~expected:"-ERR DISCARD without MULTI\r\n"
            ~actual:(send_command client [ "DISCARD" ])
            ~context:"discard outside multi";
          assert_string_equal ~expected:"+OK\r\n"
            ~actual:(send_command client [ "UNWATCH" ])
            ~context:"unwatch outside watch returns ok"))

let test_watch_multiple_keys_and_clear_behavior () =
  with_server (fun ~port:_ ~connect ->
      let watcher = connect () in
      let modifier = connect () in
      Fun.protect
        ~finally:(fun () ->
          Unix.close watcher;
          Unix.close modifier)
        (fun () ->
          ignore (send_command watcher [ "WATCH"; "a"; "b" ]);
          ignore (send_command watcher [ "MULTI" ]);
          ignore (send_command watcher [ "SET"; "a"; "watcher" ]);
          ignore (send_command modifier [ "SET"; "b"; "modifier" ]);
          assert_string_equal ~expected:"*-1\r\n"
            ~actual:(send_command watcher [ "EXEC" ])
            ~context:"watching multiple keys aborts when one changes";
          ignore (send_command watcher [ "WATCH"; "a" ]);
          ignore (send_command watcher [ "MULTI" ]);
          ignore (send_command watcher [ "SET"; "a"; "discarded" ]);
          assert_string_equal ~expected:"+OK\r\n"
            ~actual:(send_command watcher [ "DISCARD" ])
            ~context:"discard returns ok";
          ignore (send_command modifier [ "SET"; "a"; "after-discard" ]);
          ignore (send_command watcher [ "MULTI" ]);
          ignore (send_command watcher [ "SET"; "a"; "committed" ]);
          assert_string_equal ~expected:"*1\r\n+OK\r\n"
            ~actual:(send_command watcher [ "EXEC" ])
            ~context:"discard cleared watched keys before next transaction";
          ignore (send_command watcher [ "WATCH"; "a" ]);
          ignore (send_command watcher [ "MULTI" ]);
          ignore (send_command watcher [ "SET"; "a"; "final" ]);
          assert_string_equal ~expected:"*1\r\n+OK\r\n"
            ~actual:(send_command watcher [ "EXEC" ])
            ~context:"exec clears watched keys after success";
          ignore (send_command modifier [ "SET"; "a"; "post-exec" ]);
          ignore (send_command watcher [ "MULTI" ]);
          ignore (send_command watcher [ "SET"; "a"; "fresh" ]);
          assert_string_equal ~expected:"*1\r\n+OK\r\n"
            ~actual:(send_command watcher [ "EXEC" ])
            ~context:"post-exec transaction is unaffected by old watch state"))

let test_wait_zero_and_subscribed_mode_edges () =
  with_server (fun ~port:master_port ~connect:connect_master ->
      with_server ~replicaof:("127.0.0.1", master_port)
        (fun ~port:_ ~connect:connect_replica ->
          let master = connect_master () in
          let subscriber = connect_master () in
          let replica = connect_replica () in
          Fun.protect
            ~finally:(fun () ->
              Unix.close master;
              Unix.close subscriber;
              Unix.close replica)
            (fun () ->
              ignore
                (retry_until 5.0 (fun () ->
                     let reply =
                       send_command replica [ "INFO"; "replication" ]
                     in
                     if string_contains reply "master_link_status:up" then
                       Some reply
                     else None));
              assert_string_equal ~expected:":1\r\n"
                ~actual:(send_command master [ "WAIT"; "0"; "1000" ])
                ~context:
                  "wait zero replicas returns current connected replica count";
              ignore (send_command subscriber [ "SUBSCRIBE"; "alerts" ]);
              assert_string_equal ~expected:subscribed_mode_only_reply
                ~actual:(send_command subscriber [ "WAIT"; "1"; "1000" ])
                ~context:
                  "wait in subscribed mode uses subscribed-mode restriction")))

let test_sorted_set_reply_shape_edges () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          ignore (send_command client [ "ZADD"; "scores"; "1"; "alpha" ]);
          assert_string_equal ~expected:"$-1\r\n"
            ~actual:(send_command client [ "ZSCORE"; "scores"; "missing" ])
            ~context:"zscore missing member returns nil";
          assert_string_equal ~expected:"$-1\r\n"
            ~actual:(send_command client [ "ZSCORE"; "missing"; "alpha" ])
            ~context:"zscore missing key returns nil";
          assert_string_equal ~expected:"*0\r\n"
            ~actual:(send_command client [ "ZRANGE"; "scores"; "5"; "1" ])
            ~context:"zrange start greater than stop returns empty array";
          assert_string_equal ~expected:"*0\r\n"
            ~actual:(send_command client [ "ZRANGE"; "scores"; "100"; "200" ])
            ~context:"zrange far out of range returns empty array"))

let test_geodist_reply_shape_edges () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          ignore
            (send_command client
               [ "GEOADD"; "places"; "0"; "0"; "origin"; "0"; "1"; "north" ]);
          let dist_m =
            parse_bulk_float_reply
              (send_command client [ "GEODIST"; "places"; "origin"; "north" ])
          in
          assert_bool
            (dist_m > 111000. && dist_m < 112000.)
            ~context:"geodist default unit is meters";
          assert_string_equal ~expected:"$-1\r\n"
            ~actual:
              (send_command client [ "GEODIST"; "places"; "origin"; "missing" ])
            ~context:"geodist missing member returns nil"))

let test_timeout_error_wording () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal
            ~expected:"-ERR invalid timeout in command: -1\r\n"
            ~actual:(send_command client [ "BLPOP"; "queue"; "-1" ])
            ~context:"blpop negative timeout wording";
          assert_string_equal
            ~expected:"-ERR invalid timeout in command: -1\r\n"
            ~actual:
              (send_command client
                 [ "XREAD"; "BLOCK"; "-1"; "STREAMS"; "stream"; "0-0" ])
            ~context:"xread negative block wording";
          assert_string_equal
            ~expected:"-ERR invalid timeout in command: -1\r\n"
            ~actual:(send_command client [ "WAIT"; "1"; "-1" ])
            ~context:"wait negative timeout wording";
          assert_string_equal
            ~expected:"-ERR invalid timeout in command: -2\r\n"
            ~actual:(send_command client [ "WAIT"; "-2"; "1" ])
            ~context:"wait negative replica count wording"))

let test_server_errors () =
  with_server (fun ~port:_ ~connect ->
      let client = connect () in
      Fun.protect
        ~finally:(fun () -> Unix.close client)
        (fun () ->
          assert_string_equal
            ~expected:"-ERR wrong number of arguments for 'get' command\r\n"
            ~actual:(send_command client [ "GET" ])
            ~context:"wrong arity";
          let malformed = "*1\r\n+PING\r\n" in
          let written =
            Unix.write_substring client malformed 0 (String.length malformed)
          in
          assert_int64_equal
            ~expected:(Int64.of_int (String.length malformed))
            ~actual:(Int64.of_int written) ~context:"malformed payload write";
          let reply = read_reply client in
          assert_bool
            (String.starts_with ~prefix:"-ERR protocol error:" reply)
            ~context:"protocol error prefix"))

let tests =
  [
    ("manual clock starts at configured instant", test_manual_clock_initial);
    ( "manual advance changes observed time predictably",
      test_manual_clock_advance );
    ("manual set overrides time exactly", test_manual_clock_set);
    ( "expiry logic can be driven without real sleeps",
      test_expiry_without_real_sleeps );
    ("engine set/get honors expiry deterministically", test_engine_expiry);
    ("engine supports basic list operations", test_engine_list_operations);
    ("engine supports sorted set operations", test_engine_sorted_set_operations);
    ("server responds to ping and repeated pings", test_server_ping);
    ("server echoes payloads", test_server_echo);
    ( "server stores and retrieves values across clients",
      test_server_shared_store_across_clients );
    ("server supports blocking list pop", test_server_blocking_list_pop);
    ("server blocking list pop times out", test_server_blocking_list_pop_timeout);
    ("server increments string counters", test_server_incr);
    ("server commits transactions", test_server_transactions_commit);
    ("server handles empty transactions", test_server_empty_transaction);
    ("server discards queued transactions", test_server_discard);
    ( "server reports runtime errors inside exec",
      test_server_transaction_runtime_error );
    ( "server aborts watched transactions on change",
      test_server_watch_abort_on_change );
    ( "server unwatch clears optimistic lock",
      test_server_unwatch_clears_abort_condition );
    ("server rejects watch inside multi", test_server_watch_inside_multi_errors);
    ("server reports key types", test_server_type_command);
    ("server supports sorted set commands", test_server_sorted_set_commands);
    ( "server rejects invalid sorted set scores and wrong types",
      test_server_sorted_set_wrong_type_and_invalid_score );
    ( "server adds stream entries and queries ranges",
      test_server_xadd_and_xrange );
    ( "server rejects non-increasing stream ids",
      test_server_xadd_rejects_non_increasing_ids );
    ( "server auto-generates stream ids",
      test_server_fully_auto_generated_stream_ids );
    ("server reads a single stream", test_server_xread_single_stream);
    ("server reads multiple streams", test_server_xread_multiple_streams);
    ( "server blocks xread using dollar semantics",
      test_server_blocking_xread_uses_dollar );
    ("server times out blocking xread", test_server_blocking_xread_timeout);
    ( "server treats XREAD BLOCK 0 as indefinite wait",
      test_server_blocking_xread_zero_blocks_until_entry );
    ("master reports replication info", test_replication_info_master);
    ("master speaks replication handshake", test_replication_handshake_protocol);
    ("master propagates to one replica", test_single_replica_propagation);
    ("master propagates to multiple replicas", test_multi_replica_propagation);
    ("wait returns zero with no replicas", test_wait_with_no_replicas);
    ("wait works with no prior commands", test_wait_with_no_commands);
    ( "wait tracks acknowledgements after multiple commands",
      test_wait_after_multiple_commands );
    ("server loads a single key from rdb", test_rdb_load_single_key);
    ("server loads multiple keys from rdb", test_rdb_load_multiple_keys);
    ("server loads rdb expiries", test_rdb_load_expiry);
    ( "pubsub subscribe supports ping in subscribed mode",
      test_pubsub_subscribe_and_ping );
    ( "pubsub subscribes to multiple channels",
      test_pubsub_subscribe_multiple_channels );
    ( "pubsub publish delivers and unsubscribe removes subscriber",
      test_pubsub_publish_and_unsubscribe );
    ( "pubsub unsubscribe-all and subscribed ping edges are compatible",
      test_pubsub_unsubscribe_all_and_ping_edges );
    ("server supports geospatial commands", test_server_geo_commands);
    ( "server enforces geospatial validation and wrong-type boundaries",
      test_server_geo_wrong_type_and_validation );
    ("server wrong-type matrix smoke test", test_server_wrong_type_matrix_smoke);
    ( "server missing-key reply shapes are consistent",
      test_server_missing_key_reply_shapes );
    ( "auth rejects replica mode at startup",
      test_auth_startup_rejects_replica_mode );
    ( "auth-enabled master still allows replica sync",
      test_auth_enabled_master_allows_replica_replication );
    ("auth and acl subset works", test_auth_and_acl_subset);
    ("nopass acl shape is correct", test_auth_nopass_acl_shape);
    ( "auth precedence and subscribed-mode interaction hold",
      test_auth_precedence_and_pubsub_interaction );
    ("auth gate matrix is enforced", test_auth_gate_matrix);
    ("startup config validation bundle", test_startup_config_validation_bundle);
    ( "replication propagates zset and geo writes",
      test_replication_propagates_zset_and_geo_writes );
    ("timeout error wording is normalized", test_timeout_error_wording);
    ("transaction edge shapes are compatible", test_transaction_edge_shapes);
    ( "wait zero/subscribed-mode edges are compatible",
      test_wait_zero_and_subscribed_mode_edges );
    ( "watch multiple-key and clear semantics are compatible",
      test_watch_multiple_keys_and_clear_behavior );
    ( "sorted-set reply-shape edges are compatible",
      test_sorted_set_reply_shape_edges );
    ("geodist reply-shape edges are compatible", test_geodist_reply_shape_edges);
    ("server returns command errors and protocol errors", test_server_errors);
  ]

let () =
  List.iter
    (fun (name, test_fn) ->
      test_fn ();
      Printf.printf "PASS: %s\n%!" name)
    tests
