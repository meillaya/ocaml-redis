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

type error

val parse : string list -> (t, error) result
val to_error_message : error -> string
val to_arguments : t -> string list option
val is_write : t -> bool
