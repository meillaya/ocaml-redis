type blocked =
  | List_pop of { key : string; timeout_ms : int64 option }
  | Stream_read of {
      queries : (string * string) list;
      timeout_ms : int64 option;
    }

type outcome = {
  reply : Resp.reply option;
  block : blocked option;
  wake_keys : string list;
}

type t

val create : unit -> t

val load_string :
  t -> key:string -> value:string -> expires_at_ms:int64 option -> unit

val watch_version : now_ms:int64 -> t -> string -> int64
val handle : now_ms:int64 -> t -> Command.t -> outcome
val wake_blpop : now_ms:int64 -> t -> string -> (string * string) option

val wake_xread :
  now_ms:int64 -> t -> queries:(string * string) list -> Resp.reply option
