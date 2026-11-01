type entry = { key : string; value : string; expires_at_ms : int64 option }

val parse : string -> (entry list, string) result
(** Parse an RDB payload already in memory. Only the documented string subset is
    supported; unsupported value types fail with an error. *)

val load_file : string -> (entry list, string) result
