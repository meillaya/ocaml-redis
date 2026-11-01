val is_expired : now_ms:int64 -> expires_at_ms:int64 -> bool
val remaining_ms : now_ms:int64 -> expires_at_ms:int64 -> int64 option

val add_saturating : int64 -> int64 -> int64
(** Add milliseconds without wrapping past [Int64.max_int]. *)
