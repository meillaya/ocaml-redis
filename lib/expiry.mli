val is_expired : now_ms:int64 -> expires_at_ms:int64 -> bool
val remaining_ms : now_ms:int64 -> expires_at_ms:int64 -> int64 option
