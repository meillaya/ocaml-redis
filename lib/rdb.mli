type entry = { key : string; value : string; expires_at_ms : int64 option }

val load_file : string -> (entry list, string) result
