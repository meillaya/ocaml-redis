let is_expired ~now_ms ~expires_at_ms = Int64.compare now_ms expires_at_ms >= 0

let remaining_ms ~now_ms ~expires_at_ms =
  if is_expired ~now_ms ~expires_at_ms then None
  else Some (Int64.sub expires_at_ms now_ms)
