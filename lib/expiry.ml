let is_expired ~now_ms ~expires_at_ms = Int64.compare now_ms expires_at_ms >= 0

let remaining_ms ~now_ms ~expires_at_ms =
  if is_expired ~now_ms ~expires_at_ms then None
  else Some (Int64.sub expires_at_ms now_ms)

let add_saturating left right =
  if
    Int64.compare right 0L > 0
    && Int64.compare left (Int64.sub Int64.max_int right) > 0
  then Int64.max_int
  else Int64.add left right
