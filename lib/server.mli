val run :
  port:int ->
  replicaof:(string * int) option ->
  rdb_file:string option ->
  requirepass:string option ->
  masterauth:string option ->
  max_clients:int ->
  max_output_buffer:int ->
  maxmemory:int64 ->
  tls_certificate:string option ->
  tls_key:string option ->
  unit
