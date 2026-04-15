val run :
  port:int ->
  replicaof:(string * int) option ->
  rdb_file:string option ->
  requirepass:string option ->
  unit
