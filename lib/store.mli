type t
type wrong_type = [ `Wrong_type ]
type incr_error = [ wrong_type | `Not_an_integer | `Overflow ]
type 'a lookup = [ `Missing | `Value of 'a | wrong_type ]
type key_type = [ `None | `String | `List | `Stream | `Zset ]
type geo_lookup = (float * float) option

type geosearch_error =
  [ wrong_type | `Invalid_center of float * float | `Invalid_radius of float ]

type stream_id
type stream_entry = { id : stream_id; fields : (string * string) list }

type stream_write_error =
  [ wrong_type | `Invalid_stream_id of string | `Id_not_greater | `Id_too_small ]

type stream_read_error = [ wrong_type | `Invalid_stream_id of string ]

val create : unit -> t
val clear : t -> unit
val memory_usage : t -> int64
val key_type : now_ms:int64 -> t -> string -> key_type
val watch_version : now_ms:int64 -> t -> string -> int64
val get_string : now_ms:int64 -> t -> string -> string lookup

val set_string :
  t -> key:string -> value:string -> expires_at_ms:int64 option -> unit

val incr_string : now_ms:int64 -> t -> key:string -> (int64, incr_error) result

val rpush :
  now_ms:int64 -> t -> key:string -> string list -> (int64, wrong_type) result

val lpush :
  now_ms:int64 -> t -> key:string -> string list -> (int64, wrong_type) result

val lrange :
  now_ms:int64 ->
  t ->
  key:string ->
  int64 ->
  int64 ->
  (string list, wrong_type) result

val llen : now_ms:int64 -> t -> key:string -> (int64, wrong_type) result

val lrem :
  now_ms:int64 ->
  t ->
  key:string ->
  int64 ->
  string ->
  (int64, wrong_type) result

val lpop : now_ms:int64 -> t -> key:string -> (string option, wrong_type) result

val zadd :
  now_ms:int64 ->
  t ->
  key:string ->
  (float * string) list ->
  (int64, wrong_type) result

val zrank :
  now_ms:int64 ->
  t ->
  key:string ->
  member:string ->
  (int64 option, wrong_type) result

val zrange :
  now_ms:int64 ->
  t ->
  key:string ->
  int64 ->
  int64 ->
  (string list, wrong_type) result

val zcard : now_ms:int64 -> t -> key:string -> (int64, wrong_type) result

val zscore :
  now_ms:int64 ->
  t ->
  key:string ->
  member:string ->
  (float option, wrong_type) result

val zrem :
  now_ms:int64 -> t -> key:string -> string list -> (int64, wrong_type) result

val geoadd :
  now_ms:int64 ->
  t ->
  key:string ->
  (float * float * string) list ->
  (int64, [ wrong_type | `Invalid_coordinates of string ]) result

val geopos :
  now_ms:int64 ->
  t ->
  key:string ->
  string list ->
  (geo_lookup list, wrong_type) result

val geodist :
  now_ms:int64 ->
  t ->
  key:string ->
  member1:string ->
  member2:string ->
  (float option, wrong_type) result

val geosearch :
  now_ms:int64 ->
  t ->
  key:string ->
  longitude:float ->
  latitude:float ->
  radius_meters:float ->
  (string list, geosearch_error) result

val stream_id_to_string : stream_id -> string

val resolve_xread_id :
  now_ms:int64 ->
  t ->
  key:string ->
  string ->
  (string, stream_read_error) result

val xadd :
  now_ms:int64 ->
  t ->
  key:string ->
  id_spec:string ->
  (string * string) list ->
  (stream_id, stream_write_error) result

val xrange :
  now_ms:int64 ->
  t ->
  key:string ->
  start_spec:string ->
  end_spec:string ->
  (stream_entry list, stream_read_error) result

val xread :
  now_ms:int64 ->
  t ->
  queries:(string * string) list ->
  ((string * stream_entry list) list, stream_read_error) result
