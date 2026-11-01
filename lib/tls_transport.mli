type server
type t
type feed_result = { plaintext : string; response : string; eof : bool }

val server :
  certificate_file:string -> key_file:string -> (server, string) result

val create : server -> t
val handshake_in_progress : t -> bool

val send : t -> string -> (string, string) result
(** Encrypt application data. Before the handshake completes, the data is
    buffered and an empty ciphertext is returned. *)

val feed : t -> string -> (feed_result, string * string) result
(** Consume ciphertext. On success, [plaintext] is newly decrypted application
    data, [response] is ciphertext that must be written back, and [eof] reports
    a peer close notification. On failure, the pair is an error message and the
    alert ciphertext to send before closing. *)

val close_notify : t -> string
(** Ciphertext for a TLS close notification. *)
