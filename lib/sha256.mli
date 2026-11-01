val digest : string -> bytes
(** SHA-256 digest of [message]. *)

val hex : string -> string
(** Lowercase hexadecimal SHA-256 digest, the form Redis reports in
    [ACL GETUSER]. *)
