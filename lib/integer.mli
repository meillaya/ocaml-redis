val of_string_opt : string -> int64 option
(** Parse a canonical decimal integer. Rejects hexadecimal, octal, binary,
    underscore separators, leading plus signs, whitespace, and empty strings. *)

val to_int_opt : string -> int option
(** Parse a canonical decimal integer and narrow it to an OCaml [int] when it
    fits. *)
