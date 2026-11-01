type reply =
  | Simple_string of string
  | Error of string
  | Prefixed_error of { prefix : string; message : string }
  | Integer of int64
  | Bulk_string of string option
  | Array of reply list
  | Null_array

type frame =
  | Simple_string_frame of string
  | Error_frame of string
  | Bulk_string_frame of string option
  | Command_frame of string list

val max_bulk_string_length : int
val max_array_length : int

val encode_reply : reply -> string
(** Error replies have embedded CR/LF flattened and are capped at 512 bytes so a
    hostile argument cannot inject extra protocol frames. *)

val sanitize_error_message : string -> string
val encode_command : string list -> string
val parse_request : string -> (string list * int, string) result option
val parse_frame : string -> (frame * int, string) result option
