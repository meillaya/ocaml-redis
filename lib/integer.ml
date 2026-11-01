(* Strict decimal integer parsing.

   [Int64.of_string_opt] accepts forms Redis never accepts on the wire, such as
   ["0x10"], ["1_000"], and ["+7"]. Command arguments and stored counters must
   use canonical decimal notation, so every numeric command argument and every
   INCR operand goes through this module instead. Leading zeroes are accepted
   because Redis accepts them. *)

let of_string_opt value =
  let length = String.length value in
  if length = 0 then None
  else
    let first_digit = if value.[0] = '-' then 1 else 0 in
    if first_digit = length then None
    else
      let rec digits_only index =
        index = length
        ||
        let character = value.[index] in
        character >= '0' && character <= '9' && digits_only (index + 1)
      in
      if not (digits_only first_digit) then None else Int64.of_string_opt value

let to_int_opt value =
  match of_string_opt value with
  | Some parsed
    when Int64.compare parsed (Int64.of_int min_int) >= 0
         && Int64.compare parsed (Int64.of_int max_int) <= 0 ->
      Some (Int64.to_int parsed)
  | _ -> None
