
open Sosa_pervasives
open Printf

type t = char

let of_native_char x = Some x
let of_int x =
  try Some (char_of_int x) with _ -> None
let to_int = int_of_char
let compare = Char.compare

let size _ = 1

let is_print t = ' ' <= t && t <= '~'
let to_native_bytes x = Bytes.make 1 x
let to_string_hum x =
  if is_print x then String.make 1 x
  else sprintf "0x%2x" (int_of_char x)

let write_to_native_bytes c ~buf ~index =
  try Bytes.set buf index c; return 1
  with _ -> fail `out_of_bounds

let read_from_native_string ~buf ~index =
  try Some (String.get buf index, 1)
  with _ -> None

let is_whitespace =
  function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

