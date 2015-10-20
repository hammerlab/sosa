
open Sosa_pervasives

module N = Functors.Make_native(struct
  include StringLabels
  let of_buffer = Buffer.contents
  let empty = ""
  let string_for_output s = s
  end)

include N

let to_native_string x = x
let of_native_string x = return x
let of_native_substring x ~offset ~length =
  if length = 0 then return ""
  else
    try return (StringLabels.sub x ~pos:offset ~len:length)
    with e -> fail `out_of_bounds

let to_string_hum x = Printf.sprintf "%S" x
