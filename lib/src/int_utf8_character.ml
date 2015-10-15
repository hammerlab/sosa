
open Sosa_pervasives
open Printf

type t = int

let of_native_char x = Some (int_of_char x)

let compare (i: int) (j : int) = compare i j
let of_int x =
  if x land 0x7FFF_FFFF = x then Some x else None
let to_int c = c
let size x =
  if x <=        0x7f then 1 else
  if x <=       0x7ff then 2 else
  if x <=      0xffff then 3 else
  if x <=   0x1f_ffff then 4 else
  if x <=  0x3ff_ffff then 5 else
  if x <= 0x7fff_ffff then 6 else 0

let is_print t = int_of_char ' ' <= t && t <= int_of_char '~'

let to_string_hum x =
  if is_print x then String.make 1 (char_of_int x)
  else sprintf "&#x%X;" x

let write_to_native_bytes c ~buf ~index =
  let sz = size c in
  try
    let first_byte =
      match sz with
      | 1 -> ((c lsr  0) land 0b0111_1111) lor 0b0000_0000
      | 2 -> ((c lsr  6) land 0b0001_1111) lor 0b1100_0000
      | 3 -> ((c lsr 12) land 0b0000_1111) lor 0b1110_0000
      | 4 -> ((c lsr 18) land 0b0000_0111) lor 0b1111_0000
      | 5 -> ((c lsr 24) land 0b0000_0011) lor 0b1111_1000
      | 6 -> ((c lsr 30) land 0b0000_0001) lor 0b1111_1100
      | _ -> assert false in
    Bytes.set buf index (char_of_int first_byte);
    for i = 2 to sz  do
      let ith_byte =
        ((c lsr (6 * (i - 2))) land 0b0011_1111) lor 0b1000_0000 in
      Bytes.set buf (index + sz - i + 1) (char_of_int ith_byte);
    done;
    return sz
  with _ -> fail `out_of_bounds

let read_from_native_string ~buf ~index =
  try
    let first_char = buf.[index] |> int_of_char in
    let size, mask =
      if first_char lsr 7 = 0 then (1, 0b0111_1111)
      else if first_char lsr     5 =     0b110 then (2, 0b0001_1111)
      else if first_char lsr     4 =    0b1110 then (3, 0b0000_1111)
      else if first_char lsr     3 =   0b11110 then (4, 0b0000_0111)
      else if first_char lsr     2 =  0b111110 then (5, 0b0000_0011)
      else if first_char lsr     1 = 0b1111110 then (6, 0b0000_0001)
      else raise Not_found
    in
    let the_int = ref (first_char land mask) in
    for i = 1 to size - 1 do
      let the_char = buf.[index + i] |> int_of_char in
      if (the_char lsr 6) = 0b10
      then (
        the_int := (!the_int lsl 6) lor (the_char land 0b0011_1111);
      ) else raise Not_found;
    done;
    Some (!the_int, size)
  with _ -> None

let to_native_bytes x =
  let buf = Bytes.make (size x) 'B' in
  begin match write_to_native_bytes x ~buf ~index:0 with
  | `Ok _ -> ()
  | `Error e ->
    dbg "buf: %S siz: %d x: %d" (Bytes.to_string buf) (size x) x;
    assert false
  end;
  buf

let is_whitespace c =
  try
    match char_of_int c with
    | ' ' | '\t' | '\r' | '\n' -> true | _ -> false
  with _ -> false

