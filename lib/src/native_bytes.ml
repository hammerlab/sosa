
open Sosa_pervasives

module N = Functors.Make_native(struct 
  include BytesLabels
  let of_buffer = Buffer.to_bytes
  let empty = ""
  end)

include N

let mutate_exn t ~index c = BytesLabels.set t index c

let mutate t ~index c =
  try BytesLabels.set t index c; return () with _ -> fail `out_of_bounds

let blit_exn ~src ~src_index ~dst ~dst_index ~length =
  BytesLabels.blit ~src ~src_pos:src_index ~dst ~dst_pos:dst_index ~len:length

let blit ~src ~src_index ~dst ~dst_index ~length =
  try blit_exn ~src ~src_index ~dst ~dst_index ~length; return ()
  with _ -> fail `out_of_bounds

let to_native_string x = BytesLabels.copy x
let of_native_string x = return (BytesLabels.copy x)
let of_native_substring x ~offset ~length =
  if length = 0 then return ""
  else
    try return (BytesLabels.sub x ~pos:offset ~len:length)
    with e -> fail `out_of_bounds

let to_string_hum x = Printf.sprintf "%S" x
