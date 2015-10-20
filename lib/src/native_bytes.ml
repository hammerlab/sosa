
open Sosa_pervasives

module N = Functors.Make_native(struct
  include BytesLabels
  let of_buffer = Buffer.to_bytes
  (* Would this be an appropriate place for Bytes.unsafe_to_string?*)
  let string_for_output = to_string
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

let to_native_string x = BytesLabels.to_string x
let of_native_string x = return (BytesLabels.of_string x)
let of_native_substring x ~offset ~length =
  if length = 0 then return BytesLabels.empty
  else
    try return (StringLabels.sub x ~pos:offset ~len:length
                |> BytesLabels.of_string)
    with e -> fail `out_of_bounds

let to_string_hum x = Printf.sprintf "%S" (Bytes.to_string x)
