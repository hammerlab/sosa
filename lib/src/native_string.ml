(** Richer interface to the built in [string] type. *)

open Sosa_pervasives
open Printf
open Api
include StringLabels

module F = Functors

type character = char

let empty = ""
let is_empty t = (compare "" t = 0)

let of_character = make 1
let of_character_list cl =
  let length = List.length cl in
  let buf = String.make length '\x00' in
  List.iteri cl ~f:(fun i c -> buf.[i] <- c);
  buf

let to_character_list s =
  let res = ref [] in
  for i = length s - 1 downto 0 do
    res := s.[i] :: !res
  done;
  !res

let get s ~index =
  try Some (s.[index])
  with _ -> None

let set s ~index ~v =
  if index > String.length s - 1
  then None
  else begin
    let cop = String.copy s in
    cop.[index] <- v;
    Some cop
  end
let get_exn s ~index = s.[index]
let set_exn s ~index ~v =
  match set s ~index ~v with None -> failwith "set_exn" | Some s -> s

let compare_substring (a, idxa, lena) (b, idxb, lenb) =
  let module With_exns = struct
    exception Return of int
    exception Left_out of int
    exception Right_out of int
    let f () =
      try
        let shortest = min lena lenb in
        for i = 0 to shortest - 1 do
          let ca = try a.[idxa + i] with _ -> raise (Left_out i) in
          let cb = try b.[idxb + i] with _ -> raise (Right_out i) in
          let c = Char.compare  ca cb in
          if c <> 0
          then raise (Return c)
          else ()
        done;
        (Pervasives.compare (lena : int) lenb)
      with
      | Return c -> c
      | Left_out c -> (* a went out of bounds at 'c + idxa' *) -1
      | Right_out _ -> (* b went out of bounds at 'c + idxb' *)
        (* so, a is “longer” *) 1
  end in
  With_exns.f ()

type s = t
module T_length_and_compsub = struct
  type t = s let length = length let compare_substring = compare_substring
end
include F.Compare_substring_strict_of_loose(T_length_and_compsub)
include F.Make_index_of_string(T_length_and_compsub)


let to_native_string x = String.copy x
let of_native_string x = return (String.copy x)
let of_native_substring x ~offset ~length =
  if length = 0 then return ""
  else
    try return (String.sub x offset length)
    with e -> fail `out_of_bounds

let to_string_hum x = sprintf "%S" x

let concat ?(sep="") sl = concat ~sep sl

let fold t ~init ~f =
  let res = ref init in
  for i = 0 to String.length t - 1 do
    res := f !res t.[i];
  done;
  !res

let foldi t ~init ~f =
  let res = ref init in
  for i = 0 to String.length t - 1 do
    res := f i !res t.[i];
  done;
  !res

let fold2_exn t1 t2 ~init ~f =
  let lgth1 = (length t1) in
  let lgth2 = (length t2) in
  match lgth1, lgth2 with
  | 0, 0 -> init
  | _, _ when lgth1 <> lgth2 -> failwith "fold2_exn"
  | lgth1, lgth2 ->
      let res = ref init in
      for i = 0 to lgth1 - 1 do
        res := f !res t1.[i] t2.[i];
      done;
      !res

let sub_exn t ~index ~length =
  if length = 0 then empty else String.sub t index length

let sub t ~index ~length =
  if length = 0 then Some empty else
    try Some (String.sub t index length)
    with e -> None

let slice_exn ?(start=0) ?finish t =
  let length_of_t = String.length t in
  let bound_check strict m x =
    let out_of_ub = if strict then x > length_of_t else x >= length_of_t in
    if x < 0 || (not (is_empty t) && out_of_ub) then
      ksprintf failwith "slice_exn: invalid %s %d" m x
    else x
  in
  let _      = bound_check false "start" start
  and finish =
    match finish with
    | None   -> length_of_t
    | Some f -> bound_check true "finish" f
  in
  sub_exn t ~index:start ~length:(finish - start)

let slice ?start ?finish t =
  try Some (slice_exn ?start ?finish t)
  with _ -> None

let mutate_exn t ~index c = String.set t index c

let mutate t ~index c =
  try String.set t index c; return () with _ -> fail `out_of_bounds

let blit_exn ~src ~src_index ~dst ~dst_index ~length =
  blit ~src ~src_pos:src_index ~dst ~dst_pos:dst_index ~len:length

let blit ~src ~src_index ~dst ~dst_index ~length =
  try blit_exn ~src ~src_index ~dst ~dst_index ~length; return ()
  with _ -> fail `out_of_bounds

let iter t ~f = String.iter t ~f
let iteri t ~f = String.iteri t ~f
let iter_reverse t ~f =
  for i = length t -1 downto 0 do
    f (get_exn t i)
  done

let rev t =
  let lgth = length t in
  match lgth with
  | 0 -> empty
  | lgth ->
      let res = make lgth (String.get t 0) in
      for i = 0 to lgth - 1 do
        String.set res i (String.get t (lgth - 1 - i))
      done;
      res

let map t ~f = String.map t ~f

let map2_exn t1 t2 ~f =
  let lgth1 = (length t1) in
  let lgth2 = (length t2) in
  match lgth1, lgth2 with
  | 0, 0 -> empty
  | _, _ when lgth1 <> lgth2 -> failwith "map2_exn"
  | lgth1, lgth2 ->
      let res = make lgth1 (String.get t1 0) in
      for i = 0 to lgth1 - 1 do
        String.set res i (f (String.get t1 i) (String.get t2 i))
      done;
      res

let mapi t ~f =
  let buffer = String.create (String.length t) in
  let ()     = String.iteri t ~f:(fun i c -> String.set buffer i (f i c)) in
  buffer
(* TODO: Change this to 
  let mapi t ~f = String.mapi t ~f
  once we switch to 4.02 *)

let for_all t ~f =
  try (iter t (fun x -> if not (f x) then raise Not_found else ()); true)
  with Not_found -> false

let exists t ~f =
  try (iter t (fun x -> if f x then raise Not_found else ()); false)
  with Not_found -> true

let index_of_character t ?(from=0) c =
  let from = if from <= 0 then 0 else min (length t) from in
  try Some (String.index_from t from c)
  with _ -> None

let index_of_character_reverse t ?from c =
  let from =
    let length_of_t = length t in
    match from with
    | None -> length_of_t - 1
    | Some s when s < 0 -> -1
    | Some s when s > length_of_t - 1 -> length_of_t - 1
    | Some s -> s
  in
  try Some (String.rindex_from t from c)
  with _ -> None

let resize_from_length ~from ?length ~length_of_s =
  let from = if from <= 0 then 0 else min length_of_s from in
  let length =
    match length with
    | None -> length_of_s - from
    | Some lg when lg <= 0 -> 0
    | Some lg -> min (length_of_s - from) lg
  in
  (from, length)

let find ?(from=0) ?length s ~f =
  let length_of_s = String.length s in
  let from, length = resize_from_length ~from ?length ~length_of_s in
  let found = ref None in
  let i = ref 0 in
  while !found = None && !i  < length do
    if f (get_exn s (!i + from))
    then found := Some (!i + from)
    else incr i
  done;
  !found

let find_reverse ?from ?length s ~f =
  let length_of_s = String.length s in
  if length_of_s = 0 then None
  else begin
    let from = 
      match from with
      | None -> length_of_s - 1 
      | Some s when s < 0 -> -1
      | Some s when s >= length_of_s - 1 -> length_of_s - 1
      | Some s -> s
    in
    let length =
      match length with
      | None -> from + 1
      | Some l when l <= 0 -> 0
      | Some l when l >= from + 1 -> from + 1
      | Some l -> l
    in
    let found = ref None in
    let i = ref from in
    while !found = None && !i >= from - length + 1 do
      (* dbg "i: %d from: %d length: %d" !i from length; *)
      if f (get_exn s !i)
      then found := Some (!i)
      else decr i
    done;
    !found
  end

let filter_map ?(from=0) ?length s ~f =
  let length_of_s = String.length s in
  let from, length = resize_from_length ~from ?length ~length_of_s in
  if length = 0 then empty
  else begin
    let b = Buffer.create length in
    for i = 0 to length - 1 do
      match f (get_exn s (i + from)) with
      | Some c -> Buffer.add_char b c
      | None -> ()
    done;
    Buffer.contents b
  end

let filter ?from ?length s ~f =
    filter_map ?from ?length s ~f:(fun c -> if f c then Some c else None)

include F.Make_strip_function (struct
    type t = string
    type character = char
    let empty = empty
    let length = length
    let sub_exn = sub_exn
    let find = find
    let find_reverse = find_reverse
    let is_whitespace = Native_character.is_whitespace
  end)

include F.Make_split_function(struct
    type t = string
    type character = char
    let length = length
    let sub_exn = sub_exn
    let index_of_string = index_of_string
    let index_of_character = index_of_character
  end)


include F.Make_prefix_suffix_array (struct
    type t = string
    type character = char
    let length = length
    let get = (fun s i -> s.[i])
    let sub_exn = sub_exn
  end)

include F.Make_split_at_index_functions(struct
    type t = string
    type character = char
    let empty = empty
    let length = length
    let sub_exn = sub_exn
  end)

module Make_output (Model: OUTPUT_MODEL) = Model

let take_while_with_index t ~f =
  let buf = Buffer.create (length t) in
  let rec loop idx =
    match get t idx with
    | Some c when f idx c -> Buffer.add_char buf c; loop (idx + 1)
    | _ -> ()
  in
  loop 0;
  Buffer.contents buf

let take_while t ~f = take_while_with_index t ~f:(fun _ c -> f c)

