(** Components for implementing common logic throughout Sosa's implementation.*)

open Sosa_pervasives
open Printf

(* These modules types are inputs to these not-exposed functors! *)
module type T_LENGTH_AND_COMPSUB = sig
  type t
  val length: t -> int
  val compare_substring: t * int * int -> t * int * int -> int
end (* T_LENGTH_AND_COMPSUB *)

(* This module type is a subset of `BASIC_STRING` for strings with a `length`
   function, a `sub_exn` function, and the `index_of_*` functions *)
module type T_LENGTH_SUB_AND_SEARCH = sig
  type t
  type character
  val length: t -> int
  val sub_exn: t -> index:int -> length:int -> t
  val index_of_character: t -> ?from:int -> character -> int option
  val index_of_string: ?from:int ->
    ?sub_index:int -> ?sub_length:int -> t -> sub:t -> int option
end (* T_LENGTH_SUB_AND_SEARCH *)

(* This functor builds a `compare_substring_strict` function out of a
   `compare_substring` function.

   It may not be the optimal algorithm (it may call `length` on both
   strings.)
 *)
module Compare_substring_strict_of_loose (S: T_LENGTH_AND_COMPSUB) = struct
  open S
  let compare_substring_strict (a, idxa, lena) (b, idxb, lenb) =
    let check_a = lazy (idxa >= 0 && lena >= 0 && idxa + lena <= (length a)) in
    let check_b = lazy (idxb >= 0 && lenb >= 0 && idxb + lenb <= (length b)) in
    if lena = 0 && lenb = 0 then Some 0
    else
      (if lena = 0 then (if Lazy.force check_b then Some (-1) else None)
       else
         (if lenb = 0 then (if Lazy.force check_a then Some (1) else None)
          else
            (if not (Lazy.force check_a) || not (Lazy.force check_b) then None
             else
               Some (compare_substring (a, idxa, lena) (b, idxb, lenb)))))
end

module Make_index_of_string (S: T_LENGTH_AND_COMPSUB) = struct
  open S
  let index_of_string ?(from=0) ?(sub_index=0) ?sub_length t ~sub =
    let module With_exn = struct
      exception Found of int

      let f () =
        (* Readjust the arguments: *)
        let length_of_t = length t in
        let from =
          if from <= 0 then 0 else min length_of_t from in
        let total_length_of_sub = length sub in
        let sub_index =
          if sub_index <= 0 then 0 else sub_index in
        let sub_length =
          let default = max 0 (total_length_of_sub - sub_index) in
          match sub_length with
          | None -> default
          | Some s when s >= default -> default
          | Some s when s < 0 -> 0
          | Some s -> s
        in
        (* dbg "from: %d, length: %d sub_index: %d sub_length: %d" *)
          (* from length_of_t  sub_index sub_length; *)
        if from >= length_of_t then None
        else if length_of_t = 0 then None
        else if sub_length <= 0 then Some from
        else
          begin try
            for i = 0 to length_of_t - from do
              if compare_substring
                  (t, i + from, sub_length)
                  (sub, sub_index, sub_length) = 0
              then raise (Found (i + from))
            done;
            None
          with Found f -> Some f
          end
    end in
    With_exn.f ()

  let index_of_string_reverse ?from ?(sub_index=0) ?sub_length t ~sub =
    let module With_exn = struct
      exception Found of int

      let f () =
        let length_of_t = length t in
        let last = length_of_t - 1 in
        let from =
          match from with
          | None -> last
          | Some f when f >= last -> last
          | Some f -> f in
        let total_length_of_sub = length sub in
        let sub_index =
          if sub_index <= 0 then 0 else sub_index in
        let sub_length =
          let default = max 0 (total_length_of_sub - sub_index) in
          match sub_length with
          | None -> default
          | Some s when s >= default -> default
          | Some s when s < 0 -> 0
          | Some s -> s
        in
        (* dbg "from: %d, length: %d sub_index: %d sub_length: %d" *)
          (* from length_of_t  sub_index sub_length; *)
        if from < 0 then None
        else if length_of_t = 0 then None
        else if sub_length <= 0 then Some from
        else
          begin try
            for i = from downto 0 do
              if compare_substring
                  (t, i, sub_length)
                  (sub, sub_index, sub_length) = 0
              then raise (Found (i))
            done;
            None
          with Found f -> Some f
          end
    end in
    With_exn.f ()

end

(* This functor implements the `BASIC_STRING.split` function out of a
   `T_LENGTH_AND_SEARCH` *)
module Make_split_function (S: T_LENGTH_SUB_AND_SEARCH) = struct

  let split t ~on =
    let length_of_t = S.length t in
    begin match on with
    | `Character c ->
      let rec loop acc from =
        match S.index_of_character t ~from c with
        | Some index ->
          loop (S.sub_exn t ~index:from ~length:(index - from) :: acc)
            (index + 1)
        | None ->
          (S.sub_exn t ~index:from ~length:(length_of_t - from) :: acc)
      in
      List.rev (loop [] 0)
    | `String s ->
      let length_of_s = S.length s in
      let rec loop acc from =
        match S.index_of_string t ~from ~sub:s with
        | Some index ->
          loop (S.sub_exn t ~index:from ~length:(index - from) :: acc)
            (index + length_of_s)
        | None ->
          (S.sub_exn t ~index:from ~length:(length_of_t - from) :: acc)
      in
      if length_of_s > 0
      then List.rev (loop [] 0)
      else if length_of_t = 0
      then [ t ]
      else begin
        let res = ref [] in
        for index = length_of_t - 1 downto 0 do
          res := S.sub_exn t ~index ~length:1 :: !res
        done;
        !res
      end
    end

end (* Make_split_function *)

module Make_strip_function (S:
   sig
     type t
     type character
     val empty : t
     val is_whitespace: character -> bool
     val length: t -> int
     val find:
       ?from:int -> ?length:int -> t -> f:(character -> bool) -> int option
     val find_reverse:
       ?from:int -> ?length:int -> t -> f:(character -> bool) -> int option
     val sub_exn: t -> index:int -> length:int -> t
   end) = struct

  let strip ?(on=`Both) ?(whitespace=S.is_whitespace) t =
    let open S in
    let first_non () =
      match find t ~f:(fun c -> not (whitespace c)) with
      | None -> raise Not_found | Some s -> s in
    let last_non () =
      match find_reverse t ~f:(fun c -> not (whitespace c)) with
      | None -> raise Not_found | Some s -> s in
    try
      match on with
      | `Both ->
        let index = first_non () in
        let last = last_non () in
        sub_exn t ~index ~length:(last - index + 1)
      | `Left ->
        let index = first_non () in
        sub_exn t ~index ~length:(length t - index)
      | `Right ->
        let last = last_non () in
        sub_exn t ~index:0 ~length:(last + 1)
    with
    | Not_found -> empty
end (* Make_strip_function *)

module Make_prefix_suffix_array (A:
  sig
    type t
    type character
    val get : t -> int -> character
    val length: t -> int
    val sub_exn: t -> index:int -> length:int -> t
  end) = struct

  let rec sub_same_tl t ~comp ~len ~off =
    let rec loop i =
      i = len || (A.get t (off + i) = A.get comp i) && loop (i + 1)
    in
    (A.length t >= len) && loop 0

  let is_prefix t ~prefix =
    let len = A.length prefix in
    sub_same_tl t ~comp:prefix ~len ~off:0

  let is_suffix t ~suffix =
    let len = A.length suffix and lt = A.length t in
    sub_same_tl t ~comp:suffix ~len ~off:(lt - len)

  let chop_prefix_exn t ~prefix =
    let len = A.length prefix and lt = A.length t in
    if sub_same_tl t ~comp:prefix ~len ~off:0
    then A.sub_exn t ~index:len ~length:(lt - len)
    else raise (Invalid_argument "not a prefix")

  let chop_prefix t ~prefix =
    try Some (chop_prefix_exn t prefix)
    with _ -> None

  let chop_suffix_exn t ~suffix =
    let len = A.length suffix and lt = A.length t in
    if sub_same_tl t ~comp:suffix ~len ~off:(lt - len)
    then A.sub_exn t ~index:0 ~length:(lt - len)
    else raise (Invalid_argument "not a suffix")

  let chop_suffix t ~suffix =
    try Some (chop_suffix_exn t suffix)
    with _ -> None

end (* Make_prefix_suffix_array *)

module Make_split_at_index_functions (A:
    sig
      type t
      type character
      val empty : t
      val length : t -> int
      val sub_exn : t -> index:int -> length:int -> t
    end) = struct

  let split_at t ~index =
    let l = A.length t in
    if index < 0 then (A.empty, t)
    else if index >= l then (t, A.empty)
         else (A.sub_exn t ~index:0 ~length:index),
              (A.sub_exn t ~index:index ~length:(l - index))

  let take t ~index =
    let l = A.length t in
    if index < 0 then A.empty
    else if index >= l then t
         else A.sub_exn t ~index:0 ~length:index

  let drop t ~index =
    let l = A.length t in
    if index < 0 then t
    else if index >= l then A.empty
         else (A.sub_exn t ~index:index ~length:(l - index))


  end (* Make_split_at_index_functions *)

module Make_native (B :
  sig
    type t
    val empty : t
    val length : t -> int
    val get : t -> int -> char
    val make : int -> char -> t
    val init : int -> f:(int -> char) -> t
    val compare : t -> t -> int
    val concat : sep:t -> t list -> t
    val iter : f:(char -> unit) -> t -> unit
    val iteri : f:(int -> char -> unit) -> t-> unit
    val map : f:(char -> char) -> t -> t
    val mapi : f:(int -> char -> char) -> t -> t
    val index_from : t -> int -> char -> int
    val rindex_from : t -> int -> char -> int
    val sub : t -> pos:int -> len:int -> t
    val of_buffer : Buffer.t -> t
    val string_for_output : t -> string
  end) = struct

  type character = char
  type t = B.t

  let max_string_length = Some Sys.max_string_length
  let empty = B.empty
  let compare = B.compare
  let is_empty t = (compare B.empty t = 0)

  let make = B.make
  let length = B.length

  let of_character = B.make 1

  let of_character_list cl =
    let r = ref cl in
    B.init (List.length cl) ~f:(fun _ ->
      let c = List.hd !r in
      r := List.tl !r;
      c)

  let to_character_list s =
    let res = ref [] in
    for i = length s - 1 downto 0 do
      res := (B.get s i) :: !res
    done;
    !res

  let get s ~index =
    try Some (B.get s index)
    with _ -> None

  (* Since our set always returns a copy! *)
  let set s ~index ~v =
    if index > length s - 1
    then None
    else Some (B.mapi (fun i c -> if i = index then v else c) s)

  let get_exn s ~index = B.get s index

  let set_exn s ~index ~v =
    match set s ~index ~v with None -> failwith "set_exn" | Some s -> s

  let compare = B.compare

  let compare_substring (a, idxa, lena) (b, idxb, lenb) =
    let module With_exns = struct
      exception Return of int
      exception Left_out of int
      exception Right_out of int
      let f () =
        try
          let shortest = min lena lenb in
          for i = 0 to shortest - 1 do
            let ca = try B.get a (idxa + i) with _ -> raise (Left_out i) in
            let cb = try B.get b (idxb + i) with _ -> raise (Right_out i) in
            let c = Char.compare ca cb in
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
    type t = s
    let length = length
    let compare_substring = compare_substring
  end

  include Compare_substring_strict_of_loose(T_length_and_compsub)
  include Make_index_of_string(T_length_and_compsub)

  let concat ?(sep=B.empty) sl = B.concat ~sep sl

  let fold t ~init ~f =
    let res = ref init in
    for i = 0 to length t - 1 do
      res := f !res (B.get t i);
    done;
    !res

  let foldi t ~init ~f =
    let res = ref init in
    for i = 0 to length t - 1 do
      res := f i !res (B. get t i);
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
          res := f !res (B.get t1 i) (B.get t2 i);
        done;
        !res

  let sub_exn t ~index ~length =
    if length = 0 then empty else B.sub t ~pos:index ~len:length

  let sub t ~index ~length =
    if length = 0 then Some empty else
      try Some (B.sub t ~pos:index ~len:length)
      with e -> None

  let slice_exn ?(start=0) ?finish t =
    let length_of_t = length t in
    let bound_check strict m x =
      let out_of_ub = if strict then x > length_of_t else x >= length_of_t in
      if x < 0 || (not (is_empty t) && out_of_ub) then
        Printf.ksprintf failwith "slice_exn: invalid %s %d" m x
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

  let iter t ~f = B.iter t ~f
  let iteri t ~f = B.iteri t ~f
  let iter_reverse t ~f =
    for i = length t -1 downto 0 do
      f (get_exn t i)
    done

  let rev t =
    let lgth = length t in
    match lgth with
    | 0 -> empty
    | lgth ->
        let o = lgth - 1 in
        B.mapi ~f:(fun i _ -> B.get t (o - i)) t

  let map t ~f = B.map t ~f

  let map2_exn t1 t2 ~f =
    let lgth1 = (length t1) in
    let lgth2 = (length t2) in
    match lgth1, lgth2 with
    | 0, 0 -> empty
    | _, _ when lgth1 <> lgth2 -> failwith "map2_exn"
    | lgth1, lgth2 ->
        B.mapi ~f:(fun i c -> f c (B.get t2 i)) t1

  let mapi t ~f = B.mapi t ~f

  let for_all t ~f =
    try (iter t (fun x -> if not (f x) then raise Not_found else ()); true)
    with Not_found -> false

  let exists t ~f =
    try (iter t (fun x -> if f x then raise Not_found else ()); false)
    with Not_found -> true

  let index_of_character t ?(from=0) c =
    let from = if from <= 0 then 0 else min (length t) from in
    try Some (B.index_from t from c)
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
    try Some (B.rindex_from t from c)
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
    let length_of_s = B.length s in
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
    let length_of_s = B.length s in
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
    let length_of_s = B.length s in
    let from, length = resize_from_length ~from ?length ~length_of_s in
    if length = 0 then empty
    else begin
      let b = Buffer.create length in
      for i = 0 to length - 1 do
        match f (get_exn s (i + from)) with
        | Some c -> Buffer.add_char b c
        | None -> ()
      done;
      B.of_buffer b
    end

  let filter ?from ?length s ~f =
      filter_map ?from ?length s ~f:(fun c -> if f c then Some c else None)

  include Make_strip_function (struct
      type t = s
      type character = char
      let empty = empty
      let length = length
      let sub_exn = sub_exn
      let find = find
      let find_reverse = find_reverse
      let is_whitespace = Native_character.is_whitespace
    end)

  include Make_split_function(struct
      type t = s
      type character = char
      let length = length
      let sub_exn = sub_exn
      let index_of_string = index_of_string
      let index_of_character = index_of_character
    end)

  include Make_prefix_suffix_array (struct
      type t = s
      type character = char
      let length = length
      let get = B.get
      let sub_exn = sub_exn
    end)

  include Make_split_at_index_functions(struct
      type t = s
      type character = char
      let empty = empty
      let length = length
      let sub_exn = sub_exn
    end)

  module Make_output (Model: Api.OUTPUT_MODEL) = struct

    let output chan t = Model.output chan (B.string_for_output t)

  end

  let take_while_with_index t ~f =
    let buf = Buffer.create (length t) in
    let rec loop idx =
      match get t idx with
      | Some c when f idx c -> Buffer.add_char buf c; loop (idx + 1)
      | _ -> ()
    in
    loop 0;
    B.of_buffer buf

  let take_while t ~f = take_while_with_index t ~f:(fun _ c -> f c)

end  (* Make_native *)
