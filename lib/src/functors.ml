(* Components for implementing common logic throughout implementations.*)
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
