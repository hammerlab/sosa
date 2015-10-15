
open Sosa_pervasives
open Printf
open Sosa_utilities

module F = Functors

module Make (Char: Api.BASIC_CHARACTER) :
  Api.BASIC_STRING
  with type character := Char.t
  with type t = Char.t list = struct

  type character = Char.t

  type t = character list

  let empty = []
  let is_empty = (=) []

  let make length c =
    let rec loop n acc =
      if n >= length then acc else loop (n + 1) (c :: acc)
    in
    loop 0 []

  let of_character c = [c]
  let of_character_list cl = cl
  let to_character_list cl = cl

  let get sl ~index =
    try Some (List.nth sl index) with _ -> None

  let set s ~index ~v =
    let rec loop n acc = function
    | [] -> None
    | q :: t when n = index ->
      Some (List.rev_append acc (v :: t))
    | q :: t ->
      loop (n + 1) (q :: acc) t
    in
    loop 0 [] s

  let get_exn s ~index =
    match get s ~index with None -> failwith "get_exn" | Some s -> s
  let set_exn s ~index ~v =
    match set s ~index ~v with None -> failwith "set_exn" | Some s -> s

  let iter t ~f = List.iter t ~f
  let iteri t ~f = List.iteri t ~f
  let iter_reverse t ~f =
    List.iter (List.rev t) ~f

  let rev t = List.rev t

  let fold t ~init ~f = List.fold_left t ~init ~f
  let foldi t ~init ~f =
    snd (List.fold_left t ~init:(0,init)
          ~f:(fun (i,a) c -> (i+1,f i a c)))
  let fold2_exn t1 t2 ~init ~f = List.fold_left2 t1 t2 ~init ~f
  let map = Core_list_map.map
  let mapi = Core_list_map.mapi
  let map2_exn = Core_list_map.map2_exn
  let for_all t ~f = List.for_all t ~f
  let exists t ~f = List.exists t ~f

  let compare (a : Char.t list) (b: Char.t list) = compare a b
  let of_native_substring s ~offset ~length =
    Conversions.of_native_substring
      ~empty ~init:(fun () -> ref [])
      ~on_new_character:(fun x c -> x := c :: !x)
      ~finalize:(fun x -> List.rev !x)
      ~read_character_from_native_string:Char.read_from_native_string
      s ~offset ~length

  let of_native_string s =
    Conversions.of_native_string
      of_native_substring s

  let to_native_string l =
    Conversions.to_native_string_knowing_size
      ~future_size:(fun l ->
          List.fold_left l ~init:0 ~f:(fun sum c -> sum + Char.size c))
      ~iter ~write_char_to_native_bytes:Char.write_to_native_bytes
      l
    |> Bytes.to_string

  let to_string_hum l = sprintf "%S" (to_native_string l)

  let concat ?(sep=[]) ll =
    match ll with
    | [] -> []
    | hh :: tt ->
      let x = ref (List.rev hh) in
      List.iter tt ~f:(fun l ->
        x := List.rev_append sep !x;
        x := List.rev_append l !x;
        );
      List.rev !x

  let length = List.length

  let sub t ~index ~length =
    let r = ref [] in
    let c = ref 0 in
    try
      List.iteri t ~f:(fun i a ->
          if i >= index + length then raise Not_found;
          if index <= i then (
            r:= a :: !r;
            incr c;
          );
        );
      if !c = length then Some (List.rev !r) else None
    with
    | Not_found -> Some (List.rev !r)

  let sub_exn t ~index ~length =
    match sub t ~index ~length with
    | Some s -> s
    | None -> ksprintf failwith "sub_exn(%d,%d)" index length

  let slice_exn ?(start=0) ?finish t =
    let length_of_t = List.length t in
    if start < 0 || (not (is_empty t) && start >= length_of_t) then
      ksprintf failwith "slice_exn: invalid start %d" start
    else
      match finish with
      | None   -> sub_exn t ~index:start ~length:(length_of_t - start)
      | Some f -> if f < 0 || f > length_of_t then
                    ksprintf failwith "slice_exn: invalid finish %d" f
                  else
                    sub_exn t ~index:start ~length:(f - start)

  let slice ?start ?finish t =
    try Some (slice_exn ?start ?finish t)
    with _ -> None


  let rec comp_loop p lst_pair =
    if p
    then match lst_pair with
         | (i,[])           -> Some i
         | ([],j)           -> None
         | (i::is), (j::js) -> comp_loop (i = j) (is,js)
    else None

  let is_prefix t ~prefix =
    match comp_loop true (t,prefix) with
    | Some _  -> true
    | None    -> false

  let is_suffix t ~suffix =
    is_prefix (List.rev t) ~prefix:(List.rev suffix)

  let chop_prefix_exn t ~prefix =
    match comp_loop true (t,prefix) with
    | Some r  -> r
    | None    -> raise (Invalid_argument "chop_prefix_exn: not a prefix")

  let chop_prefix t ~prefix =
    try Some (chop_prefix_exn t prefix)
    with _ -> None

  let chop_suffix_exn t ~suffix =
    List.rev (chop_prefix_exn (List.rev t) ~prefix:(List.rev suffix))

  let chop_suffix t ~suffix =
    try Some (chop_suffix_exn t suffix)
    with _ -> None

  let unrevSplit t n =
    if n < 0
    then [],t
    else let rec offset i ((l,r) as p) =
           if i = n
           then p
           else match r with
                | []   -> p
                | h::t -> offset (i + 1) (h::l,t)
         in
         offset 0 ([],t)

  let split_at t ~index =
    let l,r = unrevSplit t index in
    List.rev l, r

  let take t ~index = fst (split_at t index)

  let drop t ~index =
    let l,r = unrevSplit t index in
    r


  let index_of_character t ?(from=0) c =
    let index = ref 0 in
    try begin
      List.iter t ~f:(fun x ->
          if !index >= from
          then
            if x = c
            then failwith "found"
            else incr index
          else incr index);
      None
    end
    with _ -> Some !index

  let index_of_character_reverse t ?from c =
    let length_of_t, rev =
      let rec loop lgth acc = function
      | [] -> (lgth, acc)
      | h :: t -> loop (lgth + 1) (h :: acc) t in
      loop 0 [] t
    in
    let from =
      match from with
      | None -> length_of_t - 1
      | Some s when s < 0 -> -1
      | Some s when s > length_of_t - 1 -> length_of_t - 1
      | Some s -> s
    in
    match index_of_character rev ~from:(length_of_t - from - 1) c with
    | Some c -> Some (length_of_t - c - 1)
    | None -> None

  let compare_substring (a, idxa, lena) (b, idxb, lenb) =
    let module With_exns = struct
      exception Left
      exception Right
      let rec drop_until ~exn idx l =
        match idx, l with
        | 0, l -> l
        | more, [] -> raise exn
        | more, h :: t -> drop_until ~exn (more - 1) t
      let f () =
        begin try
          let rec cmp l1 l2 len1 len2 =
            if len1 < 0 then raise Left;
            if len2 < 0 then raise Right;
            match l1, l2 with
            | _, _ when len1 = 0 && len2 = 0 -> 0
            | _, _ when len1 = 0 -> -1
            | _, _ when len2 = 0 -> 1
            | [], [] when len1 = 0 || len2 = 0 -> Pervasives.compare lena lenb
            | [], _ when len1 > 0 -> raise Left
            | _, [] when len2 > 0 -> raise Right
            | h1 :: t1, h2 :: t2 when Char.compare h1 h2 = 0 ->
              cmp t1 t2 (len1 - 1) (len2 - 1)
            | h1 :: _, h2 :: _ -> Char.compare h1 h2
            | _, _ -> assert false (* calming down the warnings.. *)
          in
          if lena = 0 && lenb = 0 then 0
          else (
            let aa = drop_until ~exn:Left idxa a in
            let bb = drop_until ~exn:Right idxb b in
            (cmp aa bb lena lenb)
          )
        with
        | Left -> -1
        | Right -> 1
        | Failure s -> 1
          (* dbg "(%d, %d/%d) Vs (%d, %d/%d) %s" idxa lena (length a) idxb lenb (length b) s; *)
        end
    end in
    With_exns.f ()

  type s = t
  module T_length_and_compsub = struct
    type t = s let length = length let compare_substring = compare_substring
  end
  include F.Compare_substring_strict_of_loose(T_length_and_compsub)
  include F.Make_index_of_string(T_length_and_compsub)

  let find ?(from=0) ?length s ~f =
    (* index and virtual_length are maybe a bit redundant but I favor
       readability of the branches of the match *)
    let from = if from <= 0 then 0 else from in
    let rec find_from index virtual_length l =
      match l, length with
      | [], _ -> None
      | _, Some lgth when lgth <= virtual_length -> None
      | h :: t, _ when index < from -> find_from (index + 1) virtual_length t
      | h :: t, _ when index >= from && f h -> Some index
      | h :: t, _ -> find_from (index + 1) (virtual_length + 1) t
    in
    find_from 0 0 s

  let find_reverse ?from ?length s ~f =
    let length_of_s = List.length s in
    let from =
      match from with
      | None -> None
      | Some s when s < 0 -> Some length_of_s
      | Some s when s > length_of_s - 1 -> Some 0
      | Some s -> Some (length_of_s - 1 - s)
    in
    match find ?from ?length (List.rev s) ~f with
    | None -> None
    | Some i -> Some (length_of_s - 1 - i)

  let filter_map ?(from=0) ?length t ~f =
    let rec filter_map_rec acc index virtual_length l =
      match l, length with
      | [], _ -> List.rev acc
      | _, Some lgth when lgth <= virtual_length -> List.rev acc
      | h :: t, _ when index < from ->
        filter_map_rec acc (index + 1) virtual_length t
      | h :: t, _ (* when index >= from  *) ->
        begin match f h with
        | Some o -> filter_map_rec (o :: acc) (index + 1) (virtual_length + 1) t
        | None -> filter_map_rec acc (index + 1) (virtual_length + 1) t
        end
    in
    filter_map_rec [] 0 0 t

  let filter ?from ?length t ~f =
      filter_map ?from ?length t ~f:(fun c -> if f c then Some c else None)

  include F.Make_strip_function (struct
      type t = Char.t list
      type character = Char.t
      let empty = empty
      let length = length
      let sub_exn = sub_exn
      let find = find
      let find_reverse = find_reverse
      let is_whitespace = Char.is_whitespace
    end)

  include F.Make_split_function(struct
      type t = Char.t list
      type character = Char.t
      let length = length
      let sub_exn = sub_exn
      let index_of_string = index_of_string
      let index_of_character = index_of_character
    end)

  module Make_output (Model: Api.OUTPUT_MODEL) = struct

    let (>>=) = Model.bind

    let output chan l =
      List.fold_left l ~init:(Model.return ()) ~f:(fun prev_m c ->
          prev_m >>= fun () ->
          (* TODO: Safe to call Bytes.unsafe_to_string? *)
          Model.output chan (Char.to_native_bytes c |> Bytes.to_string))

  end

  let take_while_with_index t ~f =
    let rec loop idx acc =
      function
      | h :: t when f idx h -> loop (idx + 1) (h :: acc) t
      | []
      | _ :: _ -> List.rev acc
    in
    loop 0 [] t
  let take_while t ~f = take_while_with_index t ~f:(fun _ c -> f c)

end (* Make *)
