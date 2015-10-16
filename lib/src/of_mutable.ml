open Sosa_pervasives
open Printf
module F = Functors

module Make (S: Api.MINIMALISTIC_MUTABLE_STRING) : Api.BASIC_STRING
  with type character = S.character
  with type t = S.t = struct

  include S
  let is_empty s =
    try ignore (S.get s 0); false with _ -> true

  let get t ~index = try Some (get t index) with _ -> None
  let set t ~index ~v:c =
    let lgth = length t in
    if index < 0 || lgth <= index then None
    else Some (
        let res = make lgth (S.get t 0) in
        blit ~dst:res ~dst_pos:0 ~src:t ~src_pos:0 ~len:lgth;
        S.set res index c;
        res)

  let get_exn s ~index = S.get s index
  let set_exn s ~index ~v =
    match set s ~index ~v with None -> failwith "set_exn" | Some s -> s

  let of_character c = make 1 c

  let of_character_list cl =
    match cl with
    | [] -> empty
    | one :: more ->
      let res = make (List.length cl) one in
      List.iteri more ~f:(fun  i c ->
          S.set res (i + 1) c);
      res

  let to_character_list s =
    let res = ref [] in
    for i = S.length s - 1 downto 0 do
      res := S.get s i :: !res
    done;
    !res

  let rec concat  ?(sep=empty) tl =
    match tl with
    | [] -> empty
    | one :: more ->
      begin try
        let first_char =
          try S.get one 0
          with _ -> S.get sep 0
        in
        let sep_length = S.length sep in
        let total_length =
          List.fold_left ~init:(S.length one) more ~f:(fun prev s ->
              prev + sep_length + S.length s) in
        let dst = make total_length first_char in
        let index = ref 0 in
        blit ~dst ~dst_pos:!index ~src:one ~src_pos:0 ~len:(length one);
        index := !index + (length one);
        List.iter more ~f:(fun s ->
            blit ~dst ~dst_pos:!index ~src:sep ~src_pos:0 ~len:sep_length;
            index := !index + sep_length;
            blit ~dst ~dst_pos:!index ~src:s ~src_pos:0 ~len:(length s);
            index := !index + (length s);
          );
        dst
      with _ ->
        concat more ~sep (* both one and sep are empty *)
      end

  let iter t ~f =
    for i = 0 to length t - 1 do
      f (S.get t i)
    done

  let iteri t ~f =
    for i = 0 to length t - 1 do
      f i (S.get t i)
    done

  let iter_reverse t ~f =
    for i = length t -1 downto 0 do
      f (S.get t i)
    done

  let fold t ~init ~f =
    let x = ref init in
    for i = 0 to length t - 1 do
      x := f !x (S.get t i)
    done;
    !x

  let foldi t ~init ~f =
    let x = ref init in
    for i = 0 to length t - 1 do
      x := f i !x (S.get t i)
    done;
    !x

  let fold2_exn t1 t2 ~init ~f =
    let lgth1 = (length t1) in
    let lgth2 = (length t2) in
    match lgth1, lgth2 with
    | 0, 0 -> init
    | _, _ when lgth1 <> lgth2 -> failwith "fold2_exn"
    | lgth1, lgth2 ->
       let res = ref init in
       for i = 0 to lgth1 - 1 do
         res := f !res (S.get t1 i) (S.get t2 i);
       done;
       !res

  let rev t =
    let lgth = length t in
    match lgth with
    | 0 -> empty
    | lgth ->
       let res = make lgth (S.get t 0) in
       for i = 0 to lgth - 1 do
         S.set res i (S.get t (lgth - 1 - i))
       done;
       res

  let map t ~f =
    let lgth = (length t) in
    if lgth = 0
    then empty
    else begin
      let res = make lgth (S.get t 0) in
      for i = 0 to lgth - 1 do
        S.set res i (f (S.get t i))
      done;
      res
    end

  let mapi t ~f =
    let lgth = (length t) in
    if lgth = 0
    then empty
    else begin
      let res = make lgth (S.get t 0) in
      for i = 0 to lgth - 1 do
        S.set res i (f i (S.get t i))
      done;
      res
    end

  let map2_exn t1 t2 ~f =
    let lgth1 = (length t1) in
    let lgth2 = (length t2) in
    match lgth1, lgth2 with
    | 0, 0 -> empty
    | _, _ when lgth1 <> lgth2 -> failwith "map2_exn"
    | lgth1, lgth2 ->
       let res = make lgth1 (S.get t1 0) in
       for i = 0 to lgth1 - 1 do
         S.set res i (f (S.get t1 i) (S.get t2 i))
       done;
       res

  let for_all t ~f =
    try
      iter t (fun c -> if not (f c) then raise Not_found);
      true
    with _ -> false

  let exists t ~f =
    try
      iter t (fun c -> if (f c) then raise Not_found);
      false
    with _ -> true

  let sub t ~index ~length =
    if length = 0 then Some empty else
      begin
        let lgth = S.length t in
        if lgth = 0
        then None (* `length <> 0` *)
        else begin
          try
            let res = make length (S.get t index) in
            for i = 1 to length - 1 do
              S.set res i (S.get t (index + i))
            done;
            Some res
          with _ -> None
        end
      end

  let sub_exn t ~index ~length =
    match sub t ~index ~length with
    | Some s -> s
    | None -> ksprintf failwith "sub_exn(%d,%d)" index length

  let slice_exn ?(start=0) ?finish t =
    let length_of_t = S.length t in
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

  let to_string_hum t = to_native_string t |> sprintf "%S"

  let index_of_character t ?(from=0) c =
    let from = if from <= 0 then 0 else min (length t) from in
    let res = ref None in
    try
      for i = from to length t - 1 do
        if S.get t i = c then (res:= Some i; raise Not_found)
      done;
      None
    with _ -> !res

  let index_of_character_reverse t ?from c =
    let from =
      let length_of_t = length t in
      match from with
      | None -> length_of_t - 1
      | Some s when s < 0 -> -1
      | Some s when s > length_of_t - 1 -> length_of_t - 1
      | Some s -> s
    in
    let res = ref None in
    try
      for i = from downto 0 do
        if S.get t i = c then (res:= Some i; raise Not_found)
      done;
      None
    with _ -> !res

  let compare_substring (a, idxa, lena) (b, idxb, lenb) =
    let module With_exns = struct
      exception Return of int
      exception Left_out of int
      exception Right_out of int
      let f () =
        try
          let shortest = min lena lenb in
          for i = 0 to shortest - 1 do
            let ca = try S.get a (idxa + i) with _ -> raise (Left_out i) in
            let cb = try S.get b (idxb + i) with _ -> raise (Right_out i) in
            let c = S.compare_char  ca cb in
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
    let length_of_s = S.length s in
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
    let length_of_s = S.length s in
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
    let length_of_s = S.length s in
    let from, length = resize_from_length ~from ?length ~length_of_s in
    if length = 0 then empty
    else begin
      let res = ref [] in
      for i = length - 1 downto 0 do
        match f (get_exn s (i + from)) with
        | Some c -> res := c :: !res
        | None -> ()
      done;
      of_character_list !res
    end

  let filter ?from ?length s ~f =
      filter_map ?from ?length s ~f:(fun c -> if f c then Some c else None)

  include F.Make_strip_function (struct
      type t = S.t
      type character = S.character
      let empty = empty
      let length = length
      let sub_exn = sub_exn
      let find = find
      let find_reverse = find_reverse
      let is_whitespace = S.is_whitespace
    end)

  include F.Make_split_function(struct
      type t = S.t
      type character = S.character
      let length = length
      let sub_exn = sub_exn
      let index_of_string = index_of_string
      let index_of_character = index_of_character
    end)


  include F.Make_prefix_suffix_array (struct
    type t = S.t
    type character = S.character
    let length = S.length
    let get = S.get
    let sub_exn = sub_exn
    end)


  include F.Make_split_at_index_functions(struct
      type t = S.t
      type character = S.character
      let empty = empty
      let length = length
      let sub_exn t ~index ~length = sub_exn t index length
    end)

  module Make_output (Model: Api.OUTPUT_MODEL) = struct

    let (>>=) = Model.bind

    let output chan t =
      Model.output chan (to_native_string t)

  end

  let take_while_with_index t ~f =
    if length t = 0 then empty
    else (
      let buf = make (length t) (S.get t 0) in
      let rec loop idx =
        match get t idx with
        | Some c when f idx c -> S.set buf idx c; loop (idx + 1)
        | _ -> idx
      in
      let new_length = loop 0 in
      sub_exn buf ~index:0 ~length:new_length
    )

  let take_while t ~f = take_while_with_index t ~f:(fun _ c -> f c)

end (* Make *)
