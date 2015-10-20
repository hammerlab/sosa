
open Api
open Printf

module List = ListLabels
module String = StringLabels

let (|>) x f = f x
let return x : (_, _) result = `Ok x
let fail x : (_, _) result = `Error x
let bind x f =
  match x with
  | `Ok o -> f o
  | `Error e -> fail e
let (>>=) = bind
let dbg fmt = printf ("DBG: " ^^ fmt ^^ "\n%!")

(* The function `List.map` adapted from `Core_kernel`'s way of
    unrolling the loops. *)
module Core_list_map = struct

  let map_slow l ~f = List.rev (List.rev_map ~f l)

  let rec count_map ~f l ctr =
    match l with
    | [] -> []
    | [x1] ->
      let f1 = f x1 in
      [f1]
    | [x1; x2] ->
      let f1 = f x1 in
      let f2 = f x2 in
      [f1; f2]
    | [x1; x2; x3] ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      [f1; f2; f3]
    | [x1; x2; x3; x4] ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      let f4 = f x4 in
      [f1; f2; f3; f4]
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      let f4 = f x4 in
      let f5 = f x5 in
      f1 :: f2 :: f3 :: f4 :: f5 ::
        (if ctr > 1000
          then map_slow ~f tl
          else count_map ~f tl (ctr + 1))

  let map l ~f = count_map ~f l 0

  let mapi_slow l ~f ~i =
    let _, r = List.fold_left l
      ~f:(fun (i, a) e -> (i + 1, ((f i e)::a)))
      ~init:(i,[])
    in
    List.rev r

  let rec count_mapi ~f l ctr =
    match l with
    | [] -> []
    | [x1] ->
      let f1 = f ctr x1 in
      [f1]
    | [x1; x2] ->
      let f1 = f ctr x1 in
      let f2 = f (ctr + 1) x2 in
      [f1; f2]
    | [x1; x2; x3] ->
      let f1 = f ctr x1 in
      let f2 = f (ctr + 1) x2 in
      let f3 = f (ctr + 2) x3 in
      [f1; f2; f3]
    | [x1; x2; x3; x4] ->
      let f1 = f ctr x1 in
      let f2 = f (ctr + 1) x2 in
      let f3 = f (ctr + 2) x3 in
      let f4 = f (ctr + 3) x4 in
      [f1; f2; f3; f4]
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
      let f1 = f ctr x1 in
      let f2 = f (ctr + 1) x2 in
      let f3 = f (ctr + 2) x3 in
      let f4 = f (ctr + 3) x4 in
      let f5 = f (ctr + 4) x5 in
      f1 :: f2 :: f3 :: f4 :: f5 ::
        (if ctr > 5000
          then mapi_slow ~f ~i:(ctr + 5) tl
          else count_mapi ~f tl (ctr + 5))

  let mapi l ~f = count_mapi ~f l 0

  let map2_slow l1 l2 ~f = List.rev (List.rev_map2 ~f l1 l2)

  let rec count_map2_exn ~f l1 l2 ctr =
    match l1, l2 with
    | [], [] -> []
    | [x1], [y1] ->
      let f1 = f x1 y1 in
      [f1]
    | [x1; x2], [y1; y2] ->
      let f1 = f x1 y1 in
      let f2 = f x2 y2 in
      [f1; f2]
    | [x1; x2; x3], [y1; y2; y3] ->
      let f1 = f x1 y1 in
      let f2 = f x2 y2 in
      let f3 = f x3 y3 in
      [f1; f2; f3]
    | [x1; x2; x3; x4], [y1; y2; y3; y4] ->
      let f1 = f x1 y1 in
      let f2 = f x2 y2 in
      let f3 = f x3 y3 in
      let f4 = f x4 y4 in
      [f1; f2; f3; f4]
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl1,
      y1 :: y2 :: y3 :: y4 :: y5 :: tl2 ->
      let f1 = f x1 y1 in
      let f2 = f x2 y2 in
      let f3 = f x3 y3 in
      let f4 = f x4 y4 in
      let f5 = f x5 y5 in
      f1 :: f2 :: f3 :: f4 :: f5 ::
        (if ctr > 1000
          then map2_slow ~f tl1 tl2
          else count_map2_exn ~f tl1 tl2 (ctr + 1))
    | _, _ -> failwith "count_map2"

  let map2_exn l1 l2 ~f = count_map2_exn ~f l1 l2 0
end
