
type ('a, 'b) result = [
  | `Ok of 'a
  | `Error of 'b
]

module type BASIC_CHAR = sig

  type t

  val of_ocaml_char: char -> t option
  val of_int: int -> t option

  val size: t -> int

  val to_ocaml_string: t -> String.t
  val to_string_hum: t -> String.t
  val write_to_ocaml_string: t -> buf:String.t -> index:int -> (unit, [> `out_of_bounds]) result
  val read_from_ocaml_string: buf:String.t -> index:int -> (t * int) option

end

module type BASIC_STRING = sig


  type character
  type t

  val of_character: character -> t
  val of_character_list: character list -> t

  val get: t -> index:int -> character option
  (** Get the n-th char, not necessarily bytes or bits. *)

  val set: t -> index:int -> v:character -> t option
  (** String should not be mutable. *)

  val length: t -> int

  val concat: ?sep:t -> t list -> t

  val of_ocaml_string: string -> (t, [> `wrong_char_at of int ]) result
  val to_ocaml_string: t -> string
  val to_string_hum: t -> string


end

open Printf

module Internal_pervasives = struct
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
  let dbg fmt = eprintf ("DBG: " ^^ fmt ^^ "\n%!")
end
open Internal_pervasives


module type NATIVE_CHAR = BASIC_CHAR with type t = char

module type NATIVE_STRING = sig
  include BASIC_STRING
    with type t = String.t
    with type character = char
end

module Native_char : NATIVE_CHAR = struct

    type t = char

    let of_ocaml_char x = Some x
    let of_int x =
      try Some (char_of_int x) with _ -> None

    let size _ = 1

    let is_print t = ' ' <= t && t <= '~'
    let to_ocaml_string x = String.make 1 x
    let to_string_hum x =
      if is_print x then String.make 1 x
      else sprintf "0x%2x" (int_of_char x)

    let write_to_ocaml_string c ~buf ~index =
      try buf.[index] <- c; return ()
      with _ -> fail `out_of_bounds

    let read_from_ocaml_string ~buf ~index =
      try Some (buf.[index], 1)
      with _ -> None

end

module Native_string : NATIVE_STRING = struct

  include StringLabels
  type character = char


  let of_character = String.make 1
  let of_character_list cl =
    let length = List.length cl in
    let buf = String.make length '\x00' in
    List.iteri cl ~f:(fun i c -> buf.[i] <- c);
    buf

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

  let to_ocaml_string x = String.copy x
  let of_ocaml_string x = return (String.copy x)
  let to_string_hum x = sprintf "%S" x

  let concat ?(sep="") sl = concat ~sep sl

end

module List_of (Char: BASIC_CHAR) :
  BASIC_STRING
  with type character = Char.t
  with type t = Char.t list = struct

  type character = Char.t

  type t = character list

  let of_character c = [c]
  let of_character_list cl = cl

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

  let of_ocaml_string s =
    let module With_exn = struct
      exception WChar of int
      let f buf =
        let x = ref [] in
        try
          let rec loop index =
            if index < String.length buf
            then
              begin match Char.read_from_ocaml_string ~buf ~index with
              | Some (s, size) ->  x := s :: !x; loop (index + size)
              | None -> raise (WChar index)
              end
            else ()
          in
          loop 0;
          return (List.rev !x)
        with WChar c -> fail (`wrong_char_at c)
    end in
    With_exn.f s

  let to_ocaml_string l =
    let length =
      List.fold_left l ~init:0 ~f:(fun sum c -> sum + Char.size c) in
    let buf = String.make length 'B' in
    let index = ref 0 in
    List.iter l ~f:begin fun c ->
      match Char.write_to_ocaml_string c ~index:!index ~buf with
      | `Ok () ->  index := !index + Char.size c
      | `Error `out_of_bounds -> failwith "Bug in List_of.to_ocaml_string"
    end;
    buf

  let to_string_hum l = sprintf "%S" (to_ocaml_string l)

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



end

module Int_utf8_character : BASIC_CHAR with type t = int = struct

    type t = int

    let of_ocaml_char x = Some (int_of_char x)

    let of_int x =
      if x land 0x7FFF_FFFF = x then Some x else None

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

    let write_to_ocaml_string c ~buf ~index =
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
        buf.[index] <- char_of_int first_byte;
        for i = 2 to sz  do
          let ith_byte =
            ((c lsr (6 * (i - 2))) land 0b0011_1111) lor 0b1000_0000 in
          buf.[index + sz - i + 1] <- char_of_int ith_byte;
        done;
        return ()
      with _ -> fail `out_of_bounds

    let read_from_ocaml_string ~buf ~index =
      (* dbg "buf: %S" buf; *)
      try
        let first_char = buf.[index] |> int_of_char in
        (* dbg "first_char lsr     5: %x" (first_char lsr     5); *)
        let size, mask =
          if first_char lsr 7 = 0 then (1, 0b0111_1111)
          else if first_char lsr     5 =     0b110 then (2, 0b0001_1111)
          else if first_char lsr     4 =    0b1110 then (3, 0b0000_1111)
          else if first_char lsr     3 =   0b11110 then (4, 0b0000_0111)
          else if first_char lsr     2 =  0b111110 then (5, 0b0000_0011)
          else if first_char lsr     1 = 0b1111110 then (6, 0b0000_0001)
          else raise Not_found
        in
        (* dbg "first_char %d, size %d, mask %x" first_char  size mask; *)
        let the_int = ref (first_char land mask) in
        (* dbg "the_int : %d, %x" !the_int !the_int; *)
        for i = 1 to size - 1 do
          let the_char = buf.[index + i] |> int_of_char in
          (* dbg "(the_char lsr 6): %d" (the_char lsr 6); *)
          if (the_char lsr 6) = 0b10
          then (
            the_int := (!the_int lsl 6) lor (the_char land 0b0011_1111);
            (* dbg "the_int : %d, %x" !the_int !the_int; *)
          ) else raise Not_found;
        done;
        (* dbg "the_int : %d, %x" !the_int !the_int; *)
        Some (!the_int, size)
      with _ -> None

    let to_ocaml_string x =
      let buf = String.make (size x) 'B' in
      begin match write_to_ocaml_string x ~buf ~index:0 with
      | `Ok () -> ()
      | `Error e -> assert false
      end;
      buf


end
