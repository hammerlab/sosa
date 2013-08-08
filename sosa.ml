
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

  val to_ocaml_string: t -> string
  val to_string_hum: t -> string


end

open Printf

module Internal_pervasives = struct
  module List = ListLabels
  let return x : (_, _) result = `Ok x
  let fail x : (_, _) result = `Error x
  let bind x f =
    match x with
    | `Ok o -> f o
    | `Error e -> fail e
  let (>>=) = bind
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

  let to_ocaml_string l =
    let length =
      List.fold_left l ~init:0 ~f:(fun sum c -> sum + Char.size c) in
    let buf = String.make length 'B' in
    let index = ref 0 in
    List.iter l ~f:begin fun c ->
      match Char.write_to_ocaml_string c ~index:!index ~buf with
      | `Ok () -> incr index
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
