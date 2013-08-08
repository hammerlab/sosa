
type ('a, 'b) result = [
  | `Ok of 'a
  | `Error of 'b
]

module type BASIC_CHAR = sig

  type string
  type t

  val of_ocaml_char: char -> t option
  val of_int: int -> t option

  val serialize: t -> (string * int)

  val unserialize: string -> int -> ((t * int), [> `out_of_bounds]) result

  val to_string_hum: t -> String.t

end

module type BASIC_STRING = sig


  type t

  module Char: BASIC_CHAR with type string = t

  val of_char: Char.t -> t
  val of_char_list: Char.t list -> t

  val get: t -> int -> Char.t option
  (** Get the n-th char, not necessarily bytes or bits. *)

  val set: t -> int -> Char.t -> t option
  (** String should not be mutable. *)

  val length: t -> int

  val concat: ?sep:t -> t list -> t

  val to_ocaml_string: t -> string
  val to_string_hum: t -> string


end

let return x : (_, _) result = `Ok x
let fail x : (_, _) result = `Error x
let bind x f =
  match x with
  | `Ok o -> f o
  | `Error e -> fail e
let (>>=) = bind
