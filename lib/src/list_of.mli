(** A functor to create strings that are lists of characters.*)

module Make (Char : Api.BASIC_CHARACTER) :
  Api.BASIC_STRING with type character = Char.t

