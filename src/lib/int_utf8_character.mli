(** {!modtype:Api.BASIC_CHARACTER} with OCaml integers ([int]) representing
    Utf8 characters  *)

include (Api.BASIC_CHARACTER with type t = int)
