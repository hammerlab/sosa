(** A functor that uses an implementation of
   {!modtype:Api.MINIMALISTIC_MUTABLE_STRING} to build
   {!modtype:Api.BASIC_STRING}. *)

module Make (S : Api.MINIMALISTIC_MUTABLE_STRING) :
  Api.BASIC_STRING with type character = S.character
