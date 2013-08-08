#! /bin/sh

PACKAGES=core,sexplib.syntax

MD5=`md5sum $0  | cut -d ' ' -f 1`
BASE=/tmp/ocaml_script_$MD5/
mkdir -p $BASE

ML_FILE=${BASE}/source.ml
EXEC=${BASE}/`basename $0`

if test -f $BASE
then
  $EXEC $*
  RETURN_CODE=$?
else

  SKIP=`awk '/^__OCAML_FOLLOWS__/ { print NR + 1; exit 0; }' $0`
  echo "#$SKIP \"$0\"" > $ML_FILE
  tail -n +$SKIP $0 >> $ML_FILE

  ocamlfind ocamlopt -I _build/ sosa.cmxa -thread -package $PACKAGES -syntax camlp4o -linkpkg -o $EXEC $ML_FILE \
    && $EXEC $*
  RETURN_CODE=$?
fi
exit $RETURN_CODE

__OCAML_FOLLOWS__

open Core.Std

open Sosa

let say fmt = printf (fmt ^^ "\n%!")

module type TEST_STRING = sig
  module Ch: BASIC_CHAR
  module Str: BASIC_STRING with type character = Ch.t
end

let do_basic_test (module Test : TEST_STRING) =
  let open Test in
  let one =
    Str.of_character_list
      (List.filter_map ['a'; 'A'; 'B'; '\000'] Ch.of_ocaml_char) in
  say "basic_test: one: %s, length: %d"
    (Str.to_string_hum one) (Str.length one);
  let sep = Str.of_character (Option.value_exn (Ch.of_ocaml_char '-')) in
  let two = Str.concat ~sep [one;one;one] in
  let verif = Str.to_ocaml_string two in
  assert (verif = "aAB\000-aAB\000-aAB\000");
  ()

let () =
  do_basic_test (module struct
    module Ch = Native_char
    module Str = Native_string
  end);
  do_basic_test (module struct
    module Ch = Native_char
    module Str = List_of (Native_char)
  end);
  eprintf "Hello %s!\n" (<:sexp_of<
                          [ `world | `planet ]
                        >> `world |> Sexp.to_string_hum)
