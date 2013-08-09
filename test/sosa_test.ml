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
  val test_name: string
  module Chr: BASIC_CHAR
  module Str: BASIC_STRING with type character = Chr.t
end

let test_assert msg cond =
  if not cond then say ">> TEST FAILED: [%s]" msg else ()

let test_assertf cond fmt =
  ksprintf (fun s -> test_assert s cond) fmt

let random_string i =
  let length = Random.int i in
  String.init length (fun _ -> Char.of_int_exn (Random.int 256))


let random_strings =
  List.init 200 (fun i -> random_string (i * 4 + 1))

let do_basic_test (module Test : TEST_STRING) =
  let open Test in
  say "### Test %S" test_name;

  let test_ofto s =
    begin match Str.of_ocaml_string s with
    | `Ok s2 ->
      let back = Str.to_ocaml_string s2 in
      test_assert (sprintf "test_ofto %S <> %S" s back) (s = back)
    | `Error (`wrong_char_at i) ->
      test_assert (sprintf "test_ofto %S -> wrong char at index %d" s i)
        (Chr.read_from_ocaml_string ~buf:s ~index:i = None)
    end;
  in
  List.iter random_strings test_ofto;

  let rec try_separators n =
    let sep = random_string n in
    if n = 0
    then say "WARNING: %s -> try_separators did not try anything" test_name
    else
      begin match Str.of_ocaml_string sep with
      | `Ok csep ->
        let viable_strings, converted =
          List.filter_map random_strings (fun s ->
              match Str.of_ocaml_string s with
              | `Ok s2 ->  Some (s, s2)
              | `Error (`wrong_char_at c) -> None)
          |> List.unzip
        in
        let concated = String.concat ~sep viable_strings in
        let concated2 = Str.concat ~sep:csep converted in
        test_assert (sprintf "try_separators %d" n)
          (Str.to_ocaml_string concated2 = concated)
      | `Error _ -> try_separators (n - 1)
      end
  in
  try_separators 300;
  ()


let utf8_specific_test () =
  let module Utf8 = Int_utf8_character in
  say "### UTF-8 Test";
  let ground_truth = [
    "$", 0x24; (* ASCII *)
    "¢", 0xA2; (* Latin-something *)
    "€", 0x20AC; (* Multi-byte *)
    "\xF0\xA4\xAD\xA2", 0x24B62; (* Another example from Wikipedia *)
    "í", 0xED; (* Spanish stuff *)
    "œ", 0x153; (* French stuff *)
    "ß", 0xDF; (* German Stuff *)
  ] in
  List.iter ground_truth (fun (s, i) ->
      let actual_test = Utf8.to_ocaml_string i in
      test_assertf (actual_test = s) "utf8_specific_test: (%S, %d) Vs %S"
        s i actual_test;
      begin match Utf8.read_from_ocaml_string ~buf:s ~index:0 with
      | Some (v, sz) ->
        test_assertf (v = i)
          "utf8_specific_test: Utf8.read_from_ocaml_string: %d <> %d" v i;
        test_assertf (sz = String.length s)
          "utf8_specific_test: Utf8.read_from_ocaml_string: size %d Vs %S" sz s;
      | None ->
        test_assertf false "utf8_specific_test: Utf8.read_from_ocaml_string fail"
      end
    );
  ()



let () =
  do_basic_test (module struct
      let test_name = "Both natives"
      module Chr = Native_char
      module Str = Native_string
    end);
  do_basic_test (module struct
      let test_name = "List of natives"
      module Chr = Native_char
      module Str = List_of (Native_char)
  end);
  do_basic_test (module struct
      let test_name = "List of UTF-8 Integers"
      module Chr = Int_utf8_character
      module Str = List_of (Int_utf8_character)
  end);
  utf8_specific_test ()
