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
  module Chr: BASIC_CHARACTER
  module Str: BASIC_STRING with type character = Chr.t
end

let return_code = ref 0
let should_not_return_zero () = return_code := 5

let test_assert msg cond =
  if not cond then (
    should_not_return_zero ();
    say ">> TEST FAILED: [%s]" msg
  ) else ()

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

  test_assertf (Str.length Str.empty = 0) "(length empty)";
  test_assertf (Str.is_empty Str.empty) "(is_empty empty)";
  begin match Str.of_native_string "" with
  | `Ok o -> test_assertf (Str.is_empty o) "(is_empty \"\")";
  | `Error _ -> test_assertf false "Str.of_native_string %S -> Error" ""
  end;

  let test_ofto s =
    begin match Str.of_native_string s with
    | `Ok s2 ->
      (* We test that when we can transform from `s2`, the opposite
         conversion works: *)
      let back = Str.to_native_string s2 in
      test_assert (sprintf "test_ofto %S <> %S" s back) (s = back);

      (* We test `Str.fold` against a potentially *very* slow
         implementation using `Str.length` and `Str.get`. *)
      let folding ~init ~f to_string =
        let fold = Str.fold s2 ~init ~f in
        let refold =
          let r = ref init in
          for i = 0 to Str.length s2 - 1 do
            r := f !r (Option.value_exn (Str.get s2 ~index:i));
          done;
          !r in
        test_assertf (fold = refold) "\nfold: %s\nrefold: %s"
          (to_string fold) (to_string refold)
      in
      folding ~init:[] ~f:(fun p c -> c :: p)
        (fun c -> String.concat ~sep:", " (List.map c Chr.to_string_hum));
      folding ~init:42 ~f:(fun p c -> Hashtbl.hash (p, c)) (sprintf "%d");

    | `Error (`wrong_char_at i) ->
      (* If the conversion fails, we check that the error value points
         to an invalid character: *)
      test_assert (sprintf "test_ofto %S -> wrong char at index %d" s i)
        (Chr.read_from_native_string ~buf:s ~index:i = None)
    end;
  in
  List.iter random_strings test_ofto;

  let rec try_separators n =
    let sep = random_string n in
    if n = 0
    then say "WARNING: %s -> try_separators did not try anything" test_name
    else
      begin match Str.of_native_string sep with
      | `Ok csep ->
        let viable_strings, converted =
          List.filter_map random_strings (fun s ->
              match Str.of_native_string s with
              | `Ok s2 ->  Some (s, s2)
              | `Error (`wrong_char_at c) -> None)
          |> List.unzip
        in
        let concated = String.concat ~sep viable_strings in
        let concated2 = Str.concat ~sep:csep converted in
        test_assert (sprintf "try_separators %d" n)
          (Str.to_native_string concated2 = concated)
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
      let actual_test = Utf8.to_native_string i in
      test_assertf (actual_test = s) "utf8_specific_test: (%S, %d) Vs %S"
        s i actual_test;
      begin match Utf8.read_from_native_string ~buf:s ~index:0 with
      | Some (v, sz) ->
        test_assertf (v = i)
          "utf8_specific_test: Utf8.read_from_native_string: %d <> %d" v i;
        test_assertf (sz = String.length s)
          "utf8_specific_test: Utf8.read_from_native_string: size %d Vs %S" sz s;
      | None ->
        test_assertf false "utf8_specific_test: Utf8.read_from_native_string fail"
      end
    );
  ()



let () =
  do_basic_test (module struct
      let test_name = "Both natives"
      module Chr = Native_character
      module Str = Native_string
    end);
  do_basic_test (module struct
      let test_name = "List of natives"
      module Chr = Native_character
      module Str = List_of (Native_character)
  end);
  do_basic_test (module struct
      let test_name = "List of UTF-8 Integers"
      module Chr = Int_utf8_character
      module Str = List_of (Int_utf8_character)
  end);
  utf8_specific_test ();
  exit !return_code
