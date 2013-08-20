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
  val can_have_wrong_char: bool
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


let test_native_subjects =
  "" :: "A" :: "\x00" :: "Invalid UTF-8: \197"
  :: "Invalid UTF-8 again: \197\000"
  :: "Invalid UTF-8 again: \197\000 "
  :: List.init 200 (fun i -> random_string (i * 4 + 1))

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

      (* This a function that displays an Str.t (extracts the
         beginning and its size(s)) *)
      let str_to_hum s =
        sprintf "[%S.%d,%dB]"
          (Str.to_native_string s |> sprintf "%s"
           |> (fun s -> String.prefix s 10))
          (Str.length s)
          (Str.to_native_string s |> String.length) in


      (* We test `Str.sub` by comparing it with an implementation
         based on `Str.get`. *)
      let subbing ~index ~length =
        let subopt = Str.sub s2 ~index ~length in
        let r = ref [] in
        for i = index to index + length - 1 do
          r := Str.get s2 ~index:i :: !r
        done;
        begin match subopt with
        | Some sub ->
          (* If `Str.sub` returned some then `r` contains all the
             characters in reverse order: *)
          let bus =
            test_assertf (List.for_all !r ((<>) None)) "sub %d %d: r has a None"
              index length;
            Str.concat ~sep:Str.empty
              (List.rev_map (List.filter_opt !r) ~f:(Str.of_character)) in
          (* We compare the result `sub` and the re-computed version `bus`: *)
          test_assertf (Str.compare sub bus = 0) "sub %s %d %d\n→ Some %s ≠ %s"
            (str_to_hum s2) index length (str_to_hum sub) (str_to_hum bus)
        | None ->
          test_assertf (!r = [] || List.exists !r ((=) None))
            "sub %s %d %d → None" (str_to_hum s2) index length
        end
      in
      subbing ~index:0 ~length:0;
      subbing ~index:0 ~length:(Str.length s2);
      subbing ~index:2 ~length:(Str.length s2);
      subbing ~index:4 ~length:(Str.length s2);
      subbing ~index:5 ~length:(Str.length s2);
      subbing ~index:0 ~length:1;
      subbing ~index:0 ~length:3;
      subbing ~index:0 ~length:30;
      subbing ~index:1 ~length:30;
      subbing ~index:2 ~length:30;
      subbing ~index:4 ~length:3;
      subbing ~index:40 ~length:3;
      subbing ~index:400 ~length:3;
      subbing ~index:0 ~length:(Str.length s2 - 0);
      subbing ~index:2 ~length:(Str.length s2 - 2);
      subbing ~index:4 ~length:(Str.length s2 - 4);
      subbing ~index:5 ~length:(Str.length s2 - 5);
      subbing ~index:0 ~length:(Str.length s2 - 0 - 1);
      subbing ~index:2 ~length:(Str.length s2 - 2 - 1);
      subbing ~index:4 ~length:(Str.length s2 - 4 - 1);
      subbing ~index:5 ~length:(Str.length s2 - 5 - 1);


    | `Error (`wrong_char_at i) ->
      (* If the conversion fails, we check that the error value points
         to an invalid character: *)
      test_assert (sprintf "test_ofto %S -> wrong char at index %d" s i)
        (Chr.read_from_native_string ~buf:s ~index:i = None)
    end;
  in
  List.iter test_native_subjects test_ofto;

  let rec try_separators n =
    let sep = random_string n in
    if n = 0
    then say "WARNING: %s -> try_separators did not try anything" test_name
    else
      begin match Str.of_native_string sep with
      | `Ok csep ->
        let viable_strings, converted =
          List.filter_map test_native_subjects (fun s ->
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

  (* This tests `make` against `length` and `get`:  *)
  for i = 0 to 100 do
    let seed = 50 * (i + 1) in
    let char = Random.int seed in
    let length = Random.int seed in
    match Chr.of_int char with
    | Some character ->
      let s = Str.make length character in
      test_assertf (Str.length s = length) "length of make";
      for j = 0 to length - 1 do
        test_assertf ((Str.get s j) = Some character) "nth char of make"
      done;
    | None -> ()
  done;

  (* We test the`Str.Make_output` functor with `Buffer.t` by writing
     directly the transformable functions and though Out.output and
     comparing the resulting buffer contents.
  *)
  let module Out = Str.Make_output(struct
      type ('a, 'b, 'c) thread = 'a
      type ('a, 'b, 'c) channel = Buffer.t
      let return x = x
      let bind x f = f x
      let output buf s =
        (* say "adding %S" s;; *)
        Buffer.add_string buf s
    end)
  in
  let buf_ground = Buffer.create 42 in
  let buf_through_str = Buffer.create 42 in
  let there_was_an_error = ref None in
  List.iter test_native_subjects (fun s ->
      match Str.of_native_string s with
      | `Ok o ->
        Out.output buf_through_str o;
        Buffer.add_string buf_ground s;
      | `Error (`wrong_char_at i) ->
        there_was_an_error := Some i);
  test_assertf (Buffer.contents buf_ground = Buffer.contents buf_through_str)
    "Str.Make_output test %S, %S"
    (Buffer.contents buf_ground)
    (Buffer.contents buf_through_str);

  (* Some tests of `for_all` and `exists`: *)
  List.iter test_native_subjects (fun str ->
      match Str.of_native_string str with
      | `Ok o ->
        test_assertf (Str.for_all o (fun _ -> true) = true) "∀ true = true";
        test_assertf (Str.for_all o (fun _ -> false) = false || Str.is_empty o)
          "∀ false in %S = false" str;
        test_assertf (Str.exists o (fun _ -> true) = true || Str.is_empty o)
          "∃ true => true";
        test_assertf (Str.exists o (fun _ -> false) = false)
          "∃ false in %S = false" str;
        let i_did_false = ref false in
        let comp = Str.for_all o (fun _ ->
            if Random.bool () then true else (i_did_false := true; false)) in
        test_assertf (comp = not !i_did_false) "random test for_all";
        let i_did_true = ref false in
        let comp = Str.exists o (fun _ ->
            if Random.bool () then (i_did_true := true; true) else false) in
        test_assertf (comp = !i_did_true) "random test exists";
      | `Error (`wrong_char_at i) -> ()
    );

  (* First some basic tests of Str.of_native_substring, then the
     bigger test with all the test strings. *)
  test_assertf (Str.of_native_substring "" ~offset:0 ~length:0 = `Ok Str.empty)
    "sub '' 0 0 = ''";
  test_assertf (Str.of_native_substring "" ~offset:0 ~length:1 = `Error `out_of_bounds)
    "sub '' 0 1 → out_of_bounds";
  test_assertf (Str.of_native_substring "" ~offset:1 ~length:0 = `Ok Str.empty)
    "sub '' 1 0 → ''";
  test_assertf (Str.of_native_substring "" ~offset:1 ~length:1 = `Error `out_of_bounds)
    "sub '' 1 1 → out_of_bounds";
  let i_have_been_to_ok = ref false in
  let i_have_been_to_wrong_char = ref false in
  let i_have_been_to_out_of_bounds = ref false in
  List.iter test_native_subjects begin fun str ->
    let offset = Random.int 42 in
    let length = Random.int 42 in
    let substr = try (String.sub str offset length) with _ -> "" in
    begin match Str.of_native_substring str ~offset ~length with
    | `Ok _ as o ->
      i_have_been_to_ok := true;
      test_assertf (o = (Str.of_native_string substr))
        "sub %S %d %d → Ok" str offset length;
    | `Error (`wrong_char_at c) ->
      i_have_been_to_wrong_char := true;
      test_assertf (Str.of_native_string substr = `Error (`wrong_char_at (c - offset)))
        "sub %S %d %d → Ok" str offset length;
    | `Error `out_of_bounds ->
      i_have_been_to_out_of_bounds := true;
      test_assertf (substr = "") "sub out_of_bounds"
    end;
  end;
  test_assertf !i_have_been_to_ok "i_have_been_to_ok";
  test_assertf (!i_have_been_to_wrong_char || not can_have_wrong_char)
    "i_have_been_to_wrong_char";
  test_assertf !i_have_been_to_out_of_bounds "i_have_been_to_out_of_bounds";

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
      let can_have_wrong_char = false
      module Chr = Native_character
      module Str = Native_string
    end);
  do_basic_test (module struct
      let test_name = "List of natives"
      let can_have_wrong_char = false
      module Chr = Native_character
      module Str = List_of (Native_character)
  end);
  do_basic_test (module struct
      let test_name = "List of UTF-8 Integers"
      let can_have_wrong_char = true
      module Chr = Int_utf8_character
      module Str = List_of (Int_utf8_character)
  end);
  utf8_specific_test ();
  exit !return_code
