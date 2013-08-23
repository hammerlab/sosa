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

  ocamlfind ocamlopt -g -I _build/ sosa.cmxa -thread -package $PACKAGES -syntax camlp4o -linkpkg -o $EXEC $ML_FILE \
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
  :: List.init 50 (fun i -> random_string (i * 4 + 1))

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

  let tried_separators = ref 0 in
  let rec try_separators n =
    if n = 0
    then
      if !tried_separators < 10 then
        say "WARNING: %s -> try_separators did not try much (%d separators)"
          test_name !tried_separators
      else ()
    else
      let sep = random_string n in
      begin match Str.of_native_string sep with
      | `Ok csep ->
        let selection =
          List.filter test_native_subjects (fun _ -> Random.bool ()) in
        let viable_strings, converted =
          List.filter_map selection (fun s ->
              match Str.of_native_string s with
              | `Ok s2 ->  Some (s, s2)
              | `Error (`wrong_char_at c) -> None)
          |> List.unzip
        in
        let concated = String.concat ~sep viable_strings in
        let concated2 = Str.concat ~sep:csep converted in
        (* say "separators %S" sep; *)
        incr tried_separators;
        test_assertf (Str.to_native_string concated2 = concated)
          "try_separators %d (%dth): %S %s →\n  %S Vs\n  %s" n !tried_separators sep
          (String.concat ~sep:", " (List.map viable_strings (sprintf "%S")))
          concated
          (Str.to_string_hum concated2);
        try_separators (n - 1)
      | `Error _ -> try_separators (n - 1)
      end
  in
  try_separators 450;

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


  (* A test of index_of_character and index_of_character_reverse, we
     create a big cartesian product
     (nat_string, (from_index, char_to_find)) and we run both searches. *)
  let froms = List.init 10 (fun i -> Random.int (i + 1)) in
  let chars = List.init 10 (fun i -> Chr.of_int i) |> List.filter_opt in
  let to_do =
    List.(cartesian_product test_native_subjects (cartesian_product froms chars))
  in
  let i_went_to_some_index = ref 0 in
  let i_went_to_none_from_length = ref 0 in
  let i_went_to_none_from_absent = ref 0 in
  let rev_i_went_to_some_index = ref 0 in
  let rev_i_went_to_none_from_length = ref 0 in
  let rev_i_went_to_none_from_absent = ref 0 in
  List.iter to_do (fun (nat, (from, char)) ->
      match Str.of_native_string nat with
      | `Ok o ->
        begin match Str.index_of_character o ~from char with
        | Some index ->
          incr i_went_to_some_index;
          test_assertf (Str.get o index = Some char) "find | get";
        | None ->
          if from > Str.length o - 1 then (incr i_went_to_none_from_length)
          else begin
            for i = from to Str.length o - 1 do
              test_assertf (Str.get o i <> Some char)
                "not found => can't find, i: %d, from : %d, length: %d"
                i from (Str.length o);
            done;
            incr i_went_to_none_from_absent;
          end
        end;
        begin match Str.index_of_character_reverse o ~from char with
        | Some index ->
          incr rev_i_went_to_some_index;
          test_assertf (Str.get o index = Some char) "rev, find | get";
        | None ->
          if from > Str.length o - 1 then (incr rev_i_went_to_none_from_length)
          else begin
            for i = from downto 0 do
              test_assertf (Str.get o i <> Some char)
                "rev, not found => can't find, i: %d, from : %d, length: %d"
                i from (Str.length o);
            done;
            incr rev_i_went_to_none_from_absent;
          end;
        end;
      | _ -> ());
  test_assertf (!i_went_to_some_index > 0) "";
  test_assertf (!i_went_to_none_from_length > 0) "";
  test_assertf (!i_went_to_none_from_absent > 0) "";
  test_assertf (!rev_i_went_to_some_index > 0) "";
  test_assertf (!rev_i_went_to_none_from_length > 0) "";
  test_assertf (!rev_i_went_to_none_from_absent > 0) "";


  (* a first test of compare_substring with special cases, empty strings,
     and small strings, containing 'a', 'c', 'g', 't' → they should
     convertible to any backend :) *)
  let test_compare_substring (a, idxa, lena) (b, idxb, lenb) expected =
    match Str.of_native_string a, Str.of_native_string b with
    | `Ok aa, `Ok bb ->
      let res = Str.compare_substring (aa, idxa, lena) (bb, idxb, lenb) in
      test_assertf (match res, expected with
        | None, None -> true
        | Some 0, Some 0 -> true
        | Some r, Some e when r * e > 0 -> true (* i.e. same sign *)
        | _, _ -> false)
        "test_compare_substring (%S, %d, %d) (%S, %d, %d) = %s ≠ %s"
        a idxa lena b idxb lenb
        (Option.value_map ~default:"None" res ~f:(sprintf "Some %d"))
        (Option.value_map ~default:"None" expected ~f:(sprintf "Some %d"))
    | _, _ -> ()
  in
  test_compare_substring ("", 0, 0) ("", 0, 0)       (Some 0);
  test_compare_substring ("", 0, 1) ("", 0, 1)       None;
  test_compare_substring ("", 0, 1) ("", 0, 0)       (Some 1);
  test_compare_substring ("", 0, 0) ("", 0, 1)       (Some (-1));
  test_compare_substring ("aaa", 0, 0) ("", 0, 0)    (Some 0);
  test_compare_substring ("aaa", 0, 0) ("ggg", 0, 0) (Some 0);
  test_compare_substring ("aaa", 1, 0) ("ggg", 1, 0) (Some 0);
  test_compare_substring ("aaa", 1, 0) ("ggg", 1, 0) (Some 0);
  test_compare_substring ("aaa", 1, 1) ("ggg", 1, 1) (Some (-1));
  test_compare_substring ("aga", 1, 1) ("ggc", 1, 1) (Some (0));
  test_compare_substring ("aga", 1, 1) ("gag", 1, 1) (Some (1));
  test_compare_substring ("aga", 1, 1) ("gcg", 1, 1) (Some (1));

  (* Now we run a bigger randomized test of compare_substring. *)
  let been_to_none = ref 0 in
  let been_to_some_0 = ref 0 in
  let been_to_some_m = ref 0 in
  List.iter test_native_subjects (fun a ->
      List.iter test_native_subjects (fun b ->
          match Str.of_native_string a, Str.of_native_string b with
          | `Ok aa, `Ok bb ->
            let rec test n =
              let length_a = Str.length aa in
              let length_b = Str.length bb in
              let lena = Random.int (length_a + 5) in
              let idxa = Random.int (lena + 5) in
              let idxb, lenb =
                if Random.bool ()
                then  (Random.int (length_b + 5), Random.int (length_b + 5))
                else (idxa, lena) (* half of the times with same params *)
              in
              let res =
                Str.compare_substring (aa, idxa, lena) (bb, idxb, lenb) in
              begin match res with
              | None ->
                test_assertf (idxa > lena || idxb > lenb
                              || lena > length_a || lenb > length_b
                              || idxa + lena > length_a
                              || idxb + lenb > length_b
                             )
                  "compare_substring: None idxa: %d, lena: %d length_a: %d \
                  \                        idxb: %d, lenb: %d length_b: %d \
                  " idxa lena length_a idxb lenb length_b;
                incr been_to_none;
              | Some 0 ->
                for i = 0 to min lena lenb - 1 do
                  test_assertf ((Str.get aa (i + idxa)) = (Str.get bb (i + idxb)))
                    "compare_substring: Some 0 but different"
                done;
                incr been_to_some_0;
              | Some m ->
                let suba = Str.sub aa ~index:idxa ~length:(min lena (max 0 (length_a - idxa))) in
                let subb = Str.sub bb ~index:idxb ~length:(min lenb (max 0 (length_b - idxb))) in
                begin match suba, subb with
                | Some sa, Some sb ->
                  test_assertf (Str.compare sa sb * m > 0
                                || (Str.compare sa sb * m = 0 && (compare lena lenb * m > 0)))
                    "Some %d instead of %d sa: %s sb:%s" m (Str.compare sa sb)
                    (Str.to_string_hum sa) (Str.to_string_hum sb);
                | _, _ ->
                  test_assertf false "not Str.sub %s %s"
                    (Option.value_map ~default:"None" ~f:(Str.to_string_hum) suba)
                    (Option.value_map ~default:"None" ~f:(Str.to_string_hum) subb)
                  ;
                  test_assertf false
                  "compare_substring: None idxa: %d, lena: %d length_a: %d \
                  \                        idxb: %d, lenb: %d length_b: %d \
                  " idxa lena length_a idxb lenb length_b;
                end;
                incr been_to_some_m;
                ()
              end;
              if n > 0 then test (n - 1);
            in
            test 4
          | _, _ -> ()
        )
    );
  test_assertf (!been_to_none > 3) "been_to_none: %d" !been_to_none;
  test_assertf (!been_to_some_0 > 3) "been_to_some_0: %d" !been_to_some_0;
  test_assertf (!been_to_some_m > 3) "been_to_some_m: %d" !been_to_some_m;



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
  do_basic_test (module struct
      let test_name = "Of_mutable(int array)"
      let can_have_wrong_char = true
      module Chr = Int_utf8_character
      module Str = Of_mutable (struct
          type character = Chr.t
          type t = int array
          let empty = [| |]
          let length = Caml.Array.length
          let make = Caml.Array.make
          let get t i = t.(i)
          let set t i c = t.(i) <- c
          let blit = Array.blit
          let compare = compare
          let compare_char = compare

          let of_native_substring natstr ~offset ~length =
            Make_native_conversions.of_native_substring
              ~empty ~init:(fun () -> ref [])
              ~on_new_character:(fun x c -> x := c :: !x)
              ~finalize:(fun x -> List.rev !x |> Array.of_list)
              ~read_character_from_native_string:Chr.read_from_native_string
              natstr ~offset ~length
          let of_native_string natstr =
            Make_native_conversions.of_native_string
              of_native_substring natstr
          let to_native_string l =
            Make_native_conversions.to_native_string_knowing_size
              ~future_size:(fun l ->
                  Array.fold l ~init:0 ~f:(fun sum c -> sum + Chr.size c))
              ~iter:Array.iter
              ~write_char_to_native_string:Chr.write_to_native_string
              l

        end)
  end);
  do_basic_test (module struct
      let test_name = "Of_mutable((char, int8, C-Layout) Bigarray.Array1.t)"
      let can_have_wrong_char = false
      open Bigarray
      type char_bigarray = (char, int8_unsigned_elt, c_layout) Array1.t
      module Chr = Native_character
      module Str = Of_mutable (struct
          type character = Chr.t
          type t = char_bigarray
          let empty = Array1.create char c_layout 0
          let length = Array1.dim
          let make len c  =
            let res = Array1.create char c_layout len in
            Array1.fill res c;
            res
          let get t i = Array1.get t i
          let set t i c = Array1.set t i c
          let blit ~src ~src_pos ~dst ~dst_pos ~len =
            Array1.(blit (sub src src_pos len) (sub dst dst_pos len))

          let compare a b =
            let len = min (length a) (length b) in
            let res = ref 0 in
            try
              for i = 0 to len - 1 do
                let cmp = compare (get a i) (get b i)  in
                if cmp = 0
                then ()
                else (res := cmp; raise Not_found)
              done;
              compare (length a) (length b)
            with _ -> !res


          let compare_char = Char.compare

          let of_native_substring natstr ~offset ~length =
            Make_native_conversions.of_native_substring
              ~empty ~init:(fun () -> ref [])
              ~on_new_character:(fun x c -> x := c :: !x)
              ~finalize:(fun x ->
                  Array1.of_array char c_layout (List.rev !x |> Array.of_list))
              ~read_character_from_native_string:Chr.read_from_native_string
              natstr ~offset ~length

          let of_native_substring natstr ~offset ~length =
            try
              let res = Array1.create char c_layout length in
              for i = 0 to length - 1 do
                Array1.set res i (natstr.[i + offset])
              done;
              `Ok res
            with _ -> `Error `out_of_bounds

          let of_native_string natstr =
            Make_native_conversions.of_native_string of_native_substring natstr

          let to_native_string l =
            let s = String.make (Array1.dim l) '0' in
            for i = 0 to (Array1.dim l) - 1 do
              s.[i] <- Array1.get l i
            done;
            s

        end)
  end);
  utf8_specific_test ();
  exit !return_code
