(*M

Tests of the Sosa library
=========================

Test Utilities
--------------

M*)

open Nonstd
module String = StringLabels
open Printf
open Sosa
open Sosa_utilities

let say fmt = printf (fmt ^^ "\n%!")

let should_do_benchmarks =
  try Sys.argv.(1) = "bench" with _ -> false

let cartesian_product list1 list2 =
  if list2 = [] then [] else
    let rec loop l1 l2 accum = match l1 with
    | [] -> accum
    | (hd :: tl) ->
      loop tl l2
        (List.rev_append
           (List.map ~f:(fun x -> (hd,x)) l2)
           accum)
    in
    List.rev (loop list1 list2 [])

let return_code = ref 0
let should_not_return_zero () = return_code := 5

let test_assert msg cond =
  if not cond then (
    should_not_return_zero ();
    say ">> TEST FAILED: [%s]" msg
  ) else ()

let test_assertf cond fmt =
  ksprintf (fun s -> test_assert s cond) fmt

let make_string = String.init

let list_dot_init l f =
  Array.init l f |> Array.to_list

let random_string i =
  let length = Random.int i in
  make_string length (fun _ -> char_of_int (Random.int 256))

let random_ascii_string i =
  let length = Random.int i in
  make_string length (fun _ -> char_of_int (Random.int 128))

let random_utf8_string i =
  let length = Random.int i in
  list_dot_init length (fun _ -> Random.int 0x10_FFFF)
  |> List.map ~f:Int_utf8_character.to_native_string
  |> String.concat ~sep:""

let test_native_subjects =
  "" :: "A" :: "\x00" :: "Invalid UTF-8: \197"
  :: "Invalid UTF-8 again: \197\000"
  :: "Invalid UTF-8 again: \197\000 "
  :: list_dot_init 20 (fun i -> random_string (i * 4 + 1))
  @  list_dot_init 20 (fun i -> random_ascii_string (i * 4 + 1))
  @  list_dot_init 20 (fun i -> random_utf8_string (i * 4 + 1))

(*M

This is a set of common denominator native strings, i.e., string that can be
converted to every other representation. We call them DNA and use only `A`,
`C`, `G`, `T` for future implementations which will be using only 2 or 4
bits per character.

M*)
let dna_test_subjects =
  let random_read _ =
    make_string (Random.int 300 + 1) (fun _ ->
        begin match Random.int 4 with
        | 0 -> 'A'
        | 1 -> 'C'
        | 2 -> 'G'
        | _ -> 'T'
        end) in
  list_dot_init 200 random_read

(*M

Benchmarks
----------

If asked on the command line, each test will run some benchmarks on the
implementation.

M*)
module Benchmark = struct

  let now () = Unix.gettimeofday ()

  let benchmarks_table = ref []

  let add ~implementation ~experiment ~result =
    match List.Assoc.get implementation !benchmarks_table with
    | Some exps ->
       exps := (experiment, result) :: !exps
    | None ->
       benchmarks_table := (implementation, ref [experiment, result]) :: !benchmarks_table

  let measure ?(repeats=1000) f =
    let start = now () in
    for i = 1 to repeats do
      f ()
    done;
    let stop = (now ()) in
    (1000. *. (stop -. start) /. (float repeats))

  let declare ?repeats ~implementation ~experiment f =
    if should_do_benchmarks then
      let time = measure ?repeats f in
       let result = sprintf "%.3f ms" time in
       add ~implementation ~experiment ~result
    else ()

  let to_string () =
    let experiments =
      List.map !benchmarks_table ~f:(fun (_, l) ->
          List.map !l ~f:(fun (e, _) -> e))
      |> List.concat |> List.dedup in
    let first_row =
      "Implementation" :: experiments
    in
    let row_widths =
      List.map first_row (fun s -> ref (String.length s)) in
    (* say "row widths: %s" (String.concat ~sep:", "
       (List.map row_widths (fun r -> sprintf "%d" !r))); *)
    let other_rows =
      List.map !benchmarks_table (fun (impl, l) ->
          let w = List.nth_exn row_widths 0 in
          w := max !w (String.length impl);
          impl :: List.mapi experiments (fun i exp ->
              let res = List.Assoc.get exp !l
                        |> Option.value_exn ~msg:"assoc experiments" in
              let w = List.nth_exn row_widths (i  + 1) in
              (* say "w: %d, i: %d lgth: %d" !w i (String.length res); *)
              w := max !w (String.length res);
              res)) in
    let row_to_string row =
      row
      |> List.mapi ~f:(fun i c ->
          (* say "%d %s %d %d" i c !(List.nth_exn row_widths i) (String.length c); *)
          sprintf "%s%s" c
            (String.make (1 + !(List.nth_exn row_widths i) - String.length c) ' '))
      |> String.concat ~sep:"  "
    in
    sprintf "%s\n%s\n%s\n"
      (first_row |> row_to_string)
      (List.map row_widths (fun s -> String.make (!s) '-')
       |> String.concat ~sep:"   ")
      (other_rows
       |> List.map ~f:row_to_string
       |> String.concat ~sep:"\n")

end

(*M

Test with First-Class Modules
-----------------------------

The function `do_basic_test` below takes a whole OCaml module implementation as
argument; `TEST_STRING` is the expected signature:

M*)

module type TEST_STRING = sig
  val test_name: string
  val can_have_wrong_char: bool
  module Chr: Api.BASIC_CHARACTER
  module Str: Api.BASIC_STRING with type character := Chr.t
end

let do_basic_test (module Test : TEST_STRING) =
  let open Test in
  say "### Test %S" test_name;

  test_assertf (Str.length Str.empty = 0) "(length empty)";
  test_assertf (Str.is_empty Str.empty) "(is_empty empty)";
  begin match Str.of_native_string "" with
  | `Ok o -> test_assertf (Str.is_empty o) "(is_empty \"\")";
  | `Error _ -> test_assertf false "Str.of_native_string %S -> Error" ""
  end;

  begin (* test of_/to_ native_string *)
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
              r := f !r (Option.value_exn ~msg:"folding" (Str.get s2 ~index:i));
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
             |> (fun s -> String.sub s 0 (min (String.length s) 10)))
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
  end;

  begin (* test concat and a bit more *)
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
            let zipped =
              List.filter_map selection (fun s ->
                match Str.of_native_string s with
                | `Ok s2 ->  Some (s, s2)
                | `Error (`wrong_char_at c) -> None) in
            List.map zipped ~f:fst, List.map zipped ~f:snd
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
    try_separators 800;
  end;

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

  begin (* We test the`Str.Make_output` functor with `Buffer.t` by writing
           directly the transformable functions and though Out.output and
           comparing the resulting buffer contents.  *)
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
  end;

  begin (* Some tests of `for_all` and `exists`: *)
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
  end;

  begin (* of_native_substring *)
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
  end;

  let int_list_to_string l =
    sprintf "[%s]"
    (List.map ~f:Int.to_string l |> String.concat ~sep:",") in

  let str_to_int_list s =
    Str.to_character_list s |> List.map ~f:Chr.to_int in

  let int_option_to_string io =
    Option.value_map ~default:"None" ~f:(sprintf "Some %d") io in

  begin (* index_of_character{,_reverse} *)
    let test ?from ?should_find l c =
      let s = List.filter_map l Chr.of_int |>  Str.of_character_list in
      let ch =
        Option.value_exn ~msg:"test index_of_character" (Chr.of_int c) in
      let res = Str.index_of_character ?from s ch in
      test_assertf (res = should_find)
        "index_of_character: %s (from: %s) expects  %s but got %s"
        (str_to_int_list s |> int_list_to_string)
        (int_option_to_string from)
        (int_option_to_string should_find)
        (int_option_to_string res);
      if from = None then (
        let from = Some 0 in
        let res = Str.index_of_character ?from s ch in
        test_assertf (res = should_find)
          "index_of_character: %s (added-from: %s) expects  %s but got %s"
          (str_to_int_list s |> int_list_to_string)
          (int_option_to_string from)
          (int_option_to_string should_find)
          (int_option_to_string res);
      );
    in
    test [] 0;
    test [1] 0;
    test [1;2;3;4] 0;
    test [0] 0 ~should_find:0;
    test [1;2;0] 0 ~should_find:2;
    test ~from:1 [] 0;
    test ~from:1 [1] 0;
    test ~from:1 [1;2;3;4] 0;
    test ~from:1 [0] 0;
    test ~from:1 [1;2;0] 0 ~should_find:2;
    test ~from:(-1) [] 0;
    test ~from:(-1) [1] 0;
    test ~from:(-1) [1;2;3;4] 0;
    test ~from:(-1) [0] 0 ~should_find:0;
    test ~from:(-1) [1;2;0] 0 ~should_find:2;
    test ~from:4 [] 0;
    test ~from:4 [1] 0;
    test ~from:4 [1;2;3;4] 0;
    test ~from:4 [0] 0;
    test ~from:4 [1;2;0] 0;

    let test ?from ?should_find l c =
      let s = List.filter_map l Chr.of_int |>  Str.of_character_list in
      let ch =
        Option.value_exn ~msg:"test index_of_character" (Chr.of_int c) in
      let res = Str.index_of_character_reverse ?from s ch in
      test_assertf (res = should_find)
        "index_of_character_reverse: %s (from: %s) expects  %s but got %s"
        (str_to_int_list s |> int_list_to_string)
        (int_option_to_string from)
        (int_option_to_string should_find)
        (int_option_to_string res);
      if from = None then (
        let from = Some (Str.length s - 1) in
        let res = Str.index_of_character_reverse ?from s ch in
        test_assertf (res = should_find)
          "index_of_character_reverse: %s (added-from: %s) expects  %s but got %s"
          (str_to_int_list s |> int_list_to_string)
          (int_option_to_string from)
          (int_option_to_string should_find)
          (int_option_to_string res);
      );
    in
    test [] 0;
    test [1] 0;
    test [1;2;3;4] 0;
    test [0] 0 ~should_find:0;
    test [1;2;0] 0 ~should_find:2;
    test ~from:1 [] 0;
    test ~from:1 [1] 0;
    test ~from:1 [1;2;3;4] 0;
    test ~from:1 [0] 0 ~should_find:0;
    test ~from:1 [1;2;0] 0;
    test ~from:1 [1;2;0] 1 ~should_find:0;
    test ~from:(-1) [] 0;
    test ~from:(-1) [1] 0;
    test ~from:(-1) [1;2;3;4] 0;
    test ~from:(-1) [0] 0;
    test ~from:(-1) [1;2;0] 0;
    test ~from:4 [] 0;
    test ~from:4 [1] 0;
    test ~from:4 [1;2;3;4] 0;
    test ~from:4 [0] 0 ~should_find:0;
    test ~from:4 [1;2;0] 0 ~should_find:2;


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
  end;

  begin (* Test compare_substring{_strict} *)
    (* A first test of compare_substring{_strict} with special cases, empty strings,
           and small strings, containing 'a', 'c', 'g', 't' → they should
           be convertible to any backend :) *)
    let is_equivalent resopt expected =
      (match resopt with
       | None -> false
       | Some r -> r = expected || r * expected > 0) in
    let test_compare_substring (a, idxa, lena) (b, idxb, lenb) expected =
      match Str.of_native_string a, Str.of_native_string b with
      | `Ok aa, `Ok bb ->
        let res = Str.compare_substring (aa, idxa, lena) (bb, idxb, lenb) in
        test_assertf (res = expected || res * expected > 0) (* We test for the sign *)
          "test_compare_substring (%S, %d, %d) (%S, %d, %d) = %d, × %d < 0"
          a idxa lena b idxb lenb res expected;
        let resopt = Str.compare_substring_strict (aa, idxa, lena) (bb, idxb, lenb) in
        test_assertf (is_equivalent resopt expected)
          "test_compare_substring_strict (%S, %d, %d) (%S, %d, %d) = %d, × %d < 0"
          a idxa lena b idxb lenb res expected;
        (* And now check commutativity: *)
        let invres = Str.compare_substring (bb, idxb, lenb) (aa, idxa, lena) in
        test_assertf (invres = (~- expected) || invres * expected < 0) (* We test for the sign *)
          "test_compare_substring, commutes (%S, %d, %d) (%S, %d, %d) × -1 = %d, × %d < 0"
          a idxa lena b idxb lenb res expected;
        let resopt = Str.compare_substring_strict (bb, idxb, lenb) (aa, idxa, lena) in
        test_assertf (is_equivalent resopt (~- expected))
          "test_compare_substring_strict, commutes (%S, %d, %d) (%S, %d, %d) = %d, × %d < 0"
          a idxa lena b idxb lenb res expected;
      | _, _ -> test_assertf false "assumption about ACGT is wrong"
    in
    (* Semantically well-defined tests: *)
    test_compare_substring ("", 0, 0) ("", 0, 0)       ( 0);
    test_compare_substring ("aaa", 0, 0) ("", 0, 0)    ( 0);
    test_compare_substring ("aaa", 0, 0) ("ggg", 0, 0) ( 0);
    test_compare_substring ("aaa", 1, 0) ("ggg", 1, 0) ( 0);
    test_compare_substring ("aaa", 1, 0) ("ggg", 1, 0) ( 0);
    test_compare_substring ("aaa", 0, 0) ("ggg", 0, 1) (-1);
    test_compare_substring ("aaa", 1, 0) ("ggg", 1, 1) (-1);
    test_compare_substring ("aaa", 1, 0) ("ggg", 1, 1) (-1);
    test_compare_substring ("aaa", 1, 1) ("ggg", 1, 1) (-1);
    test_compare_substring ("aga", 1, 1) ("ggc", 1, 1) ( 0);
    test_compare_substring ("aga", 1, 1) ("gag", 1, 1) ( 1);
    test_compare_substring ("aga", 1, 1) ("gcg", 1, 1) ( 1);
    test_compare_substring ("aagg", 2, 2) ("gg", 0, 2) ( 0);
    (* A test of the out-of-bounds behavior: *)
    let test_compare_substring_strictness (a, idxa, lena) =
      match Str.of_native_string a, Str.of_native_string "acgt" with
      | `Ok aa, `Ok bb ->
        test_assertf (Str.compare_substring_strict (aa, idxa, lena) (Str.empty, 0, 0) = None)
          "Str.compare_substring_strict out_of_bounds 1";
        test_assertf (Str.compare_substring_strict (aa, idxa, lena) (bb, 1, 2) = None)
          "Str.compare_substring_strict out_of_bounds 2";
        test_assertf (Str.compare_substring_strict (Str.empty, 0, 0) (aa, idxa, lena) = None)
          "Str.compare_substring_strict out_of_bounds 3";
        test_assertf (Str.compare_substring_strict (bb, 1, 2) (aa, idxa, lena) = None)
          "Str.compare_substring_strict out_of_bounds 4";
      | _ -> test_assertf false "assumption about ACGT is wrong"
    in
    test_compare_substring_strictness ("", 0, 1);
    test_compare_substring_strictness ("a", 1, 1);
    test_compare_substring_strictness ("a", -1, 1);
    test_compare_substring_strictness ("a", 1, -1);
    test_compare_substring_strictness ("a", -1, -1);
    test_compare_substring_strictness ("aa", 10, 1);
    test_compare_substring_strictness ("aa", 10, -1);
    (* Now we run a bigger randomized test of compare_substring{_strict}. *)
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
                let resopt =
                  Str.compare_substring_strict (aa, idxa, lena) (bb, idxb, lenb) in
                begin match res with
                | 0 -> (* EQUAL *)
                  test_assertf (lena = lenb)
                    "compare_substring: equal but lengths %d ≠ %d" lena lenb;
                  for i = 0 to min lena lenb - 1 do
                    test_assertf ((Str.get aa (i + idxa)) = (Str.get bb (i + idxb)))
                      "compare_substring: equal but different …"
                  done;
                  test_assertf (is_equivalent resopt res)
                    "resopt Vs res in EQUAL case";
                  incr been_to_some_0;
                | m ->
                  let suba = Str.sub aa ~index:idxa ~length:(lena) in
                  let subb = Str.sub bb ~index:idxb ~length:(lenb) in
                  begin match suba, subb with
                  | Some sa, Some sb ->
                    (* Well defined case: "sub" returns something for both *)
                    test_assertf (Str.compare sa sb * m > 0)
                      "%d instead of %d sa: %s sb:%s (lena: %d, lenb: %d)" m (Str.compare sa sb)
                      (Str.to_string_hum sa) (Str.to_string_hum sb) lena lenb;
                    test_assertf (is_equivalent resopt res)
                      "strict: %s instead of %d sa: %s sb:%s (lena: %d, lenb: %d)"
                      (Option.value_map ~default:"None" resopt ~f:(sprintf "Some %d"))
                      (Str.compare sa sb)
                      (Str.to_string_hum sa) (Str.to_string_hum sb) lena lenb;
                  | _, _ -> ()
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
    test_assertf (!been_to_some_0 > 5) "been_to_some_0: %d" !been_to_some_0;
    test_assertf (!been_to_some_m > 5) "been_to_some_m: %d" !been_to_some_m;
  end;

  begin (* We test index_of_string and index_of_string_reverse, if we expect
           the same result we may give only `~expect` if not, we use
           `~expect_rev`. *)
    let test_index_of_string ?from ?sub_index ?sub_length ?expect_rev s ~sub ~expect =
      match Str.of_native_string s, Str.of_native_string sub with
      | `Ok t, `Ok subt ->
        let res = Str.index_of_string t ~sub:subt ?from ?sub_index ?sub_length in
        let shopt = Option.value_map ~f:(sprintf "Some %d") ~default:"None" in
        test_assertf (res = expect)
          "Str.index_of_string %s ~sub:%s ?from:%s ?sub_index:%s ?sub_length:%s gave %s not %s"
          s sub (shopt from) (shopt sub_index) (shopt sub_length)
          (shopt res) (shopt expect);
        let erev = match expect_rev with None -> expect | Some opt -> opt in
        let res = Str.index_of_string_reverse t ~sub:subt ?from ?sub_index ?sub_length in
        test_assertf (res = erev)
          "Str.index_of_string_reverse %s ~sub:%s ?from:%s ?sub_index:%s ?sub_length:%s gave %s not %s"
          s sub (shopt from) (shopt sub_index) (shopt sub_length)
          (shopt res) (shopt erev);
      | _, _ -> ()
    in
    test_index_of_string "aaaa" ~sub:"cc" ~expect:None;
    test_index_of_string "aaaa" ~sub:"aa" ~expect:(Some 0) ~expect_rev:(Some 2);
    test_index_of_string "ccaa" ~sub:"aa" ~expect:(Some 2);
    test_index_of_string "cccca" ~sub:"aa" ~expect:(None);
    test_index_of_string "aacca" ~from:1 ~sub:"aa" ~expect:(None) ~expect_rev:(Some 0);
    test_index_of_string "aaccaa" ~from:1 ~sub:"aa" ~expect:(Some 4) ~expect_rev:(Some 0);
    test_index_of_string "aacca" ~from:1 ~sub:"aa" ~sub_index:1 ~expect:(Some 1) ~expect_rev:(Some 1);
    test_index_of_string "aacca" ~from:2 ~sub:"aa" ~sub_index:1 ~expect:(Some 4) ~expect_rev:(Some 1);
    test_index_of_string "aacca" ~from:2 ~sub:"aa" ~sub_length:1 ~expect:(Some 4) ~expect_rev:(Some 1);
    test_index_of_string "aacca" ~from:2 ~sub:"aa" ~sub_length:0 ~expect:(Some 2) ~expect_rev:(Some 2);
    test_index_of_string "aacca" ~from:2 ~sub:""  ~expect:(Some 2) ~expect_rev:(Some 2);
    test_index_of_string "caaa" ~from:(-1) ~sub:"aa" ~expect:(Some 1) ~expect_rev:None;
    test_index_of_string "aaaa" ~from:3 ~sub:"aa" ~expect:None ~expect_rev:(Some 2);
    test_index_of_string "aaaa" ~from:4 ~sub:"aa" ~expect:None ~expect_rev:(Some 2);
    test_index_of_string "aaaa" ~from:5 ~sub:"aa" ~expect:None ~expect_rev:(Some 2);
    test_index_of_string "caaa" ~sub_index:(-1) ~sub:"aa" ~expect:(Some 1) ~expect_rev:(Some 2);
    test_index_of_string "aaaa" ~sub_index:2 ~sub:"aa" ~expect:(Some 0) ~expect_rev:(Some 3);
    (* ┗▶ This is  searching the empty string ! Find everywhere. *)
    test_index_of_string "caaa" ~sub_index:3 ~sub:"aa" ~expect:(Some 0) ~expect_rev:(Some 3);
    (* ┗▶ This is also searching the empty string ! *)
    test_index_of_string "caaa" ~sub_index:1 ~sub_length:3 ~sub:"aa" ~expect:(Some 1) ~expect_rev:(Some 3);
    test_index_of_string "caaa" ~sub_index:(-1) ~sub_length:3 ~sub:"aa" ~expect:(Some 1) ~expect_rev:(Some 2);
  end;

  begin (* Test `filter_map` *)
    let test ?from ?length l ~f ~expect fmt =
      let name = ksprintf (fun s -> s) fmt in
      (* say "test: %s l : %d" name (List.length l); *)
      let s =
        List.filter_map l Chr.of_int |>  Str.of_character_list in
      (* say "test: %s s: %d" name (Str.length s); *)
      let filtered =
        Str.filter_map ?from ?length s ~f:(fun c ->
            (* say "Chr: %d" (Chr.to_int c); *)
            match f (Chr.to_int c) with
            | Some i -> Chr.of_int i
            | None -> None)
      in
      let res_ints =
        Str.to_character_list filtered |> List.map ~f:Chr.to_int in
      let before =
        Str.to_character_list s |> List.map ~f:Chr.to_int in
      test_assertf (expect = res_ints)
        "test_filter_map: [%s=%s] → [%s] <> [%s] (%s)"
        (List.map l (sprintf "%d") |> String.concat ~sep:",")
        (List.map before (sprintf "%d") |> String.concat ~sep:",")
        (List.map res_ints (sprintf "%d") |> String.concat ~sep:",")
        (List.map expect (sprintf "%d") |> String.concat ~sep:",")
        name;
    in
    let some = fun s -> Some s in
    test [] ~f:some ~expect:[] "all empty";
    test [1]  ~f:some ~expect:[1] "some 1";
    test [1;2;3]  ~f:some ~expect:[1;2;3] "some 123";
    test [1;1;1]  ~f:some ~expect:[1;1;1] "some 111";
    test []  ~f:(fun _ -> None) ~expect:[] "none";
    test [1]  ~f:(fun _ -> None) ~expect:[] "none";
    test [1;2;3]  ~f:(fun _ -> None) ~expect:[] "none";
    let opt_of_cond c = fun x -> if c x then Some x else None in
    test [1;2;3]  ~f:(opt_of_cond ((<) 1)) ~expect:[2;3] "opt_of_cond _ > 1";
    test [1;2;3]  ~f:(opt_of_cond ((<) 2)) ~expect:[3] "opt_of_cond _ > 2";
    test [1;2;3] ~from:1 ~f:(opt_of_cond ((<) 1)) ~expect:[2;3] "opt_of_cond _ > 1 from 1";
    test [1;2;3] ~from:2 ~f:(opt_of_cond ((<) 1)) ~expect:[3] "opt_of_cond _ > 1 from 2";
    test [1;2;3] ~from:3 ~f:(opt_of_cond ((<) 1)) ~expect:[] "opt_of_cond _ > 1 from 3";
    test [1;2;3] ~from:4 ~f:(opt_of_cond ((<) 1)) ~expect:[] "opt_of_cond _ > 1 from 4";
    test [1;2;3]  ~length:0 ~f:(opt_of_cond ((<) 1)) ~expect:[] "opt_of_cond _ > 1 length 0";
    test [1;2;3]  ~length:1 ~f:(opt_of_cond ((<) 1)) ~expect:[] "opt_of_cond _ > 1 length 1";
    test [1;2;3]  ~length:2 ~f:(opt_of_cond ((<) 1)) ~expect:[2] "opt_of_cond _ > 1 length 2";
    test [1;2;3]  ~length:3 ~f:(opt_of_cond ((<) 1)) ~expect:[2;3] "opt_of_cond _ > 1 length 3";
    test [1;2;3]  ~length:4 ~f:(opt_of_cond ((<) 1)) ~expect:[2;3] "opt_of_cond _ > 1 length 4";
    test [1;2;3]  ~from:2 ~length:2 ~f:(opt_of_cond ((<) 1)) ~expect:[3] "opt_of_cond _ > 1";
  end;
  begin (* Test `filter` *)
    let test ?from ?length l ~f ~expect fmt =
      let name     = ksprintf (fun s -> s) fmt in
      (* say "test: %s l : %d" name (List.length l); *)
      let s        = List.filter_map l Chr.of_int |> Str.of_character_list in
      (* say "test: %s s: %d" name (Str.length s); *)
      let filtered = Str.filter ?from ?length s ~f:(fun c -> f (Chr.to_int c)) in
      let res_ints = Str.to_character_list filtered |> List.map ~f:Chr.to_int in
      let before   = Str.to_character_list s |> List.map ~f:Chr.to_int in
      let pp l     = List.map l (sprintf "%d") |> String.concat ~sep:";" in
      test_assertf (expect = res_ints)
        "test_filter: [%s=%s] → [%s] <> [%s] (%s)"
        (pp l) (pp before) (pp res_ints) (pp expect)
        name;
    in
    let always_true _   = true in
    let always_false _  = false in
    test [] ~f:always_true ~expect:[] "all empty";
    test [1]  ~f:always_true ~expect:[1] "true 1";
    test [1;2;3]  ~f:always_true ~expect:[1;2;3] "true 123";
    test [1;1;1]  ~f:always_true ~expect:[1;1;1] "some 111";
    test []  ~f:always_false ~expect:[] "all empty";
    test [1]  ~f:always_false ~expect:[] "false 1";
    test [1;2;3]  ~f:always_false ~expect:[] "false 123";
    let opt_of_cond c = fun x -> c x in
    test [1;2;3]  ~f:(opt_of_cond ((<) 1)) ~expect:[2;3] "opt_of_cond _ > 1";
    test [1;2;3]  ~f:(opt_of_cond ((<) 2)) ~expect:[3] "opt_of_cond _ > 2";
    test [1;2;3] ~from:1 ~f:(opt_of_cond ((<) 1)) ~expect:[2;3] "opt_of_cond _ > 1 from 1";
    test [1;2;3] ~from:2 ~f:(opt_of_cond ((<) 1)) ~expect:[3] "opt_of_cond _ > 1 from 2";
    test [1;2;3] ~from:3 ~f:(opt_of_cond ((<) 1)) ~expect:[] "opt_of_cond _ > 1 from 3";
    test [1;2;3] ~from:4 ~f:(opt_of_cond ((<) 1)) ~expect:[] "opt_of_cond _ > 1 from 4";
    test [1;2;3]  ~length:0 ~f:(opt_of_cond ((<) 1)) ~expect:[] "opt_of_cond _ > 1 length 0";
    test [1;2;3]  ~length:1 ~f:(opt_of_cond ((<) 1)) ~expect:[] "opt_of_cond _ > 1 length 1";
    test [1;2;3]  ~length:2 ~f:(opt_of_cond ((<) 1)) ~expect:[2] "opt_of_cond _ > 1 length 2";
    test [1;2;3]  ~length:3 ~f:(opt_of_cond ((<) 1)) ~expect:[2;3] "opt_of_cond _ > 1 length 3";
    test [1;2;3]  ~length:4 ~f:(opt_of_cond ((<) 1)) ~expect:[2;3] "opt_of_cond _ > 1 length 4";
    test [1;2;3]  ~from:2 ~length:2 ~f:(opt_of_cond ((<) 1)) ~expect:[3] "opt_of_cond _ > 1";
  end;


  begin (* Test the `split` function *)
    let test l ~on ~expect =
      let s = List.filter_map l Chr.of_int |>  Str.of_character_list in
      let on_converted =
        match on with
        | `C c ->
          `Character (Option.value_exn ~msg:"Chr.of_int" (Chr.of_int c))
        | `S l ->
          `String (List.filter_map l Chr.of_int |>  Str.of_character_list)
      in
      let res = Str.split s ~on:on_converted in
      let res_list = List.map  res ~f:str_to_int_list
      in
      test_assertf (res_list = expect)
        "split: l: %s = %s on:(%s)\n    expect: {%s}\n     res: {%s}: %s."
        (int_list_to_string l)
        (Str.to_string_hum s)
        (match on with
         | `C c -> sprintf "`Character %d"  c
         | `S s -> sprintf "`Bytes %s" (int_list_to_string s))
        (List.map ~f:int_list_to_string expect |> String.concat ~sep:" -- ")
        (List.map ~f:int_list_to_string res_list |> String.concat ~sep:" -- ")
        (List.map ~f:Str.to_string_hum res |> String.concat ~sep:"; ")
    in
    let on_one t ~expect =
      test t ~on:(`C 1) ~expect;
      test t ~on:(`S [1]) ~expect;
    in
    on_one [] ~expect:[[]];
    on_one [2;3;] ~expect:[[2;3]];
    on_one [1] ~expect:[[]; []];
    on_one [2;1;2;3;4] ~expect:[[2]; [2;3;4]];
    on_one [2;1;2;3;4;1] ~expect:[[2]; [2;3;4]; []];
    on_one [1;2;1;2;3;4;1] ~expect:[[]; [2]; [2;3;4]; []];
    on_one [1;1;2;1;2;3;4;1] ~expect:[[]; []; [2]; [2;3;4]; []];

    let on_123 t ~expect =
      test t ~on:(`S [1;2;3]) ~expect;
    in
    on_123 [] ~expect:[ [] ];
    on_123 [1] ~expect:[ [1] ];
    on_123 [1;2;4;5] ~expect:[ [1;2;4;5;] ];
    on_123 [1;2;3] ~expect:[[]; []];
    on_123 [2;1;2;3;4] ~expect:[[2]; [4]];
    on_123 [2;1;2;3;4;1] ~expect:[[2]; [4;1]];
    on_123 [1;2;1;2;3;4;1] ~expect:[[1;2]; [4;1]];
    on_123 [1;2;3;1;2;1;2;3;4;1] ~expect:[[]; [1;2]; [4;1]];

    let on_empty t ~expect =
      test t ~on:(`S []) ~expect;
    in
    on_empty [] ~expect:[ [] ];
    on_empty [1] ~expect:[ [1] ];
    on_empty [1;2;4;5] ~expect:[ [1]; [2]; [4]; [5] ];
  end;

  begin (* Test `find` *)
    let test ?from ?length ?should_find l ~f =
      let s = List.filter_map l Chr.of_int |>  Str.of_character_list in
      let f x = f (Chr.to_int x) in
      let res = Str.find ?from ?length s ~f in
      test_assertf (res = should_find)
        "find: %s (%s, %s) expects  %s but got %s"
        (str_to_int_list s |> int_list_to_string)
        (int_option_to_string from)
        (int_option_to_string length)
        (int_option_to_string should_find)
        (int_option_to_string res);
      if from = None then (
        let from = Some 0 in
        let res = Str.find ?from ?length s ~f in
        test_assertf (res = should_find)
          "find: %s (%s →, %s) expects  %s but got %s"
          (str_to_int_list s |> int_list_to_string)
          (int_option_to_string from)
          (int_option_to_string length)
          (int_option_to_string should_find)
          (int_option_to_string res);
      );
    in
    test [] ~f:(fun _ -> true);
    test [] ~f:(fun _ -> false);
    test [1] ~f:(fun _ -> true) ~should_find:0;
    test [1] ~f:(fun _ -> false);
    test [1;2;3] ~f:(fun _ -> true) ~should_find:0;
    test [1;2;3] ~f:(fun _ -> false);
    test [1;2;3] ~f:(fun x -> x > 1) ~should_find:1;
    test [1;2;3] ~f:(fun x -> x < 1);
    test [1;1;1;2;3] ~from:2 ~f:(fun x -> x > 1) ~should_find:3;
    test [1;1;1;2;3] ~from:2 ~f:(fun x -> x < 1);
    test [1;1;1;2;3] ~from:2 ~f:(fun x -> x <= 1) ~should_find:2;
    test [1;2;3]     ~from:2 ~f:(fun _ -> true) ~should_find:2;
    test [1;2;3]     ~from:3 ~f:(fun _ -> true);
    test [1;2;3]     ~from:(-2) ~f:(fun _ -> true) ~should_find:0;
    test [] ~length:0 ~f:(fun _ -> true);
    test [] ~length:0 ~f:(fun _ -> false);
    test [1;2;] ~length:0 ~f:(fun _ -> true);
    test [1;2;] ~length:0 ~f:(fun _ -> false);
    test [1;2;] ~length:1 ~f:(fun _ -> true) ~should_find:0;
    test [1;2;] ~length:1 ~f:(fun _ -> false);
    test [1;2;] ~from:1 ~length:1 ~f:(fun _ -> true) ~should_find:1;
    test [1;2;] ~from:2 ~length:1 ~f:(fun _ -> true);
    test [1;2;3;] ~from:2 ~length:1 ~f:(fun _ -> true) ~should_find:2;
    test [1;2;] ~from:1 ~length:2 ~f:(fun _ -> true) ~should_find:1;
    test [1;2;] ~from:0 ~length:(-1) ~f:(fun _ -> true);
    test [1;2;3;4] ~from:3 ~length:2 ~f:(fun _ -> true) ~should_find:3;
  end;

  begin (* Test `find_reverse` *)
    let test ?from ?length ?should_find l ~f =
      let s = List.filter_map l Chr.of_int |>  Str.of_character_list in
      let f x = f (Chr.to_int x) in
      let res = Str.find_reverse ?from ?length s ~f in
      test_assertf (res = should_find)
        "find_reverse: %s (%s, %s) expects  %s but got %s"
        (str_to_int_list s |> int_list_to_string)
        (int_option_to_string from)
        (int_option_to_string length)
        (int_option_to_string should_find)
        (int_option_to_string res);
      if from = None then (
        let from = Some (Str.length s - 1) in
        let res = Str.find_reverse ?from ?length s ~f in
        test_assertf (res = should_find)
          "find_reverse: %s (%s → added, %s) expects  %s but got %s"
          (str_to_int_list s |> int_list_to_string)
          (int_option_to_string from)
          (int_option_to_string length)
          (int_option_to_string should_find)
          (int_option_to_string res);
      );
    in
    test [] ~f:(fun _ -> true);
    test [] ~f:(fun _ -> false);
    test [1] ~f:(fun _ -> true) ~should_find:0;
    test [1] ~f:(fun _ -> false);
    test [1;2;3] ~f:(fun _ -> true) ~should_find:2;
    test [1;2;3] ~f:(fun _ -> false);
    test [1;2;3] ~f:(fun x -> x <= 2) ~should_find:1;
    test [1;2;3] ~f:(fun x -> x < 1);
    test [1;1;1;2;3] ~from:2 ~f:(fun x -> x > 1);
    test [1;1;1;2;3] ~from:2 ~f:(fun x -> x < 1);
    test [1;1;1;2;3] ~from:2 ~f:(fun x -> x <= 1) ~should_find:2;
    test [1;2;3]     ~from:2 ~f:(fun _ -> true) ~should_find:2;
    test [1;2;3]     ~from:3 ~f:(fun _ -> true) ~should_find:2;
    test [1;2;3]     ~from:(-2) ~f:(fun _ -> true);
    test [] ~length:0 ~f:(fun _ -> true);
    test [] ~length:0 ~f:(fun _ -> false);
    test [1;2;] ~length:0 ~f:(fun _ -> true);
    test [1;2;] ~length:0 ~f:(fun _ -> false);
    test [1;2;] ~length:1 ~f:(fun _ -> true) ~should_find:1;
    test [1;2;] ~length:1 ~f:(fun _ -> false);
    test [1;2;] ~from:1 ~length:1 ~f:(fun _ -> true) ~should_find:1;
    test [1;2;] ~from:2 ~length:1 ~f:(fun _ -> true) ~should_find:1;
    test [1;2;3;] ~from:2 ~length:1 ~f:(fun _ -> true) ~should_find:2;
    test [1;2;] ~from:1 ~length:2 ~f:(fun _ -> true) ~should_find:1;
    test [1;2;] ~from:0 ~length:(-1) ~f:(fun _ -> true);
    test [1;2;3;4] ~from:3 ~length:2 ~f:(fun _ -> true) ~should_find:3;
  end;

  begin (* Test `strip` *)
    let test ?on ~whitespace l ~expect =
      let s = List.filter_map l Chr.of_int |>  Str.of_character_list in
      let expect_str =
        List.filter_map expect Chr.of_int |>  Str.of_character_list in
      let whitespace x = List.mem (Chr.to_int x) whitespace in
      let res = Str.strip ?on ~whitespace s in
      test_assertf (res = expect_str)
        "strip: %s ~on:%s expects %s but got %s"
        (str_to_int_list s |> int_list_to_string)
        (match on with Some `Both -> "Both" | Some `Left -> "Left"
                     | Some `Right -> "Right" | None -> "None")
        (expect |> int_list_to_string)
        (str_to_int_list res |> int_list_to_string)
      in
      let whitespace = [0;1] in
      test ?on:None   ~whitespace [] ~expect:[];
      test ~on:`Both  ~whitespace [] ~expect:[];
      test ~on:`Left  ~whitespace [] ~expect:[];
      test ~on:`Right ~whitespace [] ~expect:[];
      test ?on:None   ~whitespace [2] ~expect:[2];
      test ~on:`Both  ~whitespace [2] ~expect:[2];
      test ~on:`Left  ~whitespace [2] ~expect:[2];
      test ~on:`Right ~whitespace [2] ~expect:[2];
      test ?on:None   ~whitespace [0;1;2;2] ~expect:[    2;2];
      test ~on:`Both  ~whitespace [0;1;2;2] ~expect:[    2;2];
      test ~on:`Left  ~whitespace [0;1;2;2] ~expect:[    2;2];
      test ~on:`Right ~whitespace [0;1;2;2] ~expect:[0;1;2;2];
      test ?on:None   ~whitespace [0;1;2;2;3;1;0;1] ~expect:[    2;2;3;     ];
      test ~on:`Both  ~whitespace [0;1;2;2;3;1;0;1] ~expect:[    2;2;3;     ];
      test ~on:`Left  ~whitespace [0;1;2;2;3;1;0;1] ~expect:[    2;2;3;1;0;1];
      test ~on:`Right ~whitespace [0;1;2;2;3;1;0;1] ~expect:[0;1;2;2;3;     ];
      test ?on:None   ~whitespace [0;1;1;0;1] ~expect:[];
      test ~on:`Both  ~whitespace [0;1;1;0;1] ~expect:[];
      test ~on:`Left  ~whitespace [0;1;1;0;1] ~expect:[];
      test ~on:`Right ~whitespace [0;1;1;0;1] ~expect:[];
      let whitespace = [] in
      test ?on:None   ~whitespace [] ~expect:[];
      test ~on:`Both  ~whitespace [] ~expect:[];
      test ~on:`Left  ~whitespace [] ~expect:[];
      test ~on:`Right ~whitespace [] ~expect:[];
      test ?on:None   ~whitespace [2;0;1;2] ~expect:[2;0;1;2];
      test ~on:`Both  ~whitespace [2;0;1;2] ~expect:[2;0;1;2];
      test ~on:`Left  ~whitespace [2;0;1;2] ~expect:[2;0;1;2];
      test ~on:`Right ~whitespace [2;0;1;2] ~expect:[2;0;1;2];
  end;

  begin (* Test `take_while{,_with_index}` *)
    List.iter ~f:(fun subject ->
        match Str.of_native_string subject with
        | `Error _ -> ()
        | `Ok s ->
          test_assertf (Str.take_while s ~f:(fun _ -> true) = s)
            "take_while-true (%S)" subject;
          test_assertf (Str.take_while s ~f:(fun _ -> false) = Str.empty)
            "take_while-false (%S)" subject;
          let length = Random.int (Str.length s + 1) in
          test_assertf (Str.take_while_with_index s ~f:(fun idx _ -> idx < length)
                        = Str.sub_exn s ~index:0 ~length)
            "take_while < length (%S)" subject;
          let rint = Random.int 42 in
          test_assertf (
            let prefix = Str.take_while s ~f:(fun c -> Chr.to_int c < rint) in
            Str.fold prefix ~init:true ~f:(fun prev c ->
                prev && Chr.to_int c < rint))
            "take_while: char < random-int (%S)" subject;
      ) test_native_subjects;
  end;

  let ints_to_str x = List.filter_map x Chr.of_int |> Str.of_character_list
  and optionMap f   = function | None   -> None
                               | Some s -> f s
  and defOptMap d f = function | None   -> d
                               | Some s -> f s in
  let pp_int_opt o  = defOptMap "None" (fun x -> "Some " ^ string_of_int x) o in

  begin (* Test slice *)
    let test ?start ?finish l ~expect =
      let s   = ints_to_str l in
      let exp = optionMap (fun e -> Some (ints_to_str e)) expect
      and res = Str.slice ?start ?finish s
      in
      test_assertf (res = exp)
        "slice: %s ?start:(%s) ?finish:(%s) expects %s but got %s"
          (int_list_to_string l)
          (pp_int_opt start)
          (pp_int_opt finish)
          (defOptMap "None" int_list_to_string expect)
          (defOptMap "None" (fun r -> str_to_int_list r |> int_list_to_string) res)
    in
    test                          []       ~expect:(Some []);
    test                          [1]      ~expect:(Some [1]);
    test                          [1;2;3]  ~expect:(Some [1;2;3]);
    test ~start:0                 []       ~expect:(Some []);
    test ~start:0                 [1]      ~expect:(Some [1]);
    test ~start:0                 [1;2;3]  ~expect:(Some [1;2;3]);
    test ~start:0    ~finish:0    []       ~expect:(Some []);
    test ~start:0    ~finish:1    [1]      ~expect:(Some [1]);
    test ~start:0    ~finish:3    [1;2;3]  ~expect:(Some [1;2;3]);
    test ~start:1                 [1;2;3]  ~expect:(Some [2;3]);
    test ~start:2                 [1;2;3]  ~expect:(Some [3]);
    test             ~finish:1    [1;2;3]  ~expect:(Some [1]);
    test             ~finish:2    [1;2;3]  ~expect:(Some [1;2]);
    test ~start:1    ~finish:1    [1;2;3]  ~expect:(Some []);
    test ~start:1    ~finish:2    [1;2;3]  ~expect:(Some [2]);

    test ~start:(-1)              []       ~expect:None;
    test ~start:(-1)              [1;2]    ~expect:None;
    test ~start:1                 []       ~expect:None;
    test ~start:2                 [1;2]    ~expect:None;

    test             ~finish:(-1) []       ~expect:None;
    test             ~finish:(-1) [1;2]    ~expect:None;

    test             ~finish:1    []       ~expect:None;
    test             ~finish:2    [1;2]    ~expect:(Some [1;2]);
    test             ~finish:3    [1;2]    ~expect:None;

  end;
  begin (* test `rev` *)
    let into_str l = List.filter_map l Chr.of_int |> Str.of_character_list in
    let pp l = List.map l (sprintf "%d") |> String.concat ~sep:";" in
    let test l ~expect fmt =
      let name = ksprintf (fun s -> s) fmt in
      let s = into_str l in
      let reversed = Str.rev s in
      let res_ints = Str.to_character_list reversed |> List.map ~f:Chr.to_int in
      let before = Str.to_character_list s |> List.map ~f:Chr.to_int in
      test_assertf (expect = res_ints)
        "test_mapi: [%s=%s] → [%s] <> [%s] (%s)"
        (pp l) (pp before) (pp res_ints) (pp expect)
        name;
    in
    test [] ~expect:[] "empty";
    test [1;2;3] ~expect:[3;2;1] "simple 123->321";
    test [1] ~expect:[1] "single 1";
  end;
  begin (* Test map *)
    let into_str l = List.filter_map l Chr.of_int |> Str.of_character_list in
    let pp l = List.map l (sprintf "%d") |> String.concat ~sep:";" in
    let test_explicit l ~f ~expect fmt =
      let name = ksprintf (fun s -> s) fmt in
      let s = into_str l in
      let test_fn_exn x =
        let res = f (Chr.to_int x) in
        match Chr.of_int res with
        | Some res -> res
        | None -> failwith "bad char"
      in
      let mapped = Str.map s ~f:test_fn_exn in
      let res_ints = Str.to_character_list mapped |> List.map ~f:Chr.to_int in
      let before = Str.to_character_list s |> List.map ~f:Chr.to_int in
      test_assertf (expect = res_ints)
        "test_mapi: [%s=%s] → [%s] <> [%s] (%s)"
        (pp l) (pp before) (pp res_ints) (pp expect)
        name;
    in
    test_explicit [1;2;1] ~f:(fun x -> x + 1) ~expect:[2;3;2] "map";
    test_explicit [1;2;2] ~f:(fun x -> x + 1) ~expect:[2;3;3] "map";
    test_explicit [1;2;3;0] ~f:(fun x -> x) ~expect:[1;2;3;0] "map";
    (* Make sure map_slow works as well. *)
    test_explicit (List.init 10000 (fun x -> x mod 50))
                  ~f:(fun x -> (x + 1) mod 50)
                  ~expect:(List.init 10000 (fun x -> (x + 1) mod 50))
                  "map large";
  end;
  begin (* Test mapi *)
    let char_upper    = 199     (* Not really, but not / 5 *)
    and char_lower    = 33 in   (* Before this come the odd ones in ASCII *)
    let d = char_upper - char_lower in
    let into_str l = List.filter_map l Chr.of_int |> Str.of_character_list in
    let pp l = List.map l (sprintf "%d") |> String.concat ~sep:";" in
    let int_to_char i =
      match Chr.of_int ((i mod d) + char_lower) with
      | None -> failwith "bad logic"
      | Some c -> c
    in
    let rec make_list acc n =
      if n < 0 then acc else make_list ((int_to_char n)::acc) (n - 1)
    in
    let test n =
      let lst = make_list [] n in
      let dum = Str.make (n + 1) (int_to_char 0) in
      let exp = Str.of_character_list lst in
      let res = Str.mapi dum ~f:(fun i _ -> int_to_char i) in
      test_assertf (res = exp) "mapi: %d [%s] [%s]"
        n (Str.to_native_string exp) (Str.to_native_string res)
    in
    let test_explicit l ~f ~expect fmt =
      let name = ksprintf (fun s -> s) fmt in
      let s = into_str l in
      let test_fn_exn i x =
        let res = f i (Chr.to_int x) in
        match Chr.of_int res with
        | Some res -> res
        | None -> failwith "bad char"
      in
      let mapped = Str.mapi s ~f:test_fn_exn in
      let res_ints = Str.to_character_list mapped |> List.map ~f:Chr.to_int in
      let before = Str.to_character_list s |> List.map ~f:Chr.to_int in
      test_assertf (expect = res_ints)
        "test_mapi: [%s=%s] → [%s] <> [%s] (%s)"
        (pp l) (pp before) (pp res_ints) (pp expect)
        name;
    in
    test 10;
    test 10000;    (* to cover that slow case *)
    test_explicit [1;2;1] ~f:(fun _ x -> x + 1) ~expect:[2;3;2] "mapi inc";
    test_explicit [1;2;2] ~f:(fun i x -> i) ~expect:[0;1;2] "mapi indexed";
    (* Make sure mapi_slow works as well. *)
    test_explicit (List.init 10000 (fun x -> x mod 50))
                  ~f:(fun _ x -> (x + 1) mod 50)
                  ~expect:(List.init 10000 (fun x -> (x + 1) mod 50))
                  "mapi large";
    test_explicit (List.init 10000 (fun x -> x mod 50))
                  ~f:(fun i _ -> i mod 50)
                  ~expect:(List.init 10000 (fun x -> x mod 50))
                  "mapi large";
  end;
  begin (* Test `map2_exn` *)
    let into_str l = List.filter_map l Chr.of_int |> Str.of_character_list in
    let pp l = List.map l (sprintf "%d") |> String.concat ~sep:";" in
    let test l1 l2 ~f ~expect fmt =
      let name = ksprintf (fun s -> s) fmt in
      let s1 = into_str l1 in
      let s2 = into_str l2 in
      let test_fn_exn x y =
        let res = f (Chr.to_int x) (Chr.to_int y) in
        match Chr.of_int res with
        | Some res -> res
        | None -> failwith "bad char"
      in
      let mapped = Str.map2_exn s1 s2 ~f:test_fn_exn in
      let res_ints = Str.to_character_list mapped |> List.map ~f:Chr.to_int in
      let before1 = Str.to_character_list s1 |> List.map ~f:Chr.to_int in
      let before2 = Str.to_character_list s2 |> List.map ~f:Chr.to_int in
      test_assertf (expect = res_ints)
        "test_map2_exn: [%s,%s=%s,%s] → [%s] <> [%s] (%s)"
        (pp l1) (pp l2) (pp before1) (pp before2) (pp res_ints) (pp expect)
        name;
    in
    let test_fail l1 l2 ~f fmt =
      let name = ksprintf (fun s -> s) fmt in
      let s1 = into_str l1 in
      let s2 = into_str l2 in
      let failed =
        begin try
            Str.map2_exn s1 s2 ~f:f |> ignore;
            false
          with _ -> true
        end
      in
      test_assertf failed
        "test_map2_exn: should have failed on [%s,%s] (%s)"
        (pp l1) (pp l2)
        name;
    in
    test [] [] ~f:(fun x _ -> x) ~expect:[] "all empty";
    test [1] [2] ~f:(fun x _ -> x) ~expect:[1] "1";
    test [1] [2] ~f:(fun _ y -> y) ~expect:[2] "2";
    test [1;2;3;4] [2;3;4;3]  ~f:(fun _ y -> y) ~expect:[2;3;4;3] "2343";
    test [1;2;1;0] [2;1;1;3]  ~f:(fun x y -> x + y) ~expect:[3;3;2;3] "3323";
    (* Make sure we hit map2_slow: *)
    test (List.init 10000 (fun _ -> 0)) (List.init 10000 (fun _ -> 1))
         ~f:(fun x _ -> x)
         ~expect:(List.init 10000 (fun _ -> 0)) "long str";
    test (List.init 10000 (fun _ -> 0)) (List.init 10000 (fun _ -> 1))
         ~f:(fun _ y -> y)
         ~expect:(List.init 10000 (fun _ -> 1)) "long str";
    (* Make sure we fail on lists of different lengths: *)
    test_fail [1;2] [] ~f:(fun x y -> x) "unequal length";
    test_fail [] [1] ~f:(fun x y -> y) "other side unequal length";
    test_fail [1;2;3] [1] ~f:(fun x y -> y) "nonempty unequal length";
  end;
  begin (* Test `foldi` *)
    let into_str l = List.filter_map l Chr.of_int |> Str.of_character_list in
    let pp l = List.map l (sprintf "%d") |> String.concat ~sep:";" in
    let test lst ~init ~expect fmt =
      let name = ksprintf (fun s -> s) fmt in
      let s = into_str lst in
      let folded = Str.foldi s ~init ~f:(fun i a c -> i + a + Chr.to_int c) in
      test_assertf (folded = expect)
        "test_foldi: init %d + the indices and chars of [%s] → [%d] <> %d %s"
          init (pp lst) folded expect name;
    in
    test [] ~init:0 ~expect:0 "empty";
    test [0] ~init:100 ~expect:100 "singleton, zero-indexed";
    test [0;0;0;0;0] ~init:0 ~expect:10
      "adding the indices of 5 long string should be ten";
    test [100;100;100;100] ~init:0 ~expect:406
      "test uses string values."
  end;
  begin (* Test `fold2_exn` *)
    let into_str l = List.filter_map l Chr.of_int |> Str.of_character_list in
    let pp l = List.map l (sprintf "%d") |> String.concat ~sep:";" in
    let test l1 l2 ~f ~init ~expect fmt =
      let name = ksprintf (fun s -> s) fmt in
      let s1 = into_str l1 in
      let s2 = into_str l2 in
      let test_fn_exn i x y = f i (Chr.to_int x) (Chr.to_int y) in
      let res = Str.fold2_exn s1 s2 ~f:test_fn_exn ~init:init in
      let before1 = Str.to_character_list s1 |> List.map ~f:Chr.to_int in
      let before2 = Str.to_character_list s2 |> List.map ~f:Chr.to_int in
      test_assertf (expect = res)
        "test_fold2_exn: [%s,%s=%s,%s] init:%s → %s <> %s (%s)"
        (pp l1) (pp l2) (pp before1) (pp before2) ((sprintf "%d") init)
        ((sprintf "%d") res) ((sprintf "%d") expect)
        name;
    in
    let test_fail l1 l2 ~f ~init fmt =
      let name = ksprintf (fun s -> s) fmt in
      let s1 = into_str l1 in
      let s2 = into_str l2 in
      let failed =
        begin try
            Str.fold2_exn s1 s2 ~f:f ~init:init |> ignore;
            false
          with _ -> true
        end
      in
      test_assertf failed
        "test_fold2_exn: should have failed on [%s,%s] (%s)"
        (pp l1) (pp l2)
        name;
    in
    test [] [] ~f:(fun _ _ _ -> 1) ~init:0 ~expect:0 "nothing";
    test [1] [2] ~f:(fun i _ _ -> i) ~init:1 ~expect:1 "1";
    test [1] [2] ~f:(fun _ _ y -> y) ~init:1 ~expect:2 "2";
    test [0;1] [0;1]  ~f:(fun i x y -> x + y + i)
         ~init:1 ~expect:3 "3";
    (* Make sure we fail on lists of different lengths: *)
    test_fail [1;2] [] ~f:(fun _ x y -> 1) ~init:0 "unequal length";
    test_fail [] [1] ~f:(fun _ x y -> 1) ~init:0 "other side unequal length";
    test_fail [1;2;3] [1] ~f:(fun _ x y -> 1) ~init:0 "nonempty unequal length";
  end;
  begin (* Test is_prefix *)
    let test l ~p ~expect =
      let t      = ints_to_str l
      and prefix = ints_to_str p in
      test_assertf (expect = Str.is_prefix t ~prefix)
        "is_prefix: %s prefix:(%s) expects %B"
          (int_list_to_string l)
          (int_list_to_string p)
          expect
    in
    test []       ~p:[]         ~expect:true;
    test [1;2;3]  ~p:[]         ~expect:true;
    test [1;2;3]  ~p:[1]        ~expect:true;
    test [1;2;3]  ~p:[1;2;3]    ~expect:true;
    test [1;2;3]  ~p:[4]        ~expect:false;
    test []       ~p:[1]        ~expect:false;
    test [1;2;3]  ~p:[1;2;3;4]  ~expect:false;
  end;
  begin (* Test is_suffix *)
    let test l ~s ~expect =
      let t      = ints_to_str l
      and suffix = ints_to_str s in
      test_assertf (expect = Str.is_suffix t ~suffix)
        "is_suffix: %s suffix:(%s) expects %B"
          (int_list_to_string l)
          (int_list_to_string s)
          expect
    in
    test []       ~s:[]         ~expect:true;
    test [1;2;3]  ~s:[]         ~expect:true;
    test [1;2;3]  ~s:[3]        ~expect:true;
    test [1;2;3]  ~s:[1;2;3]    ~expect:true;
    test [1;2;3]  ~s:[4]        ~expect:false;
    test []       ~s:[1]        ~expect:false;
    test [1;2;3]  ~s:[1;2;3;4]  ~expect:false;
  end;
  begin (* Test chop_prefix *)
    let test l ~p ~expect =
      let t      = ints_to_str l
      and prefix = ints_to_str p in
      let res    = Str.chop_prefix t ~prefix
      and exp    = optionMap (fun x -> Some (ints_to_str x)) expect in
      test_assertf (exp = res)
        "chop_prefix: %s prefix:(%s) expected %s but got %s"
          (int_list_to_string l)
          (int_list_to_string p)
          (defOptMap "None" int_list_to_string expect)
          (defOptMap "None" Str.to_native_string res)
    in
    test []       ~p:[]         ~expect:(Some []);
    test [1;2;3]  ~p:[]         ~expect:(Some [1;2;3]);
    test [1;2;3]  ~p:[1]        ~expect:(Some [2;3]);
    test [1;2;3]  ~p:[1;2;3]    ~expect:(Some []);
    test [1;2;3]  ~p:[4]        ~expect:None;
    test []       ~p:[1]        ~expect:None;
    test [1;2;3]  ~p:[1;2;3;4]  ~expect:None;
  end;
  begin (* Test chop_suffix *)
    let test l ~s ~expect =
      let t      = ints_to_str l
      and suffix = ints_to_str s in
      let res    = Str.chop_suffix t ~suffix
      and exp    = optionMap (fun x -> Some (ints_to_str x)) expect in
      test_assertf (exp = res)
        "chop_suffix: %s suffix:(%s) expected %s but got %s"
          (int_list_to_string l)
          (int_list_to_string s)
          (defOptMap "None" int_list_to_string expect)
          (defOptMap "None" Str.to_native_string res)
    in
    test []       ~s:[]         ~expect:(Some []);
    test [1;2;3]  ~s:[]         ~expect:(Some [1;2;3]);
    test [1;2;3]  ~s:[3]        ~expect:(Some [1;2]);
    test [1;2;3]  ~s:[1;2;3]    ~expect:(Some []);
    test [1;2;3]  ~s:[4]        ~expect:None;
    test []       ~s:[1]        ~expect:None;
    test [1;2;3]  ~s:[1;2;3;4]  ~expect:None;
  end;

  begin (* Test split_at, take and drop. *)
    let test l n (le,ri) =
      let s   = ints_to_str l
      and le' = ints_to_str le
      and ri' = ints_to_str ri in
      let (rl,rr) as res = Str.split_at s n
      and tres  = Str.take s n
      and dres  = Str.drop s n in
      test_assertf (res = (le',ri') && tres = le' && dres = ri')
        "split_at: %s %d expects (%s,%s) but got (%s,%s)"
          (int_list_to_string l)
          n
          (int_list_to_string le)
          (int_list_to_string ri)
          (Str.to_native_string rl)
          (Str.to_native_string rr)
    in
    test []      (-1) ([],[]);
    test []      (0)  ([],[]);
    test []      (1)  ([],[]);
    test [1;2;3] (-1) ([],[1;2;3]);
    test [1;2;3] (0)  ([],[1;2;3]);
    test [1;2;3] (1)  ([1],[2;3]);
    test [1;2;3] (2)  ([1;2],[3]);
    test [1;2;3] (3)  ([1;2;3],[]);
    test [1;2;3] (4)  ([1;2;3],[]);
  end;

  (* #### BENCHMARKS #### *)

  let converted_dna_reads =
    let all =
      List.filter_map dna_test_subjects (fun s ->
          match Str.of_native_string s with
          | `Ok cs -> Some cs
          | `Error _ -> None) in
    test_assertf (List.length dna_test_subjects = List.length all)
      "Convert DNA (common denominator) %d <> %d"
      (List.length dna_test_subjects) (List.length all);
    all in

  let implementation = test_name in
  Benchmark.declare
    ~experiment:(sprintf "Concat%d" (List.length converted_dna_reads))
    ~implementation
    (fun () ->
       let _ =
         Str.concat ~sep:Str.empty converted_dna_reads in
       ());

  let cated =
    Str.concat ~sep:Str.empty converted_dna_reads in
  Benchmark.declare
    ~experiment:(sprintf "Length")
    ~implementation
    ~repeats:10000
    (fun () -> ignore (Str.length cated));
  let sub =
    List.nth_exn converted_dna_reads (List.length converted_dna_reads - 30) in
  let sub_index = 1 in
  let sub_length = Str.length sub - sub_index in
  let from = 2 in
  Benchmark.declare
    ~experiment:(sprintf "Find")
    ~implementation
    ~repeats:5
    (fun () -> ignore (
         Str.index_of_string cated ~sub ~from ~sub_index ~sub_length
       ));
  Benchmark.declare
    ~experiment:(sprintf "R-Find")
    ~implementation
    ~repeats:5
    (fun () -> ignore (
         Str.index_of_string_reverse cated ~sub ~from ~sub_index ~sub_length
       ));

  Benchmark.declare
    ~experiment:(sprintf "Split On Char")
    ~implementation
    ~repeats:40
    (fun () ->
      match Chr.of_int (int_of_char 'A') with
      | None -> say "Skipping split test since can't convert 'A'"
      | Some a -> ignore (List.iter converted_dna_reads
                    (fun s -> ignore (Str.split ~on:(`Character a) s))));

  ()

(*M

A UTF-8-Specific Test
---------------------

M*)
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

(*M

Test Instantiations
-------------------

M*)
let () =
  do_basic_test (module struct
      let test_name = "Both natives"
      let can_have_wrong_char = false
      module Chr = Native_character
      module Str = Native_bytes
    end);
  do_basic_test (module struct
      let test_name = "List of natives"
      let can_have_wrong_char = false
      module Chr = Native_character
      module Str = List_of.Make (Native_character)
  end);
  do_basic_test (module struct
      let test_name = "List of UTF-8 Integers"
      let can_have_wrong_char = true
      module Chr = Int_utf8_character
      module Str = List_of.Make (Int_utf8_character)
  end);
  do_basic_test (module struct
      let test_name = "Of_mutable(utf8-int array)"
      let can_have_wrong_char = true
      module Chr = Int_utf8_character
      module Str = Of_mutable.Make (struct
          type character = Chr.t
          type t = int array
          let empty = [| |]
          let max_string_length = Some Sys.max_array_length
          let length = Array.length
          let make = Array.make
          let get t i = t.(i)
          let set t i c = t.(i) <- c
          let blit = Array.blit
          let is_whitespace = Chr.is_whitespace
          let compare a b =
            let open Array in
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

          let compare_char = Int.compare

          let of_native_substring natstr ~offset ~length =
            Conversions.of_native_substring
              ~empty ~init:(fun () -> ref [])
              ~on_new_character:(fun x c -> x := c :: !x)
              ~finalize:(fun x -> List.rev !x |> Array.of_list)
              ~read_character_from_native_string:Chr.read_from_native_string
              natstr ~offset ~length

          let of_native_string natstr =
            Conversions.of_native_string
              of_native_substring natstr

          let to_native_string l =
            Conversions.to_native_string_knowing_size
              ~future_size:(fun l ->
                  let s = ref 0 in
                  Array.iter l ~f:(fun c -> s := !s + Chr.size c);
                  !s)
              ~iter:(fun a ~f -> Array.iter a ~f)
              ~write_char_to_native_bytes:Chr.write_to_native_bytes
              l
            |> Bytes.to_string

        end)
  end);
  do_basic_test (module struct
      let test_name = "Of_mutable(int8 Bigarray1.t)"
      let can_have_wrong_char = false
      open Bigarray
      type char_bigarray = (char, int8_unsigned_elt, c_layout) Array1.t
      module Chr = Native_character
      module Str = Of_mutable.Make (struct
          type character = Chr.t
          type t = char_bigarray
          let empty = Array1.create char c_layout 0
          let max_string_length = None
          let length = Array1.dim
          let make len c  =
            let res = Array1.create char c_layout len in
            Array1.fill res c;
            res
          let is_whitespace = Chr.is_whitespace
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
            Conversions.of_native_substring
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
            Conversions.of_native_string of_native_substring natstr

          let to_native_string l =
            String.init (Array1.dim l) (Array1.get l)

        end)
  end);
  utf8_specific_test ();
  say "\n## Benchmarks\n\n";
  say "- `uname -a`: ";
  ignore (Unix.system "uname -a");
  say "- Word size: %d\n\n%s\n" Sys.word_size (Benchmark.to_string ());
  exit !return_code
