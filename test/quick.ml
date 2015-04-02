module Q = QCheck
module QA = QCheck.Arbitrary

open Sosa

module type TEST_STRING = sig
  val test_name: string
  val can_have_wrong_char: bool
  module Chr: BASIC_CHARACTER
  module Str: BASIC_STRING with type character = Chr.t
  val arb_chr : Chr.t QA.t
  val arb_str : Str.t QA.t
end

let gen_suite (module Test : TEST_STRING) =
  let open Test in
  let arb_string_and_indices =
    QA.(>>=) arb_str (fun s ->
      let stop = Str.length s in
      let upbd = QA.int_range ~start:0 ~stop in
      let smpl = QA.list ~len:(QA.return 100) upbd in
      QA.pair (QA.return s) smpl)
  in
  [ Q.mk_test ~name:"Empty strings have zero length"
      ~n:1 (QA.return ()) (fun () -> Str.length Str.empty = 0)

  ; Q.mk_test ~name:"Empty strings is empty"
      ~n:1000 (QA.return ()) (fun () -> Str.is_empty Str.empty)

  ; Q.mk_test ~name:"Round trip from native."
      QA.string (fun native ->
          match Str.of_native_string native with
          | `Ok s -> Str.to_native_string s = native
          | `Error (`wrong_char_at i) ->
              Chr.read_from_native_string ~buf:native ~index:i = None)

  ; Q.mk_test ~name:"Have lengths" arb_str (fun s -> Str.length s >= 0)

  ; Q.mk_test ~name:"We can index across all indices less than lengths"
  arb_string_and_indices
   (fun (str, indices) ->
      List.fold_left (fun t i ->
        match Str.get str i with
        | Some _ -> t && true
        | None -> false) true indices)


      (*
    Q.mk_test ~name:"Folding."
      arb_str (fun str ->
        Str.fold ~init:[] ~f:(fun p c -> c :: p) =
          *)

  ]

let suite =
  Q.flatten
  [ gen_suite (module struct
      let test_name = "Both natives"
      let can_have_wrong_char = false
      module Chr = Native_character
      module Str = Native_string
      let arb_chr = QA.char
      let arb_str = QA.string
    end);
    gen_suite (module struct
      let test_name = "List of natives"
      let can_have_wrong_char = false
      module Chr = Native_character
      module Str = List_of (Native_character)
      let arb_chr = QA.char
      let arb_str = QA.list arb_chr
    end);
    gen_suite (module struct
      let test_name = "List of UTF-8 Integers"
      let can_have_wrong_char = true
      module Chr = Int_utf8_character
      module Str = List_of (Int_utf8_character)
      let arb_chr = QA.int_range ~start:0 ~stop:1073741823
        (* not max possible 0x7fff_ffff, but capped at 2^30 - 1 per api *)
      let arb_str = QA.list arb_chr

    end);
  ]


let () =
  if Q.run_tests suite
  then print_string "passed"
  else print_string "failed"
