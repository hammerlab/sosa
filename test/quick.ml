module Q = QCheck
module QA = QCheck.Arbitrary

open Sosa

module Option = struct
  let is_none = function | None -> true | Some _ -> false
  let is_some o = not (is_none o)
end

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
  let no_excp f = try f (); true with _ -> false in
  let is_excp f = try f (); false with _ -> true in
  let arb_str_and f =
    QA.(>>=) arb_str (fun s ->
      QA.pair (QA.return s) (f s))
  in
  let arb_string_and_indices =
    arb_str_and (fun s ->
      QA.list ~len:(QA.return 20)
        (QA.int_range ~start:0 ~stop:(Str.length s)))
  in
  [ Q.mk_test ~name:"Empty strings have zero length"
      ~n:1 (QA.return ()) (fun () -> Str.length Str.empty = 0)

  ; Q.mk_test ~name:"Empty strings is empty"
      ~n:1000 (QA.return ()) (fun () -> Str.is_empty Str.empty)

  ; Q.mk_test ~name:"We can make strings." ~n:1000
    (QA.pair (QA.int 100) arb_chr)
      (fun (n, c) -> no_excp (fun () ->  Str.make n c))

  ; Q.mk_test ~name:"Have lengths" arb_str (fun s -> Str.length s >= 0)

  ; Q.mk_test ~name:"Make from character" ~n:10000
    arb_chr (fun c -> no_excp (fun () -> Str.of_character c))

  ; Q.mk_test ~name:"From character list round trip" ~n:1000
    (QA.list ~len:(QA.int 1000) arb_chr)
      (fun clst ->
        clst = Str.to_character_list (Str.of_character_list clst))

  ; Q.mk_test ~name:"To character list rount trip" ~n:1000
    arb_str (fun str ->
      str = Str.of_character_list (Str.to_character_list str))

  (** Getters *)
  ; Q.mk_test ~name:"Can't index negative"
    (arb_str_and (fun _ -> QA.map (QA.int 1000) (fun i -> -1 * i - 1)))
    (fun (str, index) ->
      Str.get str index |> Option.is_none &&
      is_excp (fun () -> Str.get_exn str index))

  ; Q.mk_test ~name:"Can't index past length"
    (arb_str_and (fun s -> QA.map (QA.int 100) ((+) (Str.length s))))
      (fun (str, index) ->
        Str.get str index |> Option.is_none &&
        is_excp (fun () -> Str.get_exn str index))

  ; Q.mk_test ~name:"We can index across all indices less than lengths"
    ~pp:Q.PP.(pair Str.to_native_string (list int))
    arb_string_and_indices
    (Q.Prop.(==>)
     (fun (str, _) -> not (Str.is_empty str))
     (fun (str, indices) ->
            List.fold_left (fun t index ->
              Option.is_some (Str.get str index) &&
              no_excp (fun () -> Str.get_exn str index) && t) true indices))

  (** Setters. *)
  ; Q.mk_test ~name:"We can set across all inbound indices"
    (QA.pair arb_string_and_indices arb_chr)
    (Q.Prop.(==>)
     (fun ((str, _), _) -> not (Str.is_empty str))
     (fun ((str, indices), v) ->
            List.fold_left (fun t index ->
              Option.is_some (Str.set str index v) &&
              no_excp (fun () -> Str.set_exn str index v) && t) true indices))

  ; Q.mk_test ~name:"Concat sums up the lengths"
    (QA.list arb_str)
      (fun str_lst ->
        List.fold_left (fun sum str -> sum + Str.length str) 0 str_lst =
          (Str.length (Str.concat str_lst)))

  (** Can we have a test to check that we're concatting in order. *)
  ; Q.mk_test ~name:"Concat sums up the lengths custom sep"
    (QA.pair arb_str (QA.list arb_str))
      (fun (sep, str_lst) ->
        if List.length str_lst = 0
        then Str.concat ~sep str_lst = Str.empty
        else List.fold_left (fun sum str -> sum + Str.length str) 0 str_lst
              + (Str.length sep) * (List.length str_lst - 1)
              = (Str.length (Str.concat ~sep str_lst)))

  ; Q.mk_test ~name:"Round trip from native."
      QA.string (fun native ->
          match Str.of_native_string native with
          | `Ok s -> Str.to_native_string s = native
          | `Error (`wrong_char_at i) ->
              Chr.read_from_native_string ~buf:native ~index:i = None)

  ; Q.mk_test ~name:"Round trip from native substring." ~n:1000
    ~pp:Q.PP.(triple string int int)
    (QA.triple QA.string (QA.int 100) (QA.int 100))
      (fun (native, offset, length) ->
        match Str.of_native_substring native offset length with
        | `Ok s -> Str.to_native_string s = String.sub native offset length
        | `Error (`wrong_char_at i) ->
            Chr.read_from_native_string ~buf:native ~index:i = None
        | `Error `out_of_bounds ->
            offset < 0 || offset + length > String.length native)


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


let () = if Q.run_tests suite then exit 0 else exit 1
