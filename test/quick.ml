module Q = QCheck
module QA = QCheck.Arbitrary
module QP = QCheck.PP

open Sosa

let invalidArg fmt = Printf.ksprintf (fun s -> raise (Invalid_argument s)) fmt

module Option = struct
  let is_none   = function | None -> true | Some _ -> false
  let is_some o = not (is_none o)
  let get       = function | Some x -> x | None _ -> invalidArg "Option.get"
end

module type TEST_STRING = sig
  val test_name: string
  val can_have_wrong_char: bool
  module Chr: BASIC_CHARACTER
  module Str: BASIC_STRING with type character = Chr.t
  val arb_chr : Chr.t QA.t
  val arb_str : Str.t QA.t
end

(* Capped at 2^30 - 1 per Random API *)
let max_random_int = truncate (2.0 ** 30.0) - 1
let max_str_index  = 100

let gen_suite (module Test : TEST_STRING) =
  let open Test in
  let no_excp f = try f (); true with _ -> false in
  let is_excp f = try f (); false with _ -> true in
  let arb_str_and f = QA.(arb_str >>= fun s -> pair (return s) (f s)) in
  let arb_string_and_indices =
    arb_str_and (fun s ->
      QA.(list ~len:(return 20)
        (int_range ~start:0 ~stop:(Str.length s))))
  in
  let multiplier = 1 in
  let mk_test ~name ?n =
    let n = match n with | None -> 100 | Some n -> multiplier * n in
    Q.mk_test ~name:(test_name ^ " : " ^ name) ~n in
  [
  (* Testing BASIC_CHARACTER interface *)
    mk_test ~name:"Native character conversion."
      ~pp:QP.char ~n:10000 QA.char
        (fun c -> no_excp (fun () -> Chr.of_native_char c))
  ; mk_test ~name:"Integer to character conversion."
      ~pp:QP.int ~n:10000 (QA.int max_random_int)
        (fun c -> no_excp (fun () -> Chr.of_int c))

  ; mk_test ~name:"Character can get convert to integers."
     ~n:1000 arb_chr (fun c -> no_excp (fun () -> Chr.to_int c))

  ; mk_test ~name:"Character can get size of integers."
     ~n:1000 arb_chr (fun c -> no_excp (fun () -> Chr.size c))

  ; mk_test ~name:"Character can write to native string."
      ~pp:QP.(triple Chr.to_native_string string int)
      QA.(triple arb_chr string (int max_str_index))
        (fun (c, buf, index) ->
          match Chr.write_to_native_string c ~buf ~index with
          | `Ok _ -> true
          | `Error `out_of_bounds -> index < 0 || index >= String.length buf)

  ; mk_test ~name:"Character to native string"
    ~n:1000 arb_chr (fun c -> no_excp (fun () -> Chr.to_native_string c))

  ; mk_test ~name:"Character can read from native"
    QA.(pair string (int max_str_index))
      (fun (buf, index) ->
        match Chr.read_from_native_string ~buf ~index with
        | None        -> index < 0 || index >= String.length buf
        | Some (c, _) -> c = (Chr.of_native_char buf.[index] |> Option.get))

  ; mk_test ~name:"Character can be human readable."
    ~n:1000 arb_chr (fun c -> no_excp (fun () -> Chr.to_string_hum c))

  ; mk_test ~name:"Character compare is opposite with opposite arg"
    ~n:1000 QA.(pair arb_chr arb_chr)
      (fun (c1, c2) -> Chr.compare c1 c2 = -1 * Chr.compare c2 c1)

  ; mk_test ~name:"Character is_whitespace"
    ~n:1 QA.(return " \t\r\n")
      (fun s ->
        let t c = Chr.of_native_char c |> Option.get |> Chr.is_whitespace in
        t s.[0] && t s.[1] && t s.[2] && t s.[3])

  ; mk_test ~name:"Character not is_whitespace"
    ~n:1 QA.(return "This_string_has_no_whitespace")
      (fun s ->
        let t c = Chr.of_native_char c |> Option.get |> Chr.is_whitespace |> not in
        Array.init (String.length s) (fun i -> t s.[i])
        |> Array.to_list
        |> List.for_all (fun x -> x))


  (* Testing BASIC_STRING interface *)
  ; mk_test ~name:"Empty strings have zero length"
      ~n:1 (QA.return ()) (fun () -> Str.length Str.empty = 0)

  ; mk_test ~name:"Empty strings is empty"
      ~n:1000 (QA.return ()) (fun () -> Str.is_empty Str.empty)

  ; mk_test ~name:"We can make strings." ~n:1000
      QA.(pair (int max_str_index) arb_chr)
        (fun (n, c) -> no_excp (fun () ->  Str.make n c))

  ; mk_test ~name:"Have lengths" arb_str (fun s -> Str.length s >= 0)

  ; mk_test ~name:"Make from character" ~n:10000
      arb_chr (fun c -> no_excp (fun () -> Str.of_character c))

  ; mk_test ~name:"From character list round trip" ~n:1000
      QA.(list ~len:(int 1000) arb_chr)
        (fun clst ->
          clst = Str.to_character_list (Str.of_character_list clst))

  ; mk_test ~name:"To character list rount trip" ~n:1000
      arb_str (fun str ->
        str = Str.of_character_list (Str.to_character_list str))

  (** Getters *)
  ; mk_test ~name:"Can't index negative"
      (arb_str_and (fun _ -> QA.(map (int 1000) (fun i -> -1 * i - 1))))
      (fun (str, index) ->
        Str.get str index |> Option.is_none &&
        is_excp (fun () -> Str.get_exn str index))

  ; mk_test ~name:"Can't index past length"
    (arb_str_and (fun s -> let l = Str.length s in
        QA.(map (int max_str_index) ((+) l))))
      (fun (str, index) ->
        Str.get str index |> Option.is_none &&
        is_excp (fun () -> Str.get_exn str index))

  ; mk_test ~name:"We can index across all indices less than lengths"
    ~pp:QP.(pair Str.to_native_string (list int))
    arb_string_and_indices
    (Q.Prop.((fun (str, _) -> not (Str.is_empty str)) ==>
     (fun (str, indices) ->
        indices
        |> List.fold_left (fun t index ->
            Option.is_some (Str.get str index) &&
            no_excp (fun () -> Str.get_exn str index) && t) true )))

  (** Setters. *)
  ; mk_test ~name:"We can set across all inbound indices"
    (QA.pair arb_string_and_indices arb_chr)
    (Q.Prop.((fun ((str, _), _) -> not (Str.is_empty str)) ==>
     (fun ((str, indices), v) ->
        indices
        |> List.fold_left (fun t index ->
            Option.is_some (Str.set str index v) &&
            no_excp (fun () -> Str.set_exn str index v) && t) true)))

  ; mk_test ~name:"Concat sums up the lengths"
    (QA.list arb_str)
      (fun str_lst ->
        List.fold_left (fun sum str -> sum + Str.length str) 0 str_lst =
          (Str.length (Str.concat str_lst)))

  (** Can we have a test to check that we're concatting in order. *)
  ; mk_test ~name:"Concat sums up the lengths custom sep"
    QA.(pair arb_str (list arb_str))
      (fun (sep, str_lst) ->
        if List.length str_lst = 0
        then Str.concat ~sep str_lst = Str.empty
        else List.fold_left (fun sum str -> sum + Str.length str) 0 str_lst
              + (Str.length sep) * (List.length str_lst - 1)
              = (Str.length (Str.concat ~sep str_lst)))

  ; mk_test ~name:"Round trip from native."
      QA.string (fun native ->
          match Str.of_native_string native with
          | `Ok s -> Str.to_native_string s = native
          | `Error (`wrong_char_at i) ->
              Chr.read_from_native_string ~buf:native ~index:i = None)

  ; mk_test ~name:"Round trip from native substring." ~n:1000
    ~pp:QP.(triple string int int)
    QA.(triple string (int max_str_index) (int max_str_index))
      (fun (native, offset, length) ->
        match Str.of_native_substring native offset length with
        | `Ok s -> Str.to_native_string s = String.sub native offset length
        | `Error (`wrong_char_at i) ->
            Chr.read_from_native_string ~buf:native ~index:i = None
        | `Error `out_of_bounds ->
            offset < 0 || offset + length > String.length native)

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
      (* not max possible 0x7fff_ffff (aka 2^31) *)
      let arb_chr = QA.int max_random_int
      let arb_str = QA.list arb_chr
    end);
  ]


let () = if Q.run_tests suite then exit 0 else exit 1
