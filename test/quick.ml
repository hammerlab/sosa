
module type TEST_STRING = sig
  val test_name: string
  val can_have_wrong_char: bool
  module Chr: BASIC_CHARACTER
  module Str: BASIC_STRING with type character = Chr.t
end


let suite (module Test : TEST_STRING) =
  let arb_str = QCheck.string in
  [
    QCheck.mk_test ~name:"Round trip from native."
      QCheck.string (fun native ->
          match Str.of_native_substring native with
          | `Ok s -> Str.to_native_string s = native
          | `Error (`wrong_char_at i) ->
              Chr.read_from_native_string ~buf:native ~index:i = None)

     (*QCheck.mk_test ~name:"Empty strings have no length" arb_sosa_str
        (fun s -> Str.length s = 0)
        *)
  ]

let () =
  if QCheck.run_tests suite
  then print_strings "passed"
  else print_stirngs "failed"
