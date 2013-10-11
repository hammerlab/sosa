
Sane OCaml String API
=====================


This library is a set of APIs defined with module types, and a set of
modules implementing one or more of those interfaces.

The APIs define what a *character* and a *string of characters* should
be.

APIs / Module Types
-------------------

We have:

- `BASIC_CHARACTER`: characters of any length.
- `NATIVE_CONVERSIONS`: functions to transform from/to native OCaml
  strings.
- `BASIC_STRING`: immutable strings of (potentially abstract)
  characters:
    - includes `NATIVE_CONVERSIONS`,
    - contains a functor to provide a thread agnostic `output` function:
    `Make_output`: `OUTPUT_MODEL` → `sig val output: ... end`.
- `UNSAFELY_MUTABLE`: mutability of some string implementations
  (“unsafe” meaning that they break immutability
  invariants/assumptions).
- `MINIMALISTIC_MUTABLE_STRING`: abstract mutable string used as
  argument of the `Of_mutable` functor.

The Implementations
-------------------

### Native OCaml Chars

The `Native_character` module implements `BASIC_CHARACTER` with
OCaml's `char` type.

### Native OCaml Strings

The `Native_string` module implements `BASIC_STRING` and
`UNSAFELY_MUTABLE` with OCaml's `string` type (and hence
`Native_character`).

### Lists Of Arbitrary Characters

`List_of` is a functor: `BASIC_CHARACTER` → `BASIC_STRING`

### Build From Basic Mutable Data-structures

The functor `Of_mutable` uses an implementation of
`MINIMALISTIC_MUTABLE_STRING` to build a `BASIC_STRING`.
See the file `test/sosa_test.ml` for examples of usage (with `array`s of `UTF-8
integers`, and `Bigarray.Array1.t` of bytes).

### Integer UTF-8 Characters

The `Int_utf8_character` module implements `BASIC_CHARACTER` with
OCaml integers (`int`) representing Utf8 characters (we force the
handling of not more than 31 bits, even if [RFC 3629][RFC3629]
restricts them to end at U+10FFFF, c.f. also
[wikipedia][wikipedia:UTF-8]). Note that the function `is_whitespace` considers
only ASCII whitespace (useful while writing parsers for example).

Tests and Benchmarks
--------------------

You may run all the regression tests with:

    ./test/sosa_test.ml

and you may add the basic benchmarks to the process with:

    ./test/sosa_test.ml bench

[wikipedia:UTF-8]: http://en.wikipedia.org/wiki/UTF-8
[RFC3629]: http://tools.ietf.org/html/rfc3629
