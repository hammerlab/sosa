
Sane OCaml String API
=====================


This library is a set of APIs defined with module types, and a set of
modules and functors implementing one or more of those interfaces.

The APIs define what a *character* and a *string of characters* should
be.

This is the development branch of the library, the latest released version is 
[0.0.1](http://seb.mondet.org/software/sosa/doc.0.0.1/).

See the [INSTALL](INSTALL.md) file for build instructions.

The library is “packed” in the `Sosa` toplevel module name.

Module Types (APIs)
-------------------

We have, in the sub-module `Api`:

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

Implementations
---------------

### Native OCaml Characters

The `Native_character` module implements `BASIC_CHARACTER` with
OCaml's `char` type.

### Native OCaml Strings

The `Native_string` module implements `BASIC_STRING` with OCaml's `string` type
considered immutable (and hence `Native_character`).

### Native Mutable OCaml Strings (Bytes)

The `Native_bytes` module implements `BASIC_STRING`
and `UNSAFELY_MUTABLE` with OCaml's `bytes` type.

### Lists Of Arbitrary Characters

`List_of` is a functor: `BASIC_CHARACTER` → `BASIC_STRING`, i.e., it creates a
string datastructure made of a list of characters.

### Build From Basic Mutable Data-structures

The functor `Of_mutable` uses an implementation of
`MINIMALISTIC_MUTABLE_STRING` to build a `BASIC_STRING`.

### Integer UTF-8 Characters

The `Int_utf8_character` module implements `BASIC_CHARACTER` with
OCaml integers (`int`) representing Utf8 characters (we force the
handling of not more than 31 bits, even if [RFC 3629][RFC3629]
restricts them to end at U+10FFFF, c.f. also
[wikipedia][wikipedia:UTF-8]). Note that the function `is_whitespace` considers
only ASCII whitespace (useful while writing parsers for example).

Examples, Tests, and Benchmarks
-------------------------------

See the file [`test/main.ml`](src/test/main.ml) for usage examples, the
library is tested with:

- native strings and characters,
- lists of native characters (`List_of(Native_character)`),
- lists of integers representing UTF-8 characters (`List_of(utf8-int array)`),
- arrays of integers representing UTF-8 characters (`Of_mutable(utf8-int array)`),
- bigarrays of 8-bit integers (`Of_mutable(int8 Bigarray1.t)`).

The tests depend on the [Nonstd](https://bitbucket.org/smondet/nonstd),
`unix`, and `bigarray` libraries:

    make test
    ./sosa_tests

and you may add the basic benchmarks to the process with:

    ./sosa_tests bench

[wikipedia:UTF-8]: http://en.wikipedia.org/wiki/UTF-8
[RFC3629]: http://tools.ietf.org/html/rfc3629

