
Sane OCaml String API
=====================


This library is a set APIs defined with module types, and a set of
modules implementing one or more of those interfaces.

The APIs define what a *character* and a *string of characters* should
be.

The implementations are so far:

### Native OCaml Chars

The `Native_char` module implements `BASIC_CHAR` with OCaml's `char` type.

### Native OCaml Strings

The `Native_string` module implements `BASIC_STRING` with OCaml's
`string` type (and hence `Native_char`).

### Lists

`List_of` is a functor: `BASIC_CHAR` → `BASIC_STRING`

### Integer UTF-8 Characters

The `Int_utf8_character` module implements `BASIC_CHAR` with OCaml
integers (`int`) representing Utf8 characters (we force the handling
of not more than 31 bits, even if [RFC 3629][RFC3629] restricts them
to end at U+10FFFF, c.f. also [wikipedia][wikipedia:UTF-8]).

[wikipedia:UTF-8]: http://en.wikipedia.org/wiki/UTF-8
[RFC3629]: http://tools.ietf.org/html/rfc3629
