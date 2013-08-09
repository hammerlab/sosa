
Sane OCaml String API
=====================


This library is a set APIs defined with module types, and a set of
modules implementing one or more of those interfaces.

The APIs define what a *character* and a *string of characters* should
be.

The implementations are so far:

### Native OCaml Chars

### Native OCaml Strings

### Lists

`List_of` is a functor: `BASIC_CHAR` → `BASIC_STRING`

### Integer UTF-8 Characters

Utf8 Chars represented by native integers (we force the handling
of not more than 31 bits, even if
[RFC 3629][RFC3629] restricts them to end at U+10FFFF).

[wikipedia:UTF-8]: http://en.wikipedia.org/wiki/UTF-8
[RFC3629]: http://tools.ietf.org/html/rfc3629
