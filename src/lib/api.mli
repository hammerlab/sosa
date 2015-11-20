(** The module types that define Sosa's API. *)

(* Document generation nuisances:
   - Unfortunately, some of the internal references ie,  things like {!val:foo}
   turn out to look horrible because ocamldoc generates the full path
   'Api.BASIC_CHARACTER.Foo' which distracts from the readability; so I've tried
   to avoid using them, and replaced them with specific links: {{!val:foo}foo}.
*)

type ('a, 'b) result = [
  | `Ok of 'a
  | `Error of 'b
]
(** The type [result] is a reusable version of the classical [Result.t]
    type. *)

(** A monadic thread model (like [Lwt], [Async]) and an [output]
    function. *)
module type OUTPUT_MODEL = sig

  type ('a, 'b, 'c) thread
  (** The type of the threads, the type parameters are there in case
      the user needs up to 3 of them.

      For instance, if implement with [Lwt], we will have [type ('a, 'b, 'c)
      thread = 'a Lwt.t], but with [Pvem.DEFERRED_RESULT]: [type ('a, 'b, 'c)
      thread = ('a, 'b) Deferred_result.t]. *)

  type ('a, 'b, 'c) channel
  (** The channel type, channels can also have up to 3 type-parameters. *)

  val return: 'a -> ('a, 'b, 'c) thread
  (** The monadic [return]. *)

  val bind: ('a, 'b, 'c) thread -> ('a -> ('d, 'b, 'c) thread) -> ('d, 'b, 'c) thread
  (** The monadic [bind]. *)

  val output: ('a, 'b, 'c) channel -> String.t -> (unit, 'e, 'f) thread
  (** The function to output a given native string to a channel.  *)

end (* OUTPUT_MODEL *)

(** The minimal API implemented by characters. *)
module type BASIC_CHARACTER = sig

  type t
  (** The type representing the character. *)

  val of_native_char: char -> t option
  (** Import a native [char], returns [None] if the character is not
      representable. *)

  val of_int: int -> t option
  (** Import an integer, returns [None] if there is no character for
      that value. *)

  val to_int: t -> int
  (** Returns the integer representation of the character. *)

  val size: t -> int
  (** Get the size of the character, the exact semantics are
      implementation-specific (c.f. {!write_to_native_bytes}) *)

  val write_to_native_bytes: t -> buf:Bytes.t -> index:int -> (int, [> `out_of_bounds]) result
  (** [write_to_native_bytes c ~buf ~index] serializes
      the character [c] at position [index] in the native bytes
      [buf] (writing [size c] units). Note, as with {!size} that the
      meaning of [index] is implementation dependent (can be the {i
      index-th} byte, the {i index-th} bit, etc.). *)

  val to_native_string: t -> String.t
  (** [to_native_string c] creates a string containing the
      serialization of the character [c] (if [size c] is not a
      multiple of 8, the end-padding is undefined). *)

  val read_from_native_string: buf:String.t -> index:int -> (t * int) option
  (** Read a character at a given [index] in a native string, returns
      [Some (c, s)], the character [c] and the number of units read [s],
      or [None] if there is no representable/valid character at that
      index. *)

  val to_string_hum: t -> String.t
  (** Convert the character to a human-readable native string (in the
      spirit of [sprintf "%s"]). *)

  val compare: t -> t -> int
  (** Comparison function (as expected by most common functors in the
      ecosystem). *)

  val is_whitespace: t -> bool
  (** Tell whether a character is considered whitespace. *)

end (* BASIC_CHARACTER *)

(** API definition of conversions from native OCaml strings to a
    given string type or vice-versa. *)
module type NATIVE_CONVERSIONS = sig

  type t
  (** The string type. *)

  val of_native_string: string -> (t, [> `wrong_char_at of int ]) result
  (** Convert a native string to the current representation.
      [of_native_string] returns [`Error (`wrong_char_at index)]
      when the native string contains a character not representable
      with the type [character] at [index]. *)

  val of_native_substring: string -> offset:int -> length:int ->
    (t, [> `wrong_char_at of int | `out_of_bounds ]) result
  (** Convert a native string like [of_native_string] but take a
      subset of the string. *)

  val to_native_string: t -> string
  (** Serialize the string to a native string. *)

end (* NATIVE_CONVERSIONS *)

(** The minimal API implemented by string modules. *)
module type BASIC_STRING = sig

  type character
  (** A string is composed of characters. *)

  type t
  (** The type of the string. *)

  val max_string_length : int option
  (** If the representation of strings is bounded (by a constant,
      something else that the process memory), the maximum length of a
      string (assumed to be a {i number of [character]s}). *)

  val empty: t
  (** A string of zero length. *)

  val is_empty: t -> bool
  (** Test whether a string is empty. *)

  val make: int -> character -> t
  (** [make size char] builds a new string of the passed [length] where the
      character at every position is [char], like [String.make].

      [make size] may raise an exception when [size] is [< 0] or
      [> {max_string_length}] (if it is [Some _]) depending on the
      backend implementing the API. *)

  val length: t -> int
  (** Get the length of the string (i.e. the number of characters). *)

  val of_character: character -> t
  (** Make a string with one character. *)

  val of_character_list: character list -> t
  (** Make a string out of a list of characters. This function may
      also raise an exception when the required length is larger than
      {!max_string_length} (depends on the backend implementation). *)

  val to_character_list: t -> character list
  (** Explode a string into a list of characters. *)

  val get: t -> index:int -> character option
  (** Get the n-th char, indexes are not necessarily bytes, they can
      be bits. [get] returns [None] when [index] is out of bounds. *)

  val set: t -> index:int -> v:character -> t option
  (** [set str ~index ~v] creates a new string equal to [t] with
      character [v] at position [index]. [set] returns [None] when
      [index] is out of bounds. *)

  val get_exn: t -> index:int -> character
  (** Like {{!val:get}get} but fail with an exception.

      @raise Invalid_argument when [index] is not in [\[0,length)]
  *)

  val set_exn: t -> index:int -> v:character -> t
  (** Like {{!val:set} set} but fail with an exception.

      @raise Invalid_argument when [index] is not in [\[0,length)]
  *)

  val concat: ?sep:t -> t list -> t
  (** The classical [concat] function. 

      The function is subject to same limitations as
      {!of_character_list} regarding {!max_string_length}. *)

  (** By including {{!modtype:Api.NATIVE_CONVERSIONS} NATIVE_CONVERSIONS}, a
      basic string provides
      {{!val:Api.NATIVE_CONVERSIONS.of_native_string} of_native_string},
      {{!val:Api.NATIVE_CONVERSIONS.of_native_substring} of_native_substring},
      and {{!val:Api.NATIVE_CONVERSIONS.to_native_string} to_native_string}.
  *)
  include NATIVE_CONVERSIONS with type t := t

  val to_string_hum: t -> string
  (** Convert the string to a human-readable native string (à la
      [sprintf "%S"]).

      Returning an OCaml native string, the function may raise an
      exception when try to exceed [Sys.max_string_length]. *)

  val fold: t -> init:'a -> f:('a -> character -> 'a) -> 'a
  (** The standard [fold] function, see [List.fold_left] for example. *)

  val foldi: t -> init:'a -> f:(int -> 'a -> character -> 'a) -> 'a
  (** Pass an accumulator over the string's characters and their ordinals,
      starting with the first; left most. *)

  val fold2_exn: t -> t -> init:'a -> f:('a -> character -> character -> 'a) -> 'a
  (** The standard [fold2] function, see [List.fold_left2] for example. Fails on
      [t]s of different length. *)

  val compare: t -> t -> int
  (** Comparison function (as expected by most common functors in the
      ecosystem). *)

  val sub: t -> index:int -> length:int -> t option
  (** Get the sub-string of size [length] at position [index]. If
      [length] is 0, [sub] returns [Some empty] regardless of the other
      parameters. *)

  val sub_exn: t -> index:int -> length:int -> t
  (** Like {{!val:sub}sub} but throw an exception instead of returning [None].

      @raise Invalid_argument when [index] and [length] do not represent a
      valid substring.
  *)

  val slice: ?start:int -> ?finish:int -> t -> t option
  (** Create a sub-string from [start] to just before [finish] if all of the
      indices are in bounds.

      @param start defaults to [0], must be within [\[0,length)]
      @param finish default to [length], must be within [\[0,length)]
  *)

  val slice_exn: ?start:int -> ?finish:int -> t -> t
  (** Like {{!val:slice}slice} but throw an exception instead of returning [None]
      if the indices are out of bounds.

      @raise Invalid_argument when [start] or [finish] are not in their
      respective bounds. See [slice].
  *)

  val is_prefix: t -> prefix:t -> bool
  (** Does [t] start with [prefix]? *)

  val is_suffix: t -> suffix:t -> bool
  (** Does [t] end with [suffix]? *)

  val chop_prefix_exn: t -> prefix:t -> t
  (** Return a copy of [t] with [prefix] removed from the beginning.

      @raise Invalid_argument if [t] does not start with [prefix].
  *)

  val chop_prefix: t -> prefix:t -> t option
  (** Like {{!val:chop_prefix_exn}chop_prefix_exn} but return [None] instead of
      throwing an [Invalid_argument]. *)

  val chop_suffix_exn: t -> suffix:t -> t
  (** Return a copy of [t] with [suffix] removed from the end.

      @raise Invalid_argument if [t] does not end with [suffix].
  *)

  val chop_suffix: t -> suffix:t -> t option
  (** Like {{!val:chop_suffix_exn}chop_suffix_exn} but return [None] instead of
      throwing an exception. *)

  val split_at: t -> index:int -> t * t
  (** Return a tuple where the first string is a prefix of the specified length
      and the second is the rest.

      If index is [=< 0] then the first element is empty and the string is
      returned in the second element, similarly if the index is [>= length t]
      then the first element is [t] and the second is [empty]. *)

  val take: t -> index:int -> t
  (** Just the first part of [split_at]. *)

  val drop: t -> index:int -> t
  (** Just the second part of [split_at]. *)

  val compare_substring: t * int * int -> t * int * int -> int
  (** Comparison function for substrings: use as [compare_substring
      (s1, index1, length1) (s2, index2, length2)].

      Note that out-of-bounds accesses will {b not} be reported: for
      performance reasons, if the result can be decided with the
      smallest sub-string then [compare_substring] won't look
      further.

      However, if {{!val:compare_substring_strict}compare_substring_strict}
      returns [Some c] then [compare_substring] {i must} return [d] such as
      [c] = [d] or [c] × [d] > 0 (i.e. strictly same sign).

      In other words, if [sub a ~index:ia ~length:la] returns [Some suba] and
      [sub b ~index:ib ~length:lb] returns [Some subb], then
      [compare_substring (a, ia, la) (b, ib, lb)] will behave like
      [compare suba subb] (again, with the same sign).
  *)

  val compare_substring_strict: t * int * int -> t * int * int -> int option
  (** Like {{!val:compare_substring}compare_substring} but return [Some _] only
      when it is well defined (same validity criteria as {{!val:sub}sub}: if
      [length] is [0], [index] is irrelevant).

      Depending on the backend implementation, this function might be
      significantly slower than [compare_substring] (for example when
      calls to [length] are not {i O(1)}). *)

  val iter: t -> f:(character -> unit) -> unit
  (** Apply [f] on every character successively. *)

  val iteri: t -> f:(int -> character -> unit) -> unit
  (** Apply [f] on every character and its index. *)

  val iter_reverse: t -> f:(character -> unit) -> unit
  (** Apply [f] on every character successively in reverse order. *)

  val rev: t -> t
  (** Reverse the string. **)

  val map: t -> f:(character -> character) -> t
  (** Make a new string by applying [f] to all characters of the
      input. *)

  val mapi: t -> f:(int -> character -> character) -> t
  (** Make a new string by applying [f] to all characters and their indices. *)

  val map2_exn: t -> t -> f:(character -> character -> character) -> t
  (** Make a new string by applying [f] to all pairs of characters of the
      inputs. Fail if strings are not the same length. *)

  val for_all: t -> f:(character -> bool) -> bool
  (** Return [true] if-and-only-if [f] returns [true] on all characters. *)

  val exists: t -> f:(character -> bool) -> bool
  (** Return [true] if-and-only-if [f] returns [true] on at least one
      character. *)

  val take_while: t -> f:(character -> bool) -> t
  (** Take a prefix of the string until [f] returns [false]. *)

  val take_while_with_index: t -> f:(int -> character -> bool) -> t
  (** Like {{!val:take_while}take_while} but the function also takes the
      current index. *)

  val index_of_character: t -> ?from:int -> character -> int option
  (** Find the first occurrence of a character in the string (starting
      at position [from]).

      @param from default value is [0].
      If [from] is negative, [0] will be used.
      If [from >= length t], [None] will be returned.
  *)

  val index_of_character_reverse: t -> ?from:int -> character -> int option
  (** Like {{!val:index_of_character}index_of_character} but start from the
      end of the string.

      @param from defaults to [length t - 1] (end of the string).
      If [from] is negative, [None] will be returned.
      If [from >= length t], [length t - 1] will be used.
  *)

  val index_of_string: ?from:int -> ?sub_index:int -> ?sub_length:int -> t ->
      sub:t -> int option
  (** Find the first occurrence of the substring [(sub, sub_index,
      sub_length)] in a given string, starting at [from].

      @param from behaves like [from] in
      {{!val:index_of_character}index_of_character}.

      @param sub_index is constrained to [\[0, length sub)]
      @param sub_length is constrained to [\[0, length sub - sub_index)].

      For example, if called with [~sub:"abc" ~sub_index:(-1) ~sub_length:4]
      then [sub_index] and [sub_length] will be constrained to [0] and [3],
      respectively.

      If called with [~sub:"abc" ~sub_index:1 ~sub_length:3] then [sub_index]
      and [sub_length] will be constrained to [1] and [2], respectively.

      Searching for an empty string (if [sub] is empty or it is constrained via
      [sub_index] or [sub_length]) from a valid position always succeeds at
      that position ({i ie} [from]).
  *)

  val index_of_string_reverse: ?from:int -> ?sub_index:int -> ?sub_length:int ->
      t -> sub:t -> int option
  (** Like {{!val:index_of_string}index_of_string} but start from the end of the
      string.

      @param from behaves like [from] in
      {{!val:index_of_character_reverse}index_of_character_reverse}.
      @param sub_index is constrained like
      {{!val:index_of_string}index_of_string}.
      @param sub_length is constrained like
      {{!val:index_of_string}index_of_string}.
  *)

  val find: ?from:int -> ?length:int -> t -> f:(character -> bool) -> int option
  (** Find the index of the first character [c] for which [f c] is [true].

      One can restrict to the sub-string [(from, length)] (the default is to use
      the whole string, “out-of-bound” values are restricted to the bounds of
      the string). *)

  val find_reverse: ?from:int -> ?length:int -> t -> f:(character -> bool) ->
    int option
  (** Find the index of the last character [c] for which [f c] is [true].

      One can restrict to the reverse sub-string [(from, length)] (the
      default is to use the whole string, “out-of-bound” values are restricted
      to the bounds of the string). *)

  val filter_map: ?from:int -> ?length:int -> t ->
    f:(character -> character option) -> t
  (** Create a new string with the characters for which [f c] returned [Some c].

      One can restrict to the sub-string [(from, length)] (the default is to use
      the whole string, “out-of-bound” values are restricted to the bounds of
      the string). *)

  val filter: ?from:int -> ?length:int -> t -> f:(character -> bool) -> t
  (** Create a new string with the characters for which [f c] is true.

      One can restrict to the sub-string [(from, length)] (the default is to use
      the whole string, “out-of-bound” values are restricted
      to the bounds of the string). *)

  val split: t -> on:[ `Character of character | `String of t ] -> t list
  (** Split the string using [on] as separator.

      Splitting the empty string returns [\[\]].

      Splitting with [~on:(`String empty)] explodes the [t] into a list of
      one-character strings. *)

  val strip: ?on:[`Both | `Left | `Right] -> ?whitespace:(character -> bool) ->
      t -> t
  (** Remove any whitespace characters at the beginning and/or the end of the
      string

      @param on defaults to [`Both].
      @param whitespace defaults to calling
      {{!val:Api.BASIC_CHARACTER.is_whitespace}is_whitespace} of the
      implemented character.
  *)

  module Make_output (Model : OUTPUT_MODEL) : sig

    val output:  ('a, 'b, 'c) Model.channel -> t -> (unit, 'e, 'f) Model.thread
    (** Output a string to a channel. *)

  end
  (** [Make_output(Asynchronous_output_model)] provides a function
      {{!val:Api.BASIC_STRING.Make_output.output}output}
      given an {{!modtype:Api.OUTPUT_MODEL}OUTPUT_MODEL}. *)

end (* BASIC_STRING *)

(** This interface defines functions that may be implemented by
    particular string types that are actually mutable.

    They are considered “unsafe” because they break the immutability
    invariants assumed by the rest of this library; you'd better
    know what you're doing. *)
module type UNSAFELY_MUTABLE = sig

  type t
  type character

  val mutate: t -> index:int -> character -> (unit, [> `out_of_bounds ]) result
  (** Set the [index]-th character of the string. *)

  val mutate_exn: t -> index:int -> character -> unit
  (** Set the [index]-th character of the string, but fail with a
      non-specified exception. *)

  val blit:
    src:t -> src_index:int -> dst:t -> dst_index:int -> length:int ->
    (unit, [> `out_of_bounds ]) result
  (** Copy [length] characters from [src] (starting at [src_index]) to
      [dst] (starting at [dst_index]). *)

  val blit_exn:
    src:t -> src_index:int -> dst:t -> dst_index:int -> length:int -> unit
  (** Like {!blit} but fail with a non-specified exception. *)

end (* UNSAFELY_MUTABLE *)

(** Native {i OCaml} character. *)
module type NATIVE_CHARACTER = sig
  type t = char
  include BASIC_CHARACTER with type t := char
end

(** Native {i OCaml} string. *)
module type NATIVE_STRING = sig

  type character = char
  type t = string

  include BASIC_STRING
    with type t := string
    with type character := char

end (* NATIVE_STRING *)

(** Native {i OCaml} byte. *)
module type NATIVE_BYTES = sig

  type character = char
  type t = bytes

  include BASIC_STRING
    with type t := bytes
    with type character := char

  include UNSAFELY_MUTABLE
    with type t := bytes
    with type character := char

end (* NATIVE_BYTES *)

(** Minimal mutable string used as argument to the {!module:Of_mutable} functor. *)
module type MINIMALISTIC_MUTABLE_STRING = sig

  type character
  (** A string is composed of character.*)

  type t
  (** The type of the string. *)

  val empty: t
  (** A string of zero length *)

  val max_string_length : int option
  (** If the representation of strings is bounded,
      the maximum length of a string. *)

  val make: int -> character -> t
  (** [make size char] builds a new string of the passed [length] where the
      character at every position is [char], like [String.make]. *)

  val length: t -> int
  (** Get the length of the string (i.e. the number of characters). *)

  val compare: t -> t -> int
  (** Comparison function for strings. *)

  val compare_char: character -> character -> int
  (** Comparison function for characters. *)

  val get: t -> int -> character
  (** Get the n-th char. *)

  val set: t -> int -> character -> unit
  (** Set the n-th char. *)

  val blit: src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
  (** [blit src src_pos dst dst_pos len] copies [len] characters starting at
      [src_pos] of [src] into [dst] starting from [dst_post]. *)

  val is_whitespace: character -> bool
  (** Tell whether a character is considered whitespace. *)

  (** {{!modtype:MINIMALISTIC_MUTABLE_STRING}MINIMALISTIC_MUTABLE_STRING} requires
      {{!val:Api.NATIVE_CONVERSIONS.of_native_string} of_native_string},
      {{!val:Api.NATIVE_CONVERSIONS.of_native_substring} of_native_substring},
      and {{!val:Api.NATIVE_CONVERSIONS.to_native_string} to_native_string}.  *)
  include NATIVE_CONVERSIONS with type t := t
end
