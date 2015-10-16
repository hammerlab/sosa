(** The module types that define Sosa's API. *)

type ('a, 'b) result = [
  | `Ok of 'a
  | `Error of 'b
]
(** The type [result] is a reusable version the classical [Result.t]
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
  (** The of the channels, channels can have up to 3 type-parameters
      too.  *)

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
      implementation-specific (c.f. {!write_to_native_string}) *)

  val write_to_native_string: t -> buf:String.t -> index:int -> (int, [> `out_of_bounds]) result
  (** [write_to_native_string c ~buf ~index] serializes
      the character [c] at position [index] in the native string
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
      with the type [character]. *)

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
  (** A string is a string of characters. *)

  type t
  (** The type of the string. *)

  val empty: t
  (** An “empty” string. *)

  val is_empty: t -> bool
  (** Test whether a string is empty. *)

  val make: int -> character -> t
  (** Build a new string like [String.make]. *)

  val length: t -> int
  (** Get the length of the string (i.e. the number of characters). *)

  val of_character: character -> t
  (** Make a string with one character. *)

  val of_character_list: character list -> t
  (** Make a string out of a list of characters. *)

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
  (** Like [get] but fail with an exception *)

  val set_exn: t -> index:int -> v:character -> t
  (** Like [set] but fail with an exception *)

  val concat: ?sep:t -> t list -> t
  (** The classical [concat] function. *)

  include NATIVE_CONVERSIONS with type t := t
  (** By including {!NATIVE_CONVERSIONS}, a
      basic string provides
      {!NATIVE_CONVERSIONS.of_native_string},
      {!NATIVE_CONVERSIONS.of_native_substring}, and
      {!NATIVE_CONVERSIONS.to_native_string}.
  *)

  val to_string_hum: t -> string
  (** Convert the string to a human-readable native string (à la
      [sprintf "%S"]). *)

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
      [length] is 0, [sub] returns [Some empty] whichever the other
      parameters are. *)

  val sub_exn: t -> index:int -> length:int -> t
  (** Do like [sub] but throw an exception instead of returning [None] *)

  val slice: ?start:int -> ?finish:int -> t -> t option
  (** Create a sub-string from the [start] (default 0, within \[0,length\))
      position to before the [finish] (default length, within \[0,length\])
      if all of the indices are in bounds.  *)

  val slice_exn: ?start:int -> ?finish:int -> t -> t
  (** Like [slice] but throw an exception instead of returning [None] *)

  val is_prefix: t -> prefix:t -> bool
  (** Does [t] start with [prefix] ? *)

  val is_suffix: t -> suffix:t -> bool
  (** Does [t] end with [suffix] ? *)

  val chop_prefix_exn: t -> prefix:t -> t
  (** Return a copy of [t] with [prefix] removed from the beginning.
      Throws Invalid_argument if [t] does not start with [prefix]. *)

  val chop_prefix: t -> prefix:t -> t option
  (** Like [chop_prefix_exn] but return [None] instead of throwing
      an exception. *)

  val chop_suffix_exn: t -> suffix:t -> t
  (** Return a copy of [t] with [suffix] removed from the end.
      Throws Invalid_argument if [t] does not end with [suffix]. *)

  val chop_suffix: t -> suffix:t -> t option
  (** Like [chop_suffix_exn] but return [None] instead of throwing
      an exception. *)

  val split_at: t -> index:int -> t * t
  (** Return a tuple where the first string is a prefix of the specified length
      and the second is the rest. If index is [=< 0] then the first element is
      empty and the string is returned in the second element, similarly if the
      index is [>= length t] then the first element is [t] and the second is
      [empty]. *)

  val take: t -> index:int -> t
  (** Just the first part of split_at *)

  val drop: t -> index:int -> t
  (** Just the second part of split_at *)

  val compare_substring: t * int * int -> t * int * int -> int
  (** Comparison function for substrings: use as [compare_substring
      (s1, index1, length1) (s2, index2, length2)].

      Note that out-of-bounds accesses will {b not} be reported: for
      performance reasons, if the result can be decided with the
      smallest sub-string then [compare_substring] won't look
      further.

      However, if {!compare_substring_strict} returns [Some c] then
      [compare_substring] {i must} return [d] such as [c] = [d] or
      [c] × [d] > 0 (i.e. strictly same sign).

      In other words, if [sub a ~index:ia ~length:la] returns [Some suba] and
      [sub b ~index:ib ~length:lb] returns [Some subb], then
      [compare_substring (a, ia, la) (b, ib, lb)] will behave like
      [compare suba subb] (again, with the same sign).
  *)

  val compare_substring_strict: t * int * int -> t * int * int -> int option
  (** Do like {!compare_substring} but return [Some _] only when it is
      well defined (same validity criteria as {!sub}: if [length]
      is [0], [index] is irrelevant).

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
  (** Like {!take_while} but the function also takes the current index. *)

  val index_of_character: t -> ?from:int -> character -> int option
  (** Find the first occurrence of a character in the string (starting
      at position [from]).
      
      Default value for [from] is [0].
      If [from] is negative, [0] will be used.
      If [from >= length t], [None] will be returned.
  *)

  val index_of_character_reverse: t -> ?from:int -> character -> int option
  (** Do like [index_of_character] but start from the end of the string.

      Default value for [from] is [length t - 1] (end of the string).
      If [from] is negative, [None] will be returned.
      If [from >= length t], [length t - 1] will be used.
  *)

  val index_of_string: ?from:int ->
    ?sub_index:int -> ?sub_length:int -> t -> sub:t -> int option
  (** Find the first occurrence of the substring [(sub, sub_index,
      sub_length)] in a given string, starting at index [from].
  
      The [from] parameter behaves like for {!index_of_character}.

      The [(sub_index, sub_length)] parameters are constrained to [(0, length
      sub)], for example, if [sub] is ["abc"], [(-1, 4)] will be equivalent to
      [(0, 3)], [(1, 3)] will be equivalent to [(1, 2)].

      Searching for an empty string [from] a valid position always succeeds at
      that position.
  *)

  val index_of_string_reverse: ?from:int ->
    ?sub_index:int -> ?sub_length:int -> t -> sub:t -> int option
  (** Do like [index_of_string] but start from the end of the string. 

      The [from] parameter behaves like for {!index_of_character_reverse}.

      The [(sub_index, sub_length)] parameters are constrained like in
      {!index_of_string}.  *)

  val find: ?from:int -> ?length:int -> t -> f:(character -> bool) -> int option
  (** Find the index of the first character [c] for which [f c] is [true]. One
      can restrict to the sub-string [(from, length)] (the
      default is to use the whole string, “out-of-bound” values are restricted
      to the bounds of the string). *)

  val find_reverse:
    ?from:int -> ?length:int -> t -> f:(character -> bool) -> int option
  (** Find the index of the last character [c] for which [f c] is [true]. One
      can restrict to the reverse sub-string [(from, length)] (the
      default is to use the whole string,  “out-of-bound” values are restricted
      to the bounds of the string). *)

  val filter_map: ?from:int -> ?length:int -> t -> 
    f:(character -> character option) -> t
  (** Create a new string with the characters for which [f c] returned 
      [Some c]. One can restrict to the sub-string [(from, length)] (the
      default is to use the whole string, “out-of-bound” values are restricted
      to the bounds of the string). *)

  val filter: ?from:int -> ?length:int -> t -> f:(character -> bool) -> t
  (** Create a new string with the characters for which [f c] is true.
      One can restrict to the sub-string [(from, length)] (the
      default is to use the whole string, “out-of-bound” values are restricted
      to the bounds of the string). *)

  val split: t -> 
    on:[ `Character of character | `String of t ] ->
    t list
  (** Split the string using [on] as separator. 
      Splitting the empty string returns  [[empty]], splitting [on] the empty
      string explodes the string into a list of one-character strings. *)

  val strip: ?on:[`Both | `Left | `Right] ->
    ?whitespace:(character -> bool) -> t -> t
  (** Remove any whitespace characters at the beginning and/or the end of the
      string (default [`Both]).

      The default is to call the {!BASIC_CHARACTER.is_whitespace} function of
      the implemented character.
  *)

  module Make_output: functor (Model: OUTPUT_MODEL) -> sig

    val output:  ('a, 'b, 'c) Model.channel -> t -> (unit, 'e, 'f) Model.thread
    (** Output a string to a channel. *)

  end
  (** [Make_output(Asynchronous_output_model)] provides a function
      {!Make_output.output} given any {!OUTPUT_MODEL}. *)

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
module type NATIVE_CHARACTER = BASIC_CHARACTER with type t = char

(** Native {i OCaml} string. *) 
module type NATIVE_STRING = sig

  include BASIC_STRING
    with type t = String.t
    with type character = char

  include UNSAFELY_MUTABLE
    with type t := String.t
    with type character := char

end (* NATIVE_STRING *)

(** Abstract mutable string used as argument of the {!module:Of_mutable} functor. *)
module type MINIMALISTIC_MUTABLE_STRING = sig
  type character
  type t

  val empty: t
  val make: int -> character -> t
  val length: t -> int
  val compare: t -> t -> int
  val compare_char: character -> character -> int
  val get: t -> int -> character
  val set: t -> int -> character -> unit
  val blit: src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit

  val is_whitespace: character -> bool
  include NATIVE_CONVERSIONS with type t := t
end

