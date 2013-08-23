(** Sane OCaml String API *)

type ('a, 'b) result = [
  | `Ok of 'a
  | `Error of 'b
]
(** The type [result] is a reusable version the classical [Result.t]
    type. *)

module type OUTPUT_MODEL = sig
  (** A monadic thread model (like [Lwt], [Async]) and an [output]
  function. *)

  type ('a, 'b, 'c) thread
  (** The type of the threads, the type parameters are there in case
      the user needs up to 3 of them. For instance, if implement with
      [Lwt], we will have [type ('a, 'b, 'c) thread = 'a Lwt.t], but with
      [Pvem.DEFERRED_RESULT]:
      [type ('a, 'b, 'c) thread = ('a, 'b) Deferred_result.t]. *)


  type ('a, 'b, 'c) channel
  (** The of the channels, channels can have up to 3 type-parameters
      too.  *)

  val return: 'a -> ('a, 'b, 'c) thread
  (** The monadic [return]. *)

  val bind: ('a, 'b, 'c) thread -> ('a -> ('d, 'b, 'c) thread) -> ('d, 'b, 'c) thread
  (** The monadic [bind]. *)

  val output: ('a, 'b, 'c) channel -> String.t -> (unit, 'e, 'f) thread
  (** The function to output a given native string to a channel.  *)

end

module type NATIVE_CONVERSIONS = sig
  (** API definition of conversions from native OCaml strings to a
      given string type or vice-versa. *)

  type t
  (** The string type. *)

  val of_native_string: string -> (t, [> `wrong_char_at of int ]) result
  (** Convert a native string to the current reprensentation.
      [of_native_string] returns [`Error (`wrong_char_at index)]
      when the native string contains a character not representable
      with the type [character]. *)

  val of_native_substring: string -> offset:int -> length:int ->
    (t, [> `wrong_char_at of int | `out_of_bounds ]) result
  (** Convert a native string like [of_native_string] but take a
      subset of the string. *)

  val to_native_string: t -> string
  (** Serialize the string to a native string. *)

end


module type BASIC_CHARACTER = sig
  (** The minimal API implemented by characters. *)

  type t
  (** The type representing the character. *)

  val of_native_char: char -> t option
  (** Import a native [char], returns [None] if the character is not
      representable. *)

  val of_int: int -> t option
  (** Import an integer, returns [None] if there is no character for
      that value. *)

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

end

module type BASIC_STRING = sig
  (** The minimal API implemented by string modules. *)

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

  val compare: t -> t -> int
  (** Comparison function (as expected by most common functors in the
      ecosystem). *)

  val compare_substring: t * int * int -> t * int * int -> int
  (** Comparison function for substrings: use as [compare_substring
      (s1, index1, length1) (s2, index2, length2)].

      Note that out-of-bounds accesses will {b not} be reported: for
      performance reasons, if the result can be decided with the
      smallest sub-string then [compare_substring] won't look
      further.

      However, if [compare_substring_strict] returns [Some c] then
      [compare_substring] {i must} return [d] such as [c] = [d] or
      [c] × [d] > 0 (i.e. stricly same sign).

      In other words, is [sub a ~index:ia ~length:la] returns [Some suba] and
      [sub b ~index:ib ~length:lb] returns [Some subb], then
      [compare_substring (a, ia, la) (b, ib, lb)] will behave like
      [compare suba subb] (again, with the same sign).
  *)

  val sub: t -> index:int -> length:int -> t option
  (** Get the sub-string of size [length] at position [index]. If
      [length] is 0, [sub] returns [Some empty] whatever the other
      parameters. *)

  val iter: t -> f:(character -> unit) -> unit
  (** Apply [f] on every character successively. *)

  val map: t -> f:(character -> character) -> t
  (** Make a new string by applying [f] to all characters of the
      input. *)

  val for_all: t -> f:(character -> bool) -> bool
  (** Return [true] if-and-only-if [f] returns [true] on all characters. *)

  val exists: t -> f:(character -> bool) -> bool
  (** Return [true] if-and-only-if [f] returns [true] on at least one
      character. *)

  val index_of_character: t -> ?from:int -> character -> int option
  (** Find the first occurrence of a character in the string (starting
      at position [from]). *)

  val index_of_character_reverse: t -> ?from:int -> character -> int option
  (** Do like [index_of_character] but start from the end of the string. *)


  module Make_output: functor (Model: OUTPUT_MODEL) -> sig

    val output:  ('a, 'b, 'c) Model.channel -> t -> (unit, 'e, 'f) Model.thread
    (** Output a string to a channel. *)

  end
  (** [Make_output(Asynchronous_output_model)] provides a function
      {!Make_output.output} given any {!OUTPUT_MODEL}. *)

end

module type UNSAFELY_MUTABLE = sig
  (** This interface defines functions that may be implemented by
      particular string types that are actually mutable.

      They are considered “unsafe” because they break the immutability
      invariants assumed by the rest of this library; you'd better
      know what you're doing. *)

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

end

open Printf

(* Internal “Pervasives” module, tu be used in all the following
   implementations. *)
module Internal_pervasives = struct
  module List = ListLabels
  module String = StringLabels
  let (|>) x f = f x
  let return x : (_, _) result = `Ok x
  let fail x : (_, _) result = `Error x
  let bind x f =
    match x with
    | `Ok o -> f o
    | `Error e -> fail e
  let (>>=) = bind
  let dbg fmt = printf ("DBG: " ^^ fmt ^^ "\n%!")

  (* The function `List.map` adapted from `Core_kernel`'s way of
     unrolling the loops. *)
  module Core_list_map = struct

    let map_slow l ~f = List.rev (List.rev_map ~f l)

    let rec count_map ~f l ctr =
      match l with
      | [] -> []
      | [x1] ->
        let f1 = f x1 in
        [f1]
      | [x1; x2] ->
        let f1 = f x1 in
        let f2 = f x2 in
        [f1; f2]
      | [x1; x2; x3] ->
        let f1 = f x1 in
        let f2 = f x2 in
        let f3 = f x3 in
        [f1; f2; f3]
      | [x1; x2; x3; x4] ->
        let f1 = f x1 in
        let f2 = f x2 in
        let f3 = f x3 in
        let f4 = f x4 in
        [f1; f2; f3; f4]
      | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
        let f1 = f x1 in
        let f2 = f x2 in
        let f3 = f x3 in
        let f4 = f x4 in
        let f5 = f x5 in
        f1 :: f2 :: f3 :: f4 :: f5 ::
          (if ctr > 1000
           then map_slow ~f tl
           else count_map ~f tl (ctr + 1))

    let map l ~f = count_map ~f l 0

  end
end
open Internal_pervasives


module type NATIVE_CHARACTER = BASIC_CHARACTER with type t = char

module type NATIVE_STRING = sig

  include BASIC_STRING
    with type t = String.t
    with type character = char

  include UNSAFELY_MUTABLE
    with type t := String.t
    with type character := char
end

module Native_character : NATIVE_CHARACTER = struct

    type t = char

    let of_native_char x = Some x
    let of_int x =
      try Some (char_of_int x) with _ -> None
    let compare = Char.compare

    let size _ = 1

    let is_print t = ' ' <= t && t <= '~'
    let to_native_string x = String.make 1 x
    let to_string_hum x =
      if is_print x then String.make 1 x
      else sprintf "0x%2x" (int_of_char x)

    let write_to_native_string c ~buf ~index =
      try buf.[index] <- c; return 1
      with _ -> fail `out_of_bounds

    let read_from_native_string ~buf ~index =
      try Some (buf.[index], 1)
      with _ -> None

end

module Native_string : NATIVE_STRING = struct

  include StringLabels
  type character = char

  let empty = ""
  let is_empty t = (compare "" t = 0)

  let of_character = String.make 1
  let of_character_list cl =
    let length = List.length cl in
    let buf = String.make length '\x00' in
    List.iteri cl ~f:(fun i c -> buf.[i] <- c);
    buf

  let get s ~index =
    try Some (s.[index])
    with _ -> None

  let set s ~index ~v =
    if index > String.length s - 1
    then None
    else begin
      let cop = String.copy s in
      cop.[index] <- v;
      Some cop
    end
  let get_exn s ~index = s.[index]
  let set_exn s ~index ~v =
    match set s ~index ~v with None -> failwith "set_exn" | Some s -> s

  let compare_substring (a, idxa, lena) (b, idxb, lenb) =
    let module With_exns = struct
      exception Return of int
      exception Left_out of int
      exception Right_out of int
      let f () =
        try
          let shortest = min lena lenb in
          for i = 0 to shortest - 1 do
            let ca = try a.[idxa + i] with _ -> raise (Left_out i) in
            let cb = try b.[idxb + i] with _ -> raise (Right_out i) in
            let c = Char.compare  ca cb in
            if c <> 0
            then raise (Return c)
            else ()
          done;
          (Pervasives.compare (lena : int) lenb)
        with
        | Return c -> c
        | Left_out c -> (* a went out of bounds at 'c + idxa' *) -1
        | Right_out _ -> (* b went out of bounds at 'c + idxb' *)
          (* so, a is “longer” *) 1
    end in
    With_exns.f ()

  let to_native_string x = String.copy x
  let of_native_string x = return (String.copy x)
  let of_native_substring x ~offset ~length =
    if length = 0 then return ""
    else
      try return (String.sub x offset length)
      with e -> fail `out_of_bounds

  let to_string_hum x = sprintf "%S" x

  let concat ?(sep="") sl = concat ~sep sl

  let fold s ~init ~f =
    let res = ref init in
    for i = 0 to String.length s - 1 do
      res := f !res s.[i];
    done;
    !res

  let sub t ~index ~length =
    if length = 0 then Some empty else
      try Some (String.sub t index length)
      with e -> None

  let mutate_exn t ~index c = String.set t index c

  let mutate t ~index c =
    try String.set t index c; return () with _ -> fail `out_of_bounds

  let blit_exn ~src ~src_index ~dst ~dst_index ~length =
    blit ~src ~src_pos:src_index ~dst ~dst_pos:dst_index ~len:length

  let blit ~src ~src_index ~dst ~dst_index ~length =
    try blit_exn ~src ~src_index ~dst ~dst_index ~length; return ()
    with _ -> fail `out_of_bounds

  let iter t ~f = String.iter t ~f
  let map t ~f = String.map t ~f

  let for_all t ~f =
    try (iter t (fun x -> if not (f x) then raise Not_found else ()); true)
    with Not_found -> false

  let exists t ~f =
    try (iter t (fun x -> if f x then raise Not_found else ()); false)
    with Not_found -> true

  let index_of_character t ?(from=0) c =
    try Some (String.index_from t from c)
    with _ -> None

  let index_of_character_reverse t ?(from=0) c =
    try Some (String.rindex_from t from c)
    with _ -> None

  module Make_output (Model: OUTPUT_MODEL) = Model

end

(* Module to help build `{of,to}_native_[sub]string` functions.

   It is most useful while using variable sized characters. *)
module Make_native_conversions = struct


  let of_native_substring
      ~empty ~init ~on_new_character ~finalize
      ~read_character_from_native_string
      s ~offset ~length =
    if length = 0 then return empty
    else
      begin
        (if offset + length > String.length s
         then fail `out_of_bounds
         else return ())
        >>= fun () ->
        let module With_exn = struct
          exception WChar of int
          let f buf =
            let x = init () in
            try
              let rec loop index =
                if index < offset + length
                then
                  begin match read_character_from_native_string ~buf ~index with
                  | Some (s, size) when index + size <= offset + length ->
                    on_new_character x s;
                    loop (index + size)
                  | Some (_, _ (* too big size *))
                  | None -> raise (WChar index)
                  end
                else ()
              in
              loop offset;
              return (finalize x)
            with
            | WChar c -> fail (`wrong_char_at c)
        end in
        With_exn.f s
      end

  let of_native_string of_native_substring s =
    match of_native_substring s ~offset:0 ~length:(String.length s) with
    | `Ok o -> return o
    | `Error (`wrong_char_at c) -> fail (`wrong_char_at c)
    | `Error `out_of_bounds -> (* There is a bug ! *) assert false


  let to_native_string_knowing_size
      ~future_size ~iter ~write_char_to_native_string l =
    let length = future_size l in
    let buf = String.make length 'B' in
    let index = ref 0 in
    iter l ~f:begin fun c ->
      match write_char_to_native_string c ~buf ~index:!index with
      | `Ok siz ->  index := !index + siz
      | `Error `out_of_bounds ->
        failwith "Bug in Make_native_conversions.to_native_string"
    end;
    buf

end





module List_of (Char: BASIC_CHARACTER) :
  BASIC_STRING
  with type character = Char.t
  with type t = Char.t list = struct

  type character = Char.t

  type t = character list

  let empty = []
  let is_empty = (=) []

  let make length c =
    let rec loop n acc =
      if n >= length then acc else loop (n + 1) (c :: acc)
    in
    loop 0 []

  let of_character c = [c]
  let of_character_list cl = cl

  let get sl ~index =
    try Some (List.nth sl index) with _ -> None

  let set s ~index ~v =
    let rec loop n acc = function
    | [] -> None
    | q :: t when n = index ->
      Some (List.rev_append acc (v :: t))
    | q :: t ->
      loop (n + 1) (q :: acc) t
    in
    loop 0 [] s

  let get_exn s ~index =
    match get s ~index with None -> failwith "get_exn" | Some s -> s
  let set_exn s ~index ~v =
    match set s ~index ~v with None -> failwith "set_exn" | Some s -> s

  let iter t ~f = List.iter t ~f
  let fold t ~init ~f = List.fold_left t ~init ~f
  let map = Core_list_map.map
  let for_all t ~f = List.for_all t ~f
  let exists t ~f = List.exists t ~f

  let compare (a : Char.t list) (b: Char.t list) = compare a b
  let of_native_substring s ~offset ~length =
    Make_native_conversions.of_native_substring
      ~empty ~init:(fun () -> ref [])
      ~on_new_character:(fun x c -> x := c :: !x)
      ~finalize:(fun x -> List.rev !x)
      ~read_character_from_native_string:Char.read_from_native_string
      s ~offset ~length

  let of_native_string s =
    Make_native_conversions.of_native_string
      of_native_substring s

  let to_native_string l =
    Make_native_conversions.to_native_string_knowing_size
      ~future_size:(fun l ->
          List.fold_left l ~init:0 ~f:(fun sum c -> sum + Char.size c))
      ~iter ~write_char_to_native_string:Char.write_to_native_string
      l

  let to_string_hum l = sprintf "%S" (to_native_string l)

  let concat ?(sep=[]) ll =
    match ll with
    | [] -> []
    | hh :: tt ->
      let x = ref (List.rev hh) in
      List.iter tt ~f:(fun l ->
        x := List.rev_append sep !x;
        x := List.rev_append l !x;
        );
      List.rev !x

  let length = List.length



  let sub t ~index ~length =
    let r = ref [] in
    let c = ref 0 in
    try
      List.iteri t ~f:(fun i a ->
          if i >= index + length then raise Not_found;
          if index <= i then (
            r:= a :: !r;
            incr c;
          );
        );
      if !c = length then Some (List.rev !r) else None
    with
    | Not_found -> Some (List.rev !r)

  let index_of_character t ?(from=0) c =
    let index = ref 0 in
    try begin
      List.iter t ~f:(fun x ->
          if !index >= from
          then
            if x = c
            then failwith "found"
            else incr index
          else incr index);
      None
    end
    with _ -> Some !index

  let index_of_character_reverse t ?(from=0) c =
    let length, rev =
      let rec loop lgth acc = function
      | [] -> (lgth, acc)
      | h :: t -> loop (lgth + 1) (h :: acc) t in
      loop 0 [] t
    in
    match index_of_character rev ~from:(length - from - 1) c with
    | Some c -> Some (length - c - 1)
    | None -> None

  let compare_substring (a, idxa, lena) (b, idxb, lenb) =
    let module With_exns = struct
      exception Left
      exception Right
      let rec drop_until ~exn idx l =
        match idx, l with
        | 0, l -> l
        | more, [] -> raise exn
        | more, h :: t -> drop_until ~exn (more - 1) t
      let f () =
        begin try
          let rec cmp l1 l2 len1 len2 =
            if len1 < 0 then raise Left;
            if len2 < 0 then raise Right;
            match l1, l2 with
            | _, _ when len1 = 0 && len2 = 0 -> 0
            | _, _ when len1 = 0 -> -1
            | _, _ when len2 = 0 -> 1
            | [], [] when len1 = 0 || len2 = 0 -> Pervasives.compare lena lenb
            | [], _ when len1 > 0 -> raise Left
            | _, [] when len2 > 0 -> raise Right
            | h1 :: t1, h2 :: t2 when Char.compare h1 h2 = 0 ->
              cmp t1 t2 (len1 - 1) (len2 - 1)
            | h1 :: _, h2 :: _ -> Char.compare h1 h2
            | _, _ -> assert false (* calming down the warnings.. *)
          in
          if lena = 0 && lenb = 0 then 0
          else (
            let aa = drop_until ~exn:Left idxa a in
            let bb = drop_until ~exn:Right idxb b in
            (cmp aa bb lena lenb)
          )
        with
        | Left -> -1
        | Right -> 1
        | Failure s -> 1
          (* dbg "(%d, %d/%d) Vs (%d, %d/%d) %s" idxa lena (length a) idxb lenb (length b) s; *)
        end
    end in
    With_exns.f ()

  module Make_output (Model: OUTPUT_MODEL) = struct

    let (>>=) = Model.bind

    let output chan l =
      List.fold_left l ~init:(Model.return ()) ~f:(fun prev_m c ->
          prev_m >>= fun () ->
          Model.output chan (Char.to_native_string c))

  end

end

module Int_utf8_character : BASIC_CHARACTER with type t = int = struct

    type t = int

    let of_native_char x = Some (int_of_char x)

    let compare (i: int) (j : int) = compare i j
    let of_int x =
      if x land 0x7FFF_FFFF = x then Some x else None

    let size x =
      if x <=        0x7f then 1 else
      if x <=       0x7ff then 2 else
      if x <=      0xffff then 3 else
      if x <=   0x1f_ffff then 4 else
      if x <=  0x3ff_ffff then 5 else
      if x <= 0x7fff_ffff then 6 else 0

    let is_print t = int_of_char ' ' <= t && t <= int_of_char '~'

    let to_string_hum x =
      if is_print x then String.make 1 (char_of_int x)
      else sprintf "&#x%X;" x

    let write_to_native_string c ~buf ~index =
      let sz = size c in
      try
        let first_byte =
          match sz with
          | 1 -> ((c lsr  0) land 0b0111_1111) lor 0b0000_0000
          | 2 -> ((c lsr  6) land 0b0001_1111) lor 0b1100_0000
          | 3 -> ((c lsr 12) land 0b0000_1111) lor 0b1110_0000
          | 4 -> ((c lsr 18) land 0b0000_0111) lor 0b1111_0000
          | 5 -> ((c lsr 24) land 0b0000_0011) lor 0b1111_1000
          | 6 -> ((c lsr 30) land 0b0000_0001) lor 0b1111_1100
          | _ -> assert false in
        buf.[index] <- char_of_int first_byte;
        for i = 2 to sz  do
          let ith_byte =
            ((c lsr (6 * (i - 2))) land 0b0011_1111) lor 0b1000_0000 in
          buf.[index + sz - i + 1] <- char_of_int ith_byte;
        done;
        return sz
      with _ -> fail `out_of_bounds

    let read_from_native_string ~buf ~index =
      try
        let first_char = buf.[index] |> int_of_char in
        let size, mask =
          if first_char lsr 7 = 0 then (1, 0b0111_1111)
          else if first_char lsr     5 =     0b110 then (2, 0b0001_1111)
          else if first_char lsr     4 =    0b1110 then (3, 0b0000_1111)
          else if first_char lsr     3 =   0b11110 then (4, 0b0000_0111)
          else if first_char lsr     2 =  0b111110 then (5, 0b0000_0011)
          else if first_char lsr     1 = 0b1111110 then (6, 0b0000_0001)
          else raise Not_found
        in
        let the_int = ref (first_char land mask) in
        for i = 1 to size - 1 do
          let the_char = buf.[index + i] |> int_of_char in
          if (the_char lsr 6) = 0b10
          then (
            the_int := (!the_int lsl 6) lor (the_char land 0b0011_1111);
          ) else raise Not_found;
        done;
        Some (!the_int, size)
      with _ -> None

    let to_native_string x =
      let buf = String.make (size x) 'B' in
      begin match write_to_native_string x ~buf ~index:0 with
      | `Ok _ -> ()
      | `Error e -> assert false
      end;
      buf

end


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

  include NATIVE_CONVERSIONS with type t := t
end

module Of_mutable
    (S: MINIMALISTIC_MUTABLE_STRING) :
  BASIC_STRING
  with type character = S.character
  with type t = S.t = struct

  include S
  let is_empty s =
    try ignore (S.get s 0); false with _ -> true

  let get t ~index = try Some (get t index) with _ -> None
  let set t ~index ~v:c =
    let lgth = length t in
    if index < 0 || lgth <= index then None
    else Some (
        let res = make lgth (S.get t 0) in
        blit ~dst:res ~dst_pos:0 ~src:t ~src_pos:0 ~len:lgth;
        S.set res index c;
        res)

  let get_exn s ~index = S.get s index
  let set_exn s ~index ~v =
    match set s ~index ~v with None -> failwith "set_exn" | Some s -> s

  let of_character c = make 1 c
  let of_character_list cl =
    match cl with
    | [] -> empty
    | one :: more ->
      let res = make (List.length cl) one in
      List.iteri more ~f:(fun  i c -> S.set res i c);
      res

  let rec concat  ?(sep=empty) tl =
    match tl with
    | [] -> empty
    | one :: more ->
      begin try
        let first_char =
          try S.get one 0
          with _ -> S.get sep 0
        in
        let sep_length = S.length sep in
        let total_length =
          List.fold_left ~init:(S.length one) more ~f:(fun prev s ->
              prev + sep_length + S.length s) in
        let dst = make total_length first_char in
        let index = ref 0 in
        blit ~dst ~dst_pos:!index ~src:one ~src_pos:0 ~len:(length one);
        index := !index + (length one);
        List.iter more ~f:(fun s ->
            blit ~dst ~dst_pos:!index ~src:sep ~src_pos:0 ~len:sep_length;
            index := !index + sep_length;
            blit ~dst ~dst_pos:!index ~src:s ~src_pos:0 ~len:(length s);
            index := !index + (length s);
          );
        dst
      with _ ->
        concat more ~sep (* both one and sep are empty *)
      end

  let iter t ~f =
    for i = 0 to length t - 1 do
      f (S.get t i)
    done

  let fold t ~init ~f =
    let x = ref init in
    for i = 0 to length t - 1 do
      x := f !x (S.get t i)
    done;
    !x

  let map t ~f =
    let lgth = (length t) in
    if lgth = 0
    then empty
    else begin
      let res = make lgth (S.get t 0) in
      for i = 1 to lgth - 1 do
        S.set res i (f (S.get t i))
      done;
      res
    end

  let for_all t ~f =
    try
      iter t (fun c -> if not (f c) then raise Not_found);
      true
    with _ -> false

  let exists t ~f =
    try
      iter t (fun c -> if (f c) then raise Not_found);
      false
    with _ -> true

  let sub t ~index ~length =
    if length = 0 then Some empty else
      begin
        let lgth = S.length t in
        if lgth = 0
        then None (* `length <> 0` *)
        else begin
          try
            let res = make length (S.get t index) in
            for i = 1 to length - 1 do
              S.set res i (S.get t (index + i))
            done;
            Some res
          with _ -> None
        end
      end

  let to_string_hum t = to_native_string t |> sprintf "%S"

  let index_of_character t ?(from=0) c =
    let res = ref None in
    try
      for i = from to length t - 1 do
        if S.get t i = c then (res:= Some i; raise Not_found)
      done;
      None
    with _ -> !res

  let index_of_character_reverse t ?from c =
    let from = match from with Some s -> s  | None -> length t - 1 in
    let res = ref None in
    try
      for i = from downto 0 do
        if S.get t i = c then (res:= Some i; raise Not_found)
      done;
      None
    with _ -> !res

  let compare_substring (a, idxa, lena) (b, idxb, lenb) =
    let module With_exns = struct
      exception Return of int
      exception Left_out of int
      exception Right_out of int
      let f () =
        try
          let shortest = min lena lenb in
          for i = 0 to shortest - 1 do
            let ca = try S.get a (idxa + i) with _ -> raise (Left_out i) in
            let cb = try S.get b (idxb + i) with _ -> raise (Right_out i) in
            let c = S.compare_char  ca cb in
            if c <> 0
            then raise (Return c)
            else ()
          done;
          (Pervasives.compare (lena : int) lenb)
        with
        | Return c -> c
        | Left_out c -> (* a went out of bounds at 'c + idxa' *) -1
        | Right_out _ -> (* b went out of bounds at 'c + idxb' *)
          (* so, a is “longer” *) 1
    end in
    With_exns.f ()
      (*
    let module With_exns = struct
      exception Return of int
      let f () =
        try
          let shortest = min lena lenb in
          for i = 0 to shortest - 1 do
            let c = S.compare_char (S.get a (idxa + i)) (S.get b (idxb + i)) in
            if c <> 0
            then raise (Return c)
            else ()
          done;
          Some (Pervasives.compare (lena : int) lenb)
        with
        | Return c -> Some c
        | _ -> None
    end in
    With_exns.f ()
  *)



  module Make_output (Model: OUTPUT_MODEL) = struct

    let (>>=) = Model.bind

    let output chan t =
      Model.output chan (to_native_string t)

  end
end
