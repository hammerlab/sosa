(* Methods useful for constructing Sosa functor inputs. *)

open Sosa_pervasives

(** Module to help build `{of,to}_native_[sub]string` functions.
    It is most useful while using variable sized characters. *)
module Conversions = struct

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

end (* Conversions. *)
