#! /usr/bin/env ocaml
 open Printf
 module List = ListLabels
 let say fmt = ksprintf (printf "Please-> %s\n%!") fmt
 let cmdf fmt =
   ksprintf (fun s -> ignore (Sys.command s)) fmt
 let chain l =
   List.iter l ~f:(fun cmd ->
       printf "! %s\n%!" cmd;
       if  Sys.command cmd = 0
       then ()
       else ksprintf failwith "%S failed." cmd
     )
 let args = Array.to_list Sys.argv
 let in_build_directory f =
   cmdf "mkdir -p _build/";
   Sys.chdir "_build/";
   begin try
     f ();
   with
     e ->
     Sys.chdir "../";
     raise e
   end;
   Sys.chdir "../";
   ()

 let sys_string cmd =
  let ic = Unix.open_process_in cmd in
  let line = input_line ic in
  close_in ic;
  line

let build coverage =
  let pp_step =
    if coverage then
      let d = sys_string "ocamlfind query bisect" in
      if Sys.file_exists d then
        sprintf "-pp 'camlp4o str.cma %s/bisect_pp.cmo' -package bisect " d
      else begin
        say "Bisect is not installed, no coverage.";
         ""
      end
    else
      ""
  in
  in_build_directory (fun () ->
    chain
    [
        "cp ../sosa.ml .";
        sprintf "ocamlfind ocamlc %s -c sosa.ml -o sosa.cmo" pp_step;
        sprintf "ocamlfind ocamlopt %s -c sosa.ml  -annot -bin-annot -o sosa.cmx" pp_step;
        "ocamlc sosa.cmo -a -o sosa.cma";
        "ocamlopt sosa.cmx -a -o sosa.cmxa";
        "ocamlopt sosa.cmxa sosa.a -shared -o sosa.cmxs";
    ])

let install () =
    in_build_directory (fun () ->
        chain [
          "ocamlfind install sosa ../META sosa.cmx sosa.cmo sosa.cma sosa.cmi sosa.cmxa sosa.cmxs sosa.a sosa.o"
        ])

let uninstall () =
    chain [
      "ocamlfind remove sosa"
    ]


let merlinize () =
    chain [
      "echo 'S .' > .merlin";
      "echo 'B _build' >> .merlin";

     ]


let build_doc () =
    in_build_directory (fun () ->
        chain [
          "mkdir -p doc";
                         sprintf "ocamlfind ocamldoc  -charset UTF-8 -keep-code -colorize-code -html sosa.ml -d doc/";
        ])


let name = "sosa"

let () = begin
match args with
| _ :: "build" :: [] ->(
say "Building";
build false;
say "Done."
)
| _ :: "build" :: "coverage" :: [] ->(
say "Building";
build true;
say "Done."
)
| _ :: "build_doc" :: [] ->(
say "Building Documentation";
build_doc ();
say "Done."
)
| _ :: "install" :: [] ->(
say "Installing";
install ();
say "Done."
)
| _ :: "uninstall" :: [] ->(
say "Uninstalling";
uninstall ();
say "Done."
)
| _ :: "merlinize" :: [] ->(
say "Updating `.merlin` file";
merlinize ();
say "Done."
)
| _ :: "clean" :: [] ->(
say "Cleaning";
cmdf "rm -fr _build";
say "Done."
)
| _ ->(
say "usage: ocaml %s [build|install|uninstall|clean|build_doc|melinize]" Sys.argv.(0)
)

end


