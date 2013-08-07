#! /usr/bin/env ocaml
 open Printf
 module List = ListLabels
 let say fmt = ksprintf (printf "please.ml-> %s\n%!") fmt
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

let build () =
  in_build_directory (fun () ->
      chain [
        "pwd";
        "cp ../sosa.ml .";
        "ocamlc -c sosa.ml -annot -o _build/sosa.cmo";
        "ocamlopt -c sosa.ml -annot -o _build/sosa.cmx";
        "ocamlc sosa.cmo -a -o sosa.cma";
        "ocamlopt sosa.cmx -a -o sosa.cmxa";
        "ocamlopt sosa.cmx -a -o sosa.a";
        "ocamlopt sosa.cmx -shared -o sosa.cmxs";
      ];
    )

let install () =
    in_build_directory (fun () ->
        chain [
          "ocamlfind install sosa ../META sosa.cmx sosa.cmo sosa.cma sosa.cmi sosa.cmxa sosa.cmxs sosa.a"
        ])

let name = "sosa"
let () =
  match args with
  | exec :: []
  | exec :: "build" :: [] ->
    say "Building.";
    build ();
    say "Done."
  | exec :: "doc" :: [] ->
    say "Building the documentation.";
    in_build_directory (fun () ->
        chain [
          sprintf "ocamldoc -charset UTF-8 -colorize-code -html %s.ml" name;
        ])
  | exec :: "install" :: [] ->
    say "Installing";
    install ();
      | exec :: "uninstall" :: [] ->
    chain [
      sprintf "ocamlfind remove %s" name
    ]
  | exec :: "clean" :: []
  | exec :: "C" :: [] ->
    cmdf "rm -fr _build"
  | _ ->
    say "usage: ocaml %s [build|test|install|uninstall|clean]" Sys.argv.(0)

