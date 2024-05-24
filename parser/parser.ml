open Optitrust
open Printf

(** [dune exec parser foo.cpp] produces a file [foo.ser],
    that contains the AST of the file (type [trm] of [src/ast/ast.ml],
    serialized using OCaml's marshal. *)

(** Build: dune build parser/parser.exe **)

let debug = ref false

let verbose = ref false

let force = ref false

(* filenames passed as arguments to the binary *)
let filenames = ref []

(* [spec]: possible command line arguments. *)
let spec : (string * Arg.spec * string) list =
   [ ("-debug", Arg.Set debug, " debug flag for future use");
     ("-verbose", Arg.Set verbose, " verbose mode");
     ("-force", Arg.Set force, " ensures that the input file is parsed even if it is already cached");
   ]


let _ =
  (* Parsing of command line *)
  Arg.parse
    (Arg.align spec)
    (fun arg -> filenames := arg :: !filenames)
    "Usage: ./parser arg1.cpp .. argN.cpp\n";

  filenames := List.rev !filenames;

  if !force
    then Flags.ignore_serialized := true;

  if !verbose then begin
    printf "Parser starting\n";
    Flags.debug_parsing_serialization := true
  end;

  List.iter (fun filename ->
    let _ast = CParsers.parse ~serialize:true filename in
    if !verbose then printf "Parser produced serialized file for %s\n" filename;
    ) !filenames;

  ()


