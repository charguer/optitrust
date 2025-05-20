open Optitrust_utils

let verbose = ref false
let force = ref false
let show = ref false
let dump_ocaml_ast = ref true (*TODO : put at as an option string ref, and give the file name as argument of the flag*)

(** [spec]: possible command line arguments. *)
let spec : (string * Arg.spec * string) list = [
     ("--verbose", Arg.Set verbose, " verbose mode");
     ("-v", Arg.Set verbose, " shorthand for --verbose");
     ("--stdout", Arg.Set show, " print the optitrust AST produced on stdout");
     ("--force", Arg.Set force, " ensures that the input file is parsed even if it is already cached");
     ("-f", Arg.Set force, " shorthand for --force");
     ("--dump-ocaml-ast", Arg.Set dump_ocaml_ast (*Arg.String (fun f -> dump_ocaml_ast := Some f)*), " dump the AST as produced by clang into a file") (*TODO : put back the filename reference*)
   ]

let program_path = Sys.argv.(0)
let optitrust_root = Option.value ~default:"." (Sys.getenv_opt "OPTITRUST_PATH")

let verbose_info msg =
  if !verbose then
    Tools.info msg
  else
    Printf.ifprintf () msg

let verbose_warn msg =
  if !verbose then
    Tools.warn msg
  else
    Printf.ifprintf () msg
