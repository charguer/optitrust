open Optitrust_utils

let verbose = ref false
let force = ref false
let dump_clang_ast = ref None

(** [spec]: possible command line arguments. *)
let spec : (string * Arg.spec * string) list = [
     ("--verbose", Arg.Set verbose, " verbose mode");
     ("-v", Arg.Set verbose, " shorthand for --verbose");
     ("--force", Arg.Set force, " ensures that the input file is parsed even if it is already cached");
     ("-f", Arg.Set force, " shorthand for --force");
     ("--dump-clang-ast", Arg.String (fun f -> dump_clang_ast := Some f), " dump the AST as produced by clang into a file")
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
