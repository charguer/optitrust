(* Flag to activate the printing of debug information -- NOT SUPPORTED YET *)
let verbose : bool ref = ref false

(* Flag to meansure the time taken by each transformation *)
let analyse_time : bool ref = ref false

(* Flagsto meansure the time taken by each step within a transformation
   (in particular, time to resolve targets, to set up marks, etc.) *)
let analyse_time_details : bool ref = ref false

(* Flag to dump OptiTrust AST, both in the form of a '.ast' and '_enc.cpp' files *)
let dump_ast_details : bool ref = ref false

(* DEPRECATED? Flag to call [Trace.dump_last !dump_last] instead of [Trace.dump].
   Note: incompatible with the use of [switch] in scripts, currently.
    *)
let dump_last_default = -1
let dump_last : int ref = ref dump_last_default

(* Call [Trace.dump_traces_to_js] in addition to [Trace.dump] at the end of the script. *)
let dump_trace : bool ref = ref false

(* Flag to print the line numbers at which reparsing is triggered *)
let debug_reparse : bool ref = ref false

(* Flag to force reparsing of the entire file at each entry of a big step *)
let reparse_at_big_steps : bool ref = ref false

(* Flag to report on the progress of big steps during a script execution *)
let report_big_steps : bool ref = ref false

(* Flag to report more about file manipulations performed by the tool *)
let verbose_mode : bool ref = ref false

(* Flag to enable "light diffs", whereby we hide the function body of all the
   toplevel functions that are not affected by the transformation. *)
let use_light_diff : bool ref = ref false

(* Flag for using only raw ast, when parsing and printing *)
let use_new_encodings : bool ref = ref true

(* Flag used for debugging the [cfeatures_elim/intro] functions, by bypassing them *)
let bypass_cfeatures : bool ref = ref false

(* Flag used for unit tests on targets that use the show function *)
let execute_show_even_in_batch_mode : bool ref = ref false

(* Option to deal with serialized AST as input and output:
  | Serialized_None: do not read or write any serialized ast, just parse the input file.
  | Serialized_Build: parse the input file, save its serialized ast, exit
  | Serialized_Use: do not parse the input file, simply read the corresponding serialized ast
  | Serialized_Make: NOT YET IMPLEMENTED: first call 'make inputfile.ser', assuming the Makefile
                     features a rule for this (invoking the program with the Serialized_Build option,
                     with the appropriate dependencies); then load the serialized file just like
                     Serialized_Use would do, and continue the execution.
  | Serialized_Auto: if the serialized ast is up to date wrt the input file, read the serialized ast,
                     else parse the input file and save its serialized ast; then continue the execution.
*)

type serialized_mode =
  | Serialized_None
  | Serialized_Build
  | Serialized_Use
  | Serialized_Make
  | Serialized_Auto

let serialized_mode : serialized_mode ref = ref Serialized_None

let process_serialized_input (mode : string) : unit =
  serialized_mode := match mode with
  | "none" -> Serialized_None
  | "build" -> Serialized_Build
  | "use" -> Serialized_Use
  | "make" -> Serialized_Make
  | "auto" -> Serialized_Auto
  | _ -> Serialized_None

(* Option for exiting the program when reaching a '!!' (step) after a specific line number *)
let exit_line : int ref = ref max_int

let get_exit_line () : int option =
  if !exit_line = max_int
    then None
    else Some !exit_line

(* Flag for the treatment of the exit line to ignore the small steps ('!!') and only
   consider big steps ('!^'). *)
let only_big_steps : bool ref = ref false

(* List of options *)
let spec =
  Arg.align [
     ("-verbose", Arg.Set verbose, " activates debug printing");
     ("-exit-line", Arg.Set_int exit_line, " specify the line after which a '!!' or '!!!' symbol should trigger an exit");
     ("-report-big-steps", Arg.Set report_big_steps, " report on the progress of the execution at each big step");
     ("-only-big-steps", Arg.Set only_big_steps, " consider only '!!!' for computing exit lines");
     ("-debug-reparse", Arg.Set debug_reparse, " print on stdout the line number at which each reparse is performed");
     ("-reparse-at-big-steps", Arg.Set reparse_at_big_steps, " force reparsing at every big step (implies -debug-reparse)");
     ("-dump-trace", Arg.Set dump_trace, " produce a JS file with all the steps performed by the transformation script");
     ("-dump-last", Arg.Set_int dump_last, " dump outputs the number of desired last steps; only for interactive mode");
     ("-dump-ast-details", Arg.Set dump_ast_details, " produce a .ast and a _enc.cpp file with details of the ast");
     ("-analyse-time", Arg.Set analyse_time, " produce a file reporting on the execution time");
     ("-analyse-time-details", Arg.Set analyse_time_details, " produce more details in the file reporting on the execution time (implies -analyse_time)");
     ("-serialized-input", Arg.String process_serialized_input, " choose between 'build', 'use', 'make' or 'auto'.");
     ("-disable-light-diff", Arg.Clear use_light_diff, " disable light diff");
     ("-cparser", Arg.String Parsers.set_selected_parser, "specify the parser among 'clang', 'menhir', 'default' and 'all' ");
     ("-use-old-encodings", Arg.Clear use_new_encodings, "FOR DEV ONLY");
     ("-v", Arg.Set verbose_mode, " enable verbose regarding files processed out produced (not fully implemented yet).");
     (* LATER: a -dev flag to activate a combination of dump *)
  ]

(* Processing of flags than imply other flags *)
let fix_flags () =
  if !analyse_time_details then analyse_time := true;
  if !reparse_at_big_steps then debug_reparse := true

(* Flag used for a hack used by function [doc_script_cpp], for generating the output
   associated with the documentation of a unit test, before running the main contents
   of the file. *)
let documentation_save_file_at_first_check = ref ""


(*

  Note: to see a diff at the level of the OptiTrust AST, use:
    -dump-ast-details
  and the shortcut "ctrl+shift+f6" for opening the diff between [*_before_enc.cpp] and [*_after_enc.cpp]

*)
