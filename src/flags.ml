
(* TODO: perhaps split this file between runflags and syntaxflags *)
(* TODO: group options that are relevant for "reset in batching" into a record data structure;
   so that we can save its state, and restore it, in between tests;
   - options for interactivity
   - options for behavior *)

(* [code_print_width]: flag to choose the width of the printed code. *)
let code_print_width = ref 80

(* [verbose]: flag to activate the printing of debug information *)
let verbose : bool ref = ref false

(* Flag to hide stdout produced by tests when executed from ./tester *)
let hide_stdout : bool ref = ref false

(* [analyse_stats]: flag to meansure the time taken by each transformation. *)
let analyse_stats : bool ref = ref false

(* [analyse_stats_details]: flags to meansure the time taken by each step within a transformation
   (in particular, time to resolve targets, to set up marks, etc). *)
let analyse_stats_details : bool ref = ref false

(* [dump_ast_details]: flag to dump OptiTrust AST, both in the form of a '.ast' and '_enc.cpp' files. *)
let dump_ast_details : bool ref = ref false

(* [pretty_matrix_notation]: flag to display matrix macros with syntactic sugar:
  MALLOC2(n, m, sizeof(T)) --> malloc(sizeof(T[n][m]))
  x[MINDEX2(n, m, i, j)] --> x[i][j]
   *)
let pretty_matrix_notation : bool ref = ref false

(* whether to display includes AST or not. *)
let display_includes : bool ref = ref false

(* [dump_clang_ast]: flag to dump the AST as produced by clang into a specific file,
   by default "clang_ast.ml".  *)
let dump_clang_ast = ref None

let set_dump_clang_ast () : unit =
  dump_clang_ast := Some "clang_ast.ml.txt"

(* [dump_big_steps]: call [Trace.dump_big_steps] in addition to [Trace.dump] at the end of the script.
   Files are generated in the subfolder [!dump_big_steps].  *)
let dump_big_steps : string option ref = ref None

(* [set_dump_big_steps foldername]: dump bigsteps in folder [foldername]. *)
let set_dump_big_steps (foldername : string) : unit =
  dump_big_steps := Some foldername

(* LATER: document *)
(* [dump_small_steps]:  *)
let dump_small_steps : string option ref = ref None
let set_dump_small_steps (foldername : string) : unit =
  dump_small_steps := Some foldername

(* [print_backtrace_on_error] *)
let print_backtrace_on_error : bool ref = ref true
 (*LATER: make available from command line*)

(* [debug_reparse]: flag to print the line numbers at which reparsing is triggered. *)
let debug_reparse : bool ref = ref false

(* [debug_stringreprs]: flag to print stringreprs debugging info *)
let debug_stringreprs : bool ref = ref false

(* [debug_var_id]: display ids on variables when outputing C code *)
let debug_var_id : bool ref = ref false

(* [reparse_at_big_steps]: flag to force reparsing of the entire file at each entry of a big step. *)
let reparse_at_big_steps : bool ref = ref false

(* [report_big_steps]: flag to report on the progress of big steps during a script execution. *)
let report_big_steps : bool ref = ref false

(* [use_clang_format]: flag to use clang-format or not in output CPP files. *)
let use_clang_format : bool ref = ref true

(* [verbose_mode]: flag to report more about file manipulations performed by the tool. *)
let verbose_mode : bool ref = ref false

(* [use_light_diff]: flag to enable "light diffs", whereby we hide the function body of all the
   toplevel functions that are not affected by the transformation. *)
   (* TODO: could it be true by default? *)
let use_light_diff : bool ref = ref false

(* [bypass_cfeatures]: flag used for debugging the [cfeatures_elim/intro] functions, by bypassing them.
   It affects the behavior of the parsing function [c_parser],
   and the style controlling the behavior of the printing operation *)
let bypass_cfeatures : bool ref = ref false

(* [print_optitrust_syntax]: flag used for printing the optitrust AST in near-C syntax, without applying the decoding *)
let print_optitrust_syntax = ref false

(* [resource_errors_as_warnings]: Do not error on resource computation failure but only print a warning instead.
   Useful for debugging resource typing. *)
let resource_errors_as_warnings = ref false

(* [always_name_resource_hyp]: Always print named for resource hypothesis even if they were unnamed.
 * Automatically set to true during Resources.show. *)
let always_name_resource_hyp = ref false

(* [check_validity]: perform validation of transformations *)
let check_validity = ref false

(* [reparse_between_step]: always reparse between two steps *)
let reparse_between_steps = ref false

(* [recompute_resources_between_steps]: always recompute resources between two steps *)
let recompute_resources_between_steps = ref false

(* [serialized_mode]: type to deal with serialized AST ,
  | Serialized_None: do not read or write any serialized ast, just parse the input file.
  | Serialized_Build: parse the input file, save its serialized ast, exit
  | Serialized_Use: do not parse the input file, simply read the corresponding serialized ast
  | Serialized_Auto: if the serialized ast is up to date wrt the input file, read the serialized ast,
                     else parse the input file and save its serialized ast; then continue the execution.
  | Serialized_Make: NOT YET IMPLEMENTED: first call 'make inputfile.ser', assuming the Makefile
                     features a rule for this (invoking the program with the Serialized_Build option,
                     with the appropriate dependencies); then load the serialized file just like
                     Serialized_Use would do, and continue the execution.
                     [NOTE: GB: This feature seems really weird, is it worth implementing?] *)

type serialization_mode =
  | Serialized_None
  | Serialized_Build
  | Serialized_Use
  | Serialized_Auto
(*  | Serialized_Make*)

(* [serialization_mode]: mode of serialization, by default AST is not serialized. *)
let serialization_mode : serialization_mode ref = ref Serialized_None

(* [process_serialized_input mode]: based on the mode the serialized input is going to be processed
    in different ways. *)
let process_serialized_input (mode : string) : unit =
  serialization_mode := match mode with
  | "none" -> Serialized_None
  | "build" -> Serialized_Build
  | "use" -> Serialized_Use
  | "auto" -> Serialized_Auto
  | "make" -> failwith "'make' serialization is not implemented"
  | _ -> failwith "Serialization mode should be 'none', 'build', 'use', 'auto' or 'make'"

(* [execution_mode] of the script *)

type execution_mode =
  | Execution_mode_step_diff (* produce a diff for a small-step, assumes [target_line] is provided *)
  | Execution_mode_step_trace (* produce a trace for a small-step, assumes [target_line] is provided *)
  | Execution_mode_full_trace (* produce a trace, and an output file, for the full script *)
  | Execution_mode_exec (* produce only the final output file obtained at the end of the script *)

let execution_mode : execution_mode ref = ref Execution_mode_exec

let process_mode (mode : string) : unit =
  execution_mode := match mode with
  | "step-diff" -> Execution_mode_step_diff
  | "step-trace" -> Execution_mode_step_trace
  | "full-trace" -> Execution_mode_full_trace
  | "exec" -> Execution_mode_exec
  | _ -> failwith "Execution mode should be 'exec', or 'diff', or 'trace'"

(* [target_line]: indicate which line to target in execution mode
   [Execution_mode_step_trace] or [Execution_mode_step_trace]. *)
let target_line : int ref = ref (-1)

(* [get_target_line ()]: gets the targeted line *)
let get_target_line () : int =
  if !target_line = (-1)
    then failwith "Flags.get_target_line: trying to read an invalid line number";
  !target_line

(* [is_execution_mode_step ()] returns the execution mode is targeting a specific step/line *)
let is_execution_mode_step () : bool =
  match !execution_mode with
  | Execution_mode_step_diff
  | Execution_mode_step_trace -> true
  | Execution_mode_exec
  | Execution_mode_full_trace -> false

(* [is_execution_mode_trace ()] returns the execution mode is producing a trace *)
let is_execution_mode_trace () : bool =
  match !execution_mode with
  | Execution_mode_step_trace
  | Execution_mode_full_trace -> true
  | Execution_mode_step_diff
  | Execution_mode_exec -> false

(* [only_big_steps]: flag for the treatment of the exit line to ignore the small steps ('!!') and only
   consider big steps. TODO: used? *)
let only_big_steps : bool ref = ref false

(* [c_parser_name]: name of the C parser to use *)
let c_parser_name : string ref = ref "default"

(* Name of the root installation folder. It can also be the name of the source folder for not installed builds. *)
let optitrust_root : string ref = ref "."

(* Name of the currently executed transformation script.
   By default it is Sys.argv.(0) but it can be different in case of dynamic loading. *)
let program_name : string ref = ref ""

(* List of options *)

(* [cmdline_args]: a list of possible command line arguments. *)
type cmdline_args = (string * Arg.spec * string) list

(* LATER: Register options in the module where they are used *)
(* [spec]: possible command line arguments. *)
let spec : cmdline_args =
   [ ("-verbose", Arg.Set verbose, " activates debug printing");
     ("-mode", Arg.String process_mode, " mode is one of 'full-trace', 'step-trace' or 'step-diff', or 'exec' (default)");
     ("-line", Arg.Set_int target_line, " specify one line of interest for viewing a diff or a trace");
     ("-report-big-steps", Arg.Set report_big_steps, " report on the progress of the execution at each big step");
     ("-only-big-steps", Arg.Set only_big_steps, " consider only '!!!' for computing exit lines"); (* LATER: rename to: -exit-only-at-big-steps *)
     ("-debug-reparse", Arg.Set debug_reparse, " print on stdout the line number at which each reparse is performed");
     ("-reparse-at-big-steps", Arg.Set reparse_at_big_steps, " force reparsing at every big step (implies -debug-reparse)");
     ("-dump-small-steps", Arg.String set_dump_small_steps, " produce a distinct file for each small step");
     ("-dump-big-steps", Arg.String set_dump_big_steps, " produce a distinct file for each big step");
     ("-dump-ast-details", Arg.Set dump_ast_details, " produce a .ast and a _enc.cpp file with details of the ast");
     ("-dump-clang-ast", Arg.Unit set_dump_clang_ast, " produce clang_ast.ml with the AST obtained by ClangML");
     ("-analyse-stats", Arg.Set analyse_stats, " produce a file reporting on the execution time");
     ("-analyse-stats-details", Arg.Set analyse_stats_details, " produce more details in the file reporting on the execution time (implies -analyse_stats)");
     ("-print-optitrust-syntax", Arg.Set print_optitrust_syntax, " print output without conversion to C, i.e. print the internal AST, using near-C syntax");
     ("-serialized-input", Arg.String process_serialized_input, " choose an input serialization mode between 'build', 'use', 'make' or 'auto'");
     ("-disable-light-diff", Arg.Clear use_light_diff, " disable light diff");
     ("-disable-clang-format", Arg.Clear use_clang_format, " disable beautification using clang-format");
     ("-cparser", Arg.Set_string c_parser_name, "specify a C parser among 'default', 'clang', 'menhir', and 'all' ");
     ("-v", Arg.Set verbose_mode, " enable verbose regarding files processed out produced (not fully implemented yet).");
     (* LATER: a -dev flag to activate a combination of dump *)
  ]

(* Remember the name of the currently executed script *)
let process_program_name () =
  if !program_name = "" then program_name := Sys.argv.(!Arg.current)

(* [fix_flags ()]: processes flags than implies other flags. *)
let fix_flags () =
  if !analyse_stats_details then analyse_stats := true;
  if !reparse_at_big_steps then debug_reparse := true

(* [process_cmdline_args ~args ()]: processes all the command line arguments used during the script execution.
   If args are given, add them to the list of possible flags.
   This function has no effect if it was already called before. *)
let process_cmdline_args ?(args : cmdline_args = []) () : unit =
  process_program_name();
  Arg.parse
    (Arg.align (spec @ args))
    (fun _ -> raise (Arg.Bad "Error: no argument expected"))
    ("usage: no argument expected, only options");
  fix_flags()

(* [documentation_ssave_file_atfirst_check]: flag used for a hack used by function [doc_script_cpp], for generating
    the output associated with the documentation of a unit test, before running the main contents of the file. *)
let documentation_save_file_at_first_check = ref ""

(* cf. Trm.trm_combinators_unsupported_case *)
let trm_combinators_warn_unsupported_case = ref true

(* TODO: keep per-file state somewhere cleaner *)
(* cf. Clang_to_astRawC.warn_array_subscript_not_supported *)
let warned_array_subscript_not_supported = ref Tools.String_set.empty

(* [reset_flags_to_default] is used by [batching.ml] to avoid propagation of effects
  TODO: complete implementation by going over all relevant flags,
  and using a record of values.
  TODO: alternative: save flags and restore them at the end of a batching run. *)

let reset_flags_to_default () : unit =
  dump_ast_details := false;
  bypass_cfeatures := false;
  print_optitrust_syntax := false;
  use_light_diff := false;
  pretty_matrix_notation := false;
  display_includes := false;
  resource_errors_as_warnings := false;
  always_name_resource_hyp := false;
  check_validity := false;
  reparse_between_steps := false;
  recompute_resources_between_steps := false;
  trm_combinators_warn_unsupported_case := true;
  warned_array_subscript_not_supported := Tools.String_set.empty

let with_flag (flag: 'a ref) (value: 'a) (func: unit -> 'b): 'b =
  let init_value = !flag in
  flag := value;
  let x = func () in
  flag := init_value;
  x

(* *************************************************************************************************************
  Note: to see a diff at the level of the OptiTrust AST, use:
    -dump-ast-details
  and the shortcut "ctrl+shift+f6" for opening the diff between [*_before_enc.cpp] and [*_after_enc.cpp]
***************************************************************************************************************)
