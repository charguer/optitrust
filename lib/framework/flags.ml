
(* TODO: perhaps split this file between runflags and syntaxflags *)
(* TODO: group options that are relevant for "reset in batching" into a record data structure;
   so that we can save its state, and restore it, in between tests;
   - options for interactivity
   - options for behavior *)

(** [verbose]: flag to activate the printing of debug information *)
let verbose : bool ref = ref false

(* Flag to hide stdout produced by tests when executed from ./tester *)
let hide_stdout : bool ref = ref false

(** [analyse_stats]: flag to meansure the time taken by each transformation. *)
let analyse_stats : bool ref = ref false

(** [analyse_stats_details]: flags to meansure the time taken by each step within a transformation
   (in particular, time to resolve targets, to set up marks, etc). *)
let analyse_stats_details : bool ref = ref false

(** [dump_ast_details]: flag to dump OptiTrust AST, both in the form of a '.ast' and '_enc.cpp' files. *)
let dump_ast_details : bool ref = ref false

(** [pretty_matrix_notation]: flag to display matrix macros with syntactic sugar:
  MALLOC2(n, m, sizeof(T)) --> malloc(sizeof(T[n][m]))
  x[MINDEX2(n, m, i, j)] --> x[i][j]
   *)
let pretty_matrix_notation : bool ref = ref false

(* whether to display includes AST or not. *)
let display_includes : bool ref = ref false

(** [report_all_warning]: flag to control display of "known" warnings.
   [true] by default, but [false] by default when using the tester on multiple tests. *)
let report_all_warnings : bool ref = ref true

(** [dump_big_steps]: call [Trace.dump_big_steps] in addition to [Trace.dump] at the end of the script.
   Files are generated in the subfolder [!dump_big_steps].  *)
let dump_big_steps : string option ref = ref None

(** [set_dump_big_steps foldername]: dump bigsteps in folder [foldername]. *)
let set_dump_big_steps (foldername : string) : unit =
  dump_big_steps := Some foldername

(* LATER: document *)
(** [dump_small_steps]:  *)
let dump_small_steps : string option ref = ref None
let set_dump_small_steps (foldername : string) : unit =
  dump_small_steps := Some foldername

(** [print_backtrace_on_error] *)
let print_backtrace_on_error : bool ref = ref true
 (*LATER: make available from command line*)

(** [debug_parsing_serialization]: flag to trace operations involving serialization/deserialization of parse trees. *)
let debug_parsing_serialization = ref false

(** [debug_reparse]: flag to print operations saving and reading error messages saved in the ast. *)
let debug_errors_msg_embedded_in_ast : bool ref = ref false

(** [debug_reparse]: flag to print the line numbers at which reparsing is triggered. *)
let debug_reparse : bool ref = ref false

(** [debug_stringreprs]: flag to print stringreprs debugging info *)
let debug_stringreprs : bool ref = ref false

(** [debug_var_id]: display ids on variables when outputing C code *)
let debug_var_id : bool ref = ref false

(** [reparse_at_big_steps]: flag to force reparsing of the entire file at each entry of a big step. *)
let reparse_at_big_steps : bool ref = ref false

(** [report_big_steps]: flag to report on the progress of big steps during a script execution. *)
let report_big_steps : bool ref = ref false

(** [use_clang_format]: flag to use clang-format or not in output CPP files. *)
let use_clang_format : bool ref = ref true

(** [keep_file_before_clang_format]: flag to save the file before cleaning up with clang format
   "foo_out.cpp" is saved as "foo_out_notfmt.cpp". Used by the tester for faster correctness checks. *)
let keep_file_before_clang_format : bool ref = ref false

(** [clang_format_nb_columns]: flag to control the limit on the column for clang-format. *)
let clang_format_nb_columns : int ref = ref 80

(** [code_print_width]: flag to choose the width of the printed code on stdout. *)
let code_print_width = ref 80

(** [use_light_diff]: flag to enable "light diffs", whereby we hide the function body of all the
   toplevel functions that are not affected by the transformation. *)
   (* TODO: could it be true by default? *)
let use_light_diff : bool ref = ref false

(** [bypass_cfeatures]: flag used for debugging the [cfeatures_elim/intro] functions, by bypassing them.
   It affects the behavior of the parsing function [c_parser] to bypass [cfeatures_elim].
   It affects the behavior of the printipng function [output_prog] to bypass [cfeatures_intro].
   Note: this option is orthogonal to [print_optitrust_syntax]; beware, however, that it makes
   no sense to print encoded terms without [print_optitrust_syntax] activated. *)
let bypass_cfeatures : bool ref = ref false

(** [print_optitrust_syntax]: flag used for printing the optitrust AST in near-C syntax, without applying the decoding *)
let print_optitrust_syntax = ref false

(** [print_only_code]: flag used for printing the optitrust AST without contracts and ghosts *)
let print_only_code = ref false

(** [stop_on_first_resource_error]: Stops on the first resource error found.
   This allows for the propagation of the backtrace. *)
let stop_on_first_resource_error = ref true

(** [resource_typing_enabled]: if false, never attempt typing resources and never introduce ghosts. *)
let resource_typing_enabled = ref true

(** [check_validity]: perform validation of transformations *)
let check_validity = ref false

(** [disable_resource_typing ()] should be called when using OptiTrust without resources. *)
let disable_resource_typing () =
  resource_typing_enabled := false;
  check_validity := false

(** [reparse_between_step]: always reparse between two steps *)
let reparse_between_steps = ref false

(** [recompute_resources_between_steps]: always recompute resources between two steps *)
let recompute_resources_between_steps = ref false

(** [ignore_serialized] disables the read of serialized AST saved after parsing *)
let ignore_serialized = ref false

(** [execution_mode] of the script *)

type execution_mode =
  | Execution_mode_step_diff (* produce a diff for a small-step, assumes [target_line] is provided *)
  | Execution_mode_step_trace (* produce a trace for a small-step, assumes [target_line] is provided *)
  | Execution_mode_full_trace (* produce a trace, and an output file, for the full script *)
  | Execution_mode_exec (* produce only the final output file obtained at the end of the script *)

let execution_mode : execution_mode ref = ref Execution_mode_exec

(* Option to serialize the ML trace object in addition to dumping its JS respresentation;
   Currently only set when the requested mode in [full-trace] *)
let serialize_trace : bool ref = ref false

let process_mode (mode : string) : unit =
  execution_mode :=
    begin match mode with
    | "step-diff" -> Execution_mode_step_diff
    | "step-trace" -> Execution_mode_step_trace
    | "full-trace" ->
      serialize_trace := true;
      Execution_mode_full_trace
    | "standalone-full-trace" -> Execution_mode_full_trace
    | "exec" -> Execution_mode_exec
    | _ -> failwith "Execution mode should be 'exec', or 'diff', or 'trace'"
    end

(* Options to report execution time information about script and trace generation *)
let report_exectime : bool ref = ref false

(* Options to generate a text version of the trace *)
let trace_as_text : bool ref = ref false

(* Options to control how much details are exported in the trace *)
type substeps_including_ast = SubstepsAST_all | SubstepsAST_all_important | SubstepsAST_small
(* LATER: none *)

let substeps_including_ast : substeps_including_ast ref = ref SubstepsAST_all

let process_substeps_including_ast (s:string) : unit =
  substeps_including_ast := match s with
  | "all" -> SubstepsAST_all
  | "all-important" -> SubstepsAST_all_important
  | "small" -> SubstepsAST_small
  | _ -> failwith "substeps_including_ast: invalid value"

(* Option to display full resource information in the trace *)
let detailed_resources_in_trace : bool ref = ref false

(** [target_line]: indicate which line to target in execution mode
   [Execution_mode_step_trace] or [Execution_mode_step_trace]. *)
let target_line : int ref = ref (-1)

(** [get_target_line ()]: gets the targeted line, cannot be [-1]. *)
let get_target_line () : int =
  if !target_line = (-1)
    then failwith "Flags.get_target_line: trying to read an invalid line number";
  !target_line

(** [is_execution_mode_step ()] returns the execution mode is targeting a specific step/line *)
let is_execution_mode_step () : bool =
  match !execution_mode with
  | Execution_mode_step_diff
  | Execution_mode_step_trace -> true
  | Execution_mode_exec
  | Execution_mode_full_trace -> false

(** [is_execution_mode_trace ()] returns the execution mode is producing a trace *)
let is_execution_mode_trace () : bool =
  match !execution_mode with
  | Execution_mode_step_trace
  | Execution_mode_full_trace -> true
  | Execution_mode_step_diff
  | Execution_mode_exec -> false

(** [only_big_steps]: flag for the treatment of the exit line to ignore the small steps ('!!') and only
   consider big steps. Triggered by the shortcut for viewing diffs for big-steps.
   Besides, this flag is automatically set is requesting the diff or trace for a specific
   step with the cursor on a line starting with the words "bigstep". *)
let only_big_steps : bool ref = ref false

(** [c_parser_name]: name of the C parser to use *)
let c_parser_name : string ref = ref "default"

(* Name of the root installation folder. It can also be the name of the source folder for not installed builds. *)
let optitrust_root : string ref = ref "."

(* Name of the currently executed transformation script.
   By default it is Sys.argv.(0) but it can be different in case of dynamic loading. *)
let program_name : string ref = ref ""


(* List of options *)

(** [cmdline_args]: a list of possible command line arguments. *)
type cmdline_args = (string * Arg.spec * string) list

(* LATER: Register options in the module where they are used *)
(** [spec]: possible command line arguments. *)
let spec : cmdline_args =
   [ ("-verbose", Arg.Set verbose, " activates debug printing");
     ("-mode", Arg.String process_mode, " mode is one of 'full-trace', 'standalone-full-trace', 'step-trace' or 'step-diff', or 'exec' (default)");
     ("-trace-as-text", Arg.Set trace_as_text, " additionnaly generate a plain text trace in 'foo_trace.txt' ");
     ("-substeps-including-ast", Arg.String process_substeps_including_ast, " control which AST are produced, among: 'all' (default), 'small' (only small steps), 'all-important' (all important) ");
     ("-line", Arg.Set_int target_line, " specify one line of interest for viewing a diff or a trace");
     ("-report-big-steps", Arg.Set report_big_steps, " report on the progress of the execution at each big step");
     ("-only-big-steps", Arg.Set only_big_steps, " consider only '!!!' for computing exit lines"); (* LATER: rename to: -exit-only-at-big-steps *)
     ("-debug-reparse", Arg.Set debug_reparse, " print on stdout the line number at which each reparse is performed");
     ("-reparse-at-big-steps", Arg.Set reparse_at_big_steps, " force reparsing at every big step (implies -debug-reparse)");
     ("-dump-small-steps", Arg.String set_dump_small_steps, " produce a distinct file for each small step");
     ("-dump-big-steps", Arg.String set_dump_big_steps, " produce a distinct file for each big step");
     ("-dump-ast-details", Arg.Set dump_ast_details, " produce a .ast and a _enc.cpp file with details of the ast");
     ("-analyse-stats", Arg.Set analyse_stats, " produce a file reporting on the execution time");
     ("-analyse-stats-details", Arg.Set analyse_stats_details, " produce more details in the file reporting on the execution time (implies -analyse_stats)");
     ("-print-optitrust-syntax", Arg.Set print_optitrust_syntax, " print output without conversion to C, i.e. print the internal AST, using near-C syntax");
     ("-print-only-code", Arg.Set print_only_code, " print output with only code, no contracts and ghosts");
     ("-ignore-serialized", Arg.Set ignore_serialized, " ignore the serialized AST, forces the reparse of source file");
     ("-use-light-diff", Arg.Set use_light_diff, " enable light diff");
     ("-disable-light-diff", Arg.Clear use_light_diff, " disable light diff");
     ("-use-clang-format", Arg.Set use_clang_format, " enable beautification using clang-format (currently ignored by ./tester)");
     ("-disable-clang-format", Arg.Clear use_clang_format, " disable beautification using clang-format");
     ("-clang-format-nb-columns", Arg.Set_int clang_format_nb_columns, " specify the number of columns for clang-format");
     ("-cparser", Arg.Set_string c_parser_name, "specify a C parser among 'default', 'clang', 'menhir', and 'all' ");
     (* LATER: a -dev flag to activate a combination of dump *)
  ]

(* Remember the name of the currently executed script *)
let process_program_name () =
  if !program_name = "" then program_name := Sys.argv.(!Arg.current)

(** [fix_flags ()]: processes flags than implies other flags. *)
let fix_flags () =
  if !analyse_stats_details then analyse_stats := true;
  if !reparse_at_big_steps then debug_reparse := true

(** [process_cmdline_args ~args ()]: processes all the command line arguments used during the script execution.
   If args are given, add them to the list of possible flags.
   This function has no effect if it was already called before. *)
let process_cmdline_args ?(args : cmdline_args = []) () : unit =
  process_program_name();
  Arg.parse
    (Arg.align (spec @ args))
    (fun _ -> raise (Arg.Bad "Error: no argument expected"))
    ("usage: no argument expected, only options");
  fix_flags()

(** [ignore_serialized_default] is used by [reset_flags_to_default()],
   which is used by batching for ther tester *)
let ignore_serialized_default = ref !ignore_serialized

(** [reset_flags_to_default] is used by [batching.ml] to avoid propagation of effects
  TODO: complete implementation by going over all relevant flags,
  and using a record of values.
  TODO: alternative: save flags and restore them at the end of a batching run.
  TODO: document the fact that it also reset certain counters, eg for printing identifiers *)

let reset_flags_to_default () : unit =
  ignore_serialized := !ignore_serialized_default;
  dump_ast_details := false;
  bypass_cfeatures := false;
  print_optitrust_syntax := false;
  use_light_diff := false;
  pretty_matrix_notation := false;
  display_includes := false;
  stop_on_first_resource_error := true;
  resource_typing_enabled := true;
  check_validity := false;
  reparse_between_steps := false;
  recompute_resources_between_steps := false;
  serialize_trace := false

let with_flag (flag: 'a ref) (value: 'a) (func: unit -> 'b): 'b =
  let init_value = !flag in
  flag := value;
  try
    let x = func () in
    flag := init_value;
    x
  with e ->
    flag := init_value;
    Printexc.(raise_with_backtrace e (get_raw_backtrace ()))

(** [verbose_info fmt]: prints information only if the verbose
   flag is enabled *)
let verbose_info msg =
  if !verbose then
    Tools.info msg
  else
    Printf.ifprintf () msg

(** [verbose_warn loc]: Print a warning at location [loc] only if the verbose flag is enabled *)
let verbose_warn (loc : Ast.location) msg =
  if !verbose then
    match loc with
    | None -> Tools.warn msg
    | Some _ ->
      Printf.ksprintf (Tools.warn "%s: %s" (Ast.loc_to_string loc)) msg
  else
    Printf.ifprintf () msg

(* *************************************************************************************************************
  Note: to see a diff at the level of the OptiTrust AST, use:
    -dump-ast-details
  and the shortcut "ctrl+shift+f6" for opening the diff between [*_before_enc.cpp] and [*_after_enc.cpp]
***************************************************************************************************************)
