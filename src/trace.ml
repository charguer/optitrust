open Ast
(******************************************************************************)
(*                             Logging management                             *)
(******************************************************************************)

(* [logs] is a reference on the list of open log channels. *)
let logs : (out_channel list) ref = ref []

(* [close_logs] closes all open log channels. *)
let close_logs () : unit =
  List.iter (fun log -> close_out log) !logs;
  logs := []

(* [init_logs] initializes the log files. It closes any existing logs.
   Returns the one log created. *)
let init_logs directory prefix =
  close_logs();
  let clog = open_out (directory ^ prefix ^ ".log") in
  logs := clog :: [];
  clog

(* [write_log clog msg] writes the string [msg] to the channel [clog]. *)
let write_log (clog : out_channel) (msg : string) : unit =
  output_string clog msg;
  flush clog

(* [trm_to_log clog styp t] writes in the channel [clog] the term [t],
   and its typing information described by the string [styp]. *)
let trm_to_log (clog : out_channel) (exp_type : string) (t : trm) : unit =
  let sloc =
    match t.loc with
    | None -> ""
    | Some {loc_file = _; loc_start = {pos_line = start_row; pos_col = start_column}; loc_end = {pos_line = end_row; pos_col = end_column}} ->
       Printf.sprintf "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in
  let msg = Printf.sprintf (" -expression\n%s\n" ^^ " %s is a %s\n") (Ast_to_c.ast_to_string t) sloc exp_type in
 write_log clog msg

(******************************************************************************)
(*                             File input                                     *)
(******************************************************************************)

(* [get_cpp_includes filename] get list of file includes syntactically visible
   on the first lines of a CPP file -- this implementation is quite restrictive. *)
let get_cpp_includes (filename : string) : string =
  (* make sure the include list is clean *)
  let includes = ref "" in
  let c_in = open_in filename in
  try
    while (true) do
      let s = input_line c_in in
      if Str.string_match (Str.regexp "^#include") s 0 then
        includes := !includes ^ s ^ "\n\n";
    done;
    !includes
  with
  | End_of_file -> close_in c_in; !includes

(* [parse filename] returns a list of includes and an AST. *)
let parse (filename : string) : string * trm =
  print_info None "Parsing %s...\n" filename;
  let includes = get_cpp_includes filename in
  let command_line_include =
    List.map Clang.Command_line.include_directory
      (Clang.default_include_directories ()) in
  let command_line_warnings = ["-Wno-parentheses-equality"; "-Wno-c++11-extensions"] in
  let command_line_args = command_line_warnings @ command_line_include in
  let ast = Clang.Ast.parse_file ~command_line_args filename  in

  (* DEBUG: Format.eprintf "%a@."
       (Clang.Ast.format_diagnostics Clang.not_ignored_diagnostics) ast; *)
  print_info None "Parsing Done.\n";
  print_info None "Translating AST...\n";

  let t = Clang_to_ast.translate_ast ast in

  print_info None "Translation done.\n";
  (includes, t)


(******************************************************************************)
(*                             Trace management                               *)
(******************************************************************************)

(* A context contains general information about:
   - the source code that was loaded initially using [set_init_file],
   - the prefix of the filenames in which to output the final result using [dump]
   - the log file to report on the transformation performed. *)
type context =
  { extension : string;
    directory : string;
    prefix : string;
    includes : string;
    clog : out_channel; }

let context_dummy : context =
  { extension = ".cpp";
    directory = "";
    prefix = "";
    includes = "";
    clog = stdout; }

(* A trace is made of a context, a current AST, and a list of ASTs that were
   saved as "interesting intermediate steps", via the [Trace.save] function.
   Any call to the [step] function adds a copy of [cur_ast] into [history]. *)
type trace = {
  mutable context : context;
  mutable cur_ast : trm;
  mutable history : trms; }

let trm_dummy : trm =
  trm_val (Val_lit Lit_unit)

let trace_dummy : trace =
  { context = context_dummy;
    cur_ast = trm_dummy; (* dummy *)
    history = []; }

(* [traces] denotes the internal state of Optitrust. It consists of a list of traces,
   because Optitrust supports a [switch] command that allows branching in the
   transformation script, thus producing several possible traces. *)
type traces = trace list

let traces : traces ref =
  ref [trace_dummy]


(* [is_traces_dummy()] returns whether the trace was never initialized. *)
let is_traces_dummy () : bool =
  match !traces with
  | [tr] -> (tr == trace_dummy)
  | _ -> false

(* [reset()] restores the global state (object [traces]) in its uninitialized state,
   like at the start of the program. This operation is automatically called by [Trace.init]. *)
let reset () : unit =
  close_logs();
  traces := [trace_dummy]

(* Storage for the current ml script *)
let ml_file = ref []

(* Storage for the current time *)
let last_time  = ref (Unix.gettimeofday ())

(* [init f] initialize the trace with the contents of the file [f].
   This operation should be the first in a transformation script.
   The history is initialized with the initial AST.
   [~prefix:"foo"] allows to use a custom prefix for all output files,
   instead of the basename of [f]. *)
(* LATER for mli: val set_init_source : string -> unit *)
let init ?(prefix : string = "") (filename : string) : unit =
  reset ();
  
  let basename = Filename.basename filename in
  let extension = Filename.extension basename in
  let directory = (Filename.dirname filename) ^ "/" in
  let default_prefix = Filename.remove_extension basename in
  let ml_file_name = if Tools.pattern_matches "_inlined" default_prefix then 
  List.nth (Str.split (Str.regexp "_inlined") default_prefix) 0 else default_prefix in
  ml_file := if !Flags.analyse_time then
              Xfile.get_lines (ml_file_name ^ ".ml")
              else [];
  let prefix = if prefix = "" then default_prefix else prefix in
  let clog = init_logs directory prefix in
  let (includes, cur_ast) = parse filename in
  let context = { extension; directory; prefix; includes; clog } in
  let trace = { context; cur_ast; history = [cur_ast] } in
  traces := [trace];
  print_info None "Starting script execution...\n"
(* [finalize()] should be called at the end of the script, to properly close the log files
    created by the call to [init]. *)
let finalize () : unit =
  close_logs()

(* [alternative f] executes the script [f] in the original state that
   was available just after the call to [init].
   After the call, all the actions performed are discarded.

  Current usage:
     !! Trace.alternative (fun () ->
        !! Loop.fusion_on_block [cLabel "tofusion"];
        !!());

   TODO: figure out if it is possible to avoid "!!" in front and tail of [Trace.restart].
   TODO: figure out if this implementation could be extended in the presence of [switch]. *)
let alternative f : unit =
  let saved_traces = !traces in
  let trace = match !traces with
    | [] -> fail None "alternative: the trace is empty"
    | [trace] -> trace
    | _ -> fail None "alternative: incompatible with the use of switch"
    in
  let init_ast =
    match List.rev trace.history with
    | [] -> fail None "alternative: the history is empty"
    | t::_ -> t
    in
  let init_trace = { trace with cur_ast = init_ast; history = [init_ast] } in
  traces := [init_trace];
  f();
  traces := saved_traces

(* [switch cases] allows to introduce a branching point in a script.
   The [cases] argument gives a list of possible continuations (branches).
   Each of the branches can terminate with a [dump] operation to produce
   its output in a specific file. Alternatively, there could be further
   tranformations after the [switch] construct---a diamond construct.
   In such case, the instructions that follow the [switch] are applied
   in parallel on each of the traces, where one trace corresponds to one
   possible path in the script (via the branches).
   The optional argument [only_branch] can be use to temporary disable
   all branches but one. This is currently needed for the interactive mode
   to work. Branches are numbered from 1 (not from zero). *)
(* LATER for mli: switch : ?only_branch:int -> (unit -> unit) list -> unit *)
let switch ?(only_branch : int = 0) (cases : (unit -> unit) list) : unit =
  (* Close logs: new logs will be opened in every branch. *)
  close_logs ();
  let list_of_traces =
    Tools.foldi
      (fun i tr f ->
        let branch_id = i + 1 in
        if only_branch = 0 || branch_id = only_branch then
          begin
            let old_traces = !traces in
            let new_traces =
              List.fold_right
                (fun trace acc_traces ->
                  let context = trace.context in
                  (* create an extended prefix for this branch, unless there is a single branch *)
                  let prefix =
                    if List.length cases <= 1 || only_branch <> 0
                      then context.prefix
                      else context.prefix ^ "_" ^ (string_of_int branch_id)
                    in
                  (* create and register new log channel *)
                  let clog = open_out (context.directory ^ prefix ^ ".log") in
                  logs := clog :: !logs;
                  (* execute each branch in a single context *)
                  let branch_trace = { trace with context = { context with prefix; clog } } in
                  traces := [branch_trace];
                  f ();
                  (* store the traces produced by this branch *)
                  (!traces) :: acc_traces;
                )
                old_traces
                []
            in
            traces := old_traces;
            (List.flatten new_traces) :: tr
          end
        else tr
      )
      []
      cases
  in
  traces := List.flatten (List.rev list_of_traces)

(* For dynamic checks: keep track of the number of nested calls to [Trace.call] *)
let call_depth = ref 0

(* [apply f] applies the transformation [f] to the current AST,
   and updates the current ast with the result of that transformation.
   If there are several active trace (e.g., after a [switch]),
   then [f] is applied to each of the traces. During the execution of [f]
   on a given trace, the set of traces is replaced with a singleton set
   made of only that trace; this allows for safe re-entrant calls
   (i.e., the function [f] itself may call [Trace.apply]. *)
let apply (f : trm -> trm) : unit =
  if is_traces_dummy()
    then fail None "Trace.init must be called prior to any transformation.";
  let cur_traces = !traces in
  incr call_depth;
  List.iter (fun trace ->
    traces := [trace]; (* temporary view on a single trace *)
    trace.cur_ast <- f trace.cur_ast)
    cur_traces;
  traces := cur_traces; (* restoring the original view on all traces *)
  decr call_depth

(* [call f] is similar to [apply] except that it applies to a function [f]
   with unit return type: [f] is meant to update the [cur_ast] by itself
   through calls to [apply].
   If there are several active trace (e.g., after a [switch]),
   then [f] is applied to each of the traces. During the execution of [f]
   on a given trace, the set of traces is replaced with a singleton set
   made of only that trace; this allows for safe re-entrant calls
   (i.e., the function [f] itself may call [Trace.apply]. *)
let call (f : trm -> unit) : unit =
  if is_traces_dummy()
    then fail None "Trace.init must be called prior to any transformation.";
  incr call_depth;
  let cur_traces = !traces in
  List.iter (fun trace ->
    traces := [trace]; (* temporary view on a single trace *)
    f trace.cur_ast)
    cur_traces;
  traces := cur_traces; (* restoring the original view on all traces *)
  decr call_depth

(* [step()] takes the current AST and adds it to the history.
   If there are several traces, it does so in every branch. *)
let step () : unit =
  List.iter (fun trace ->
    trace.history <- trace.cur_ast::trace.history)
    !traces


(******************************************************************************)
(*                                   Output                                   *)
(******************************************************************************)

(* [cleanup_cpp_file_using_clang_format filename] makes a system call to
   reformat a CPP file using the clang format tool.
   LATER: find a way to remove extra parentheses in ast_to_doc, by using
   priorities to determine when parentheses are required. *)
let cleanup_cpp_file_using_clang_format (filename : string) : unit =
  ignore (Sys.command ("clang-format -i " ^ filename))

(* [output_prog ctx prefix ast] writes the program described by the term [ast]
   in several files:
   - one describing the raw AST ("prefix.ast")
   - one describing the internal AST ("prefix_enc.cpp")
   - one describing the CPP code ("prefix.cpp").
   The CPP code is automatically formatted using clang-format. *)


type language = | Language_cpp | Language_rust | Language_ocaml

let language_of_extension (extension:string) : language =
  match extension with
  | ".cpp" -> Language_cpp
  | ".rs" -> Language_rust
  | ".ml" -> Language_ocaml
  | _ -> fail None ("unknown extension " ^ extension)

let get_language () =
  match !traces with
  | [] -> fail None "cannot detect language -- trace should not be empty"
  | t::_ -> language_of_extension t.context.extension

let output_prog ?(ast_and_enc:bool=true) (ctx : context) (prefix : string) (ast : trm) : unit =
  let file_prog = prefix ^ ctx.extension in
  let out_prog = open_out file_prog in
  begin try
    (* print C++ code with decoding *)
    (*   DEPRECATED
    Printf.printf "===> %s \n" (ctx.includes); print_newline();*)
    output_string out_prog ctx.includes;
    Ast_to_c.ast_to_doc out_prog ast;
    close_out out_prog;
  with | Failure s ->
    close_out out_prog;
    failwith s
  end;
  (* beautify the C++ code --comment out for debug *)
  cleanup_cpp_file_using_clang_format file_prog;
  (* ast and enc *)
  if ast_and_enc then begin
    let file_ast = prefix ^ ".ast" in
    let file_enc = prefix ^ "_enc" ^ ctx.extension in
    let out_ast = open_out file_ast in
    let out_enc = open_out file_enc in
    begin try
    (* print the raw ast *)
      Ast_to_text.print_ast out_ast ast;
      output_string out_ast "\n";
      output_string out_enc ctx.includes;
      Ast_to_c.ast_to_undecoded_doc out_enc ast;
      output_string out_enc "\n";
      close_out out_ast;
      close_out out_enc;
      cleanup_cpp_file_using_clang_format file_enc;
    with | Failure s ->
      close_out out_ast;
      close_out out_enc;
      failwith s
    end
  end

(* [output_prog_opt ctx prefix ast_opt] is similar to [output_prog], but it
   generates an empty file in case the [ast_opt] is [None]. *)
let output_prog_opt  ?(ast_and_enc:bool=true) (ctx : context) (prefix : string) (ast_opt : trm option) : unit =
  match ast_opt with
  | Some ast -> output_prog ~ast_and_enc ctx prefix ast
  | None ->
      let file_prog = prefix ^ ctx.extension in
      let out_prog = open_out file_prog in
      close_out out_prog

(* [output_js index cpp_filename prefix _ast] Produces a javascript file used for viewing the ast in interactive mode
   This javascript file contains an array of source codes and an array of ast's. Where the entry at index i contains the state
   of the source and ast after applying transformaion i.
*)
let output_js  ?(vars_declared : bool = false)(index : int) (prefix : string) (ast : trm) : unit =
  (* DEPRECATED let (_, ast) = parse cpp_filename in *)
  let file_js = prefix ^ ".js" in
  let out_js = open_out file_js in
  try
    (* Dump the description of the AST nodes *)
    let lang, extension = match get_language () with
      | Language_cpp -> "\'" ^ "text/x-c++src" ^ "\'", ".cpp"
      | Language_rust -> "\'" ^ "text/x-rustsrc" ^ "\'", ".rs"
      | Language_ocaml -> "\'" ^ "text/x-Ocaml" ^ "\'", ".ml" in
    if not vars_declared
      then begin
      output_string out_js (Tools.sprintf "var source = %s\n" "new Array();");
      output_string out_js (Tools.sprintf "var contents = %s\n" "new Array();");
      output_string out_js (Tools.sprintf "var language = %s\n" lang);
      end else ();
    let src = Xfile.get_contents (prefix ^ "_before" ^ extension) in
    Ast_to_js.Json.code_to_js out_js index src;
    output_string out_js "\n";
    Ast_to_js.ast_to_js out_js index ast;
    output_string out_js "\n";
    close_out out_js;
  with | Failure s ->
    close_out out_js;
    failwith s

(* [dump_trace_to_js] writes into one/several (?) files
   the contents of the current AST and of all the history,
   that is, of all the ASTs for which the [step] method was called.
*)
let dump_trace_to_js ?(prefix : string = "") () : unit =
  assert (prefix = prefix && false);
  let dump_history (prefix : string) (asts : trms) : unit =
    let nbAst = List.length asts in
    let i = ref (nbAst - 2) in
    List.iter
      (fun ast ->
        if !i = 0 then
          output_js 0 prefix ast
        else if !i = (nbAst - 2) then
          output_js ~vars_declared:true (nbAst - 2) prefix ast
        else if !i = (-1) then ()
        else
          begin
          output_js ~vars_declared:true !i prefix ast;
          i := !i - 1
          end
      )
      asts in
  List.iter
    (fun trace ->
      let ctx = trace.context in
      let prefix =
        if prefix = "" then ctx.directory ^ ctx.prefix else prefix
      in
      dump_history prefix (trace.cur_ast :: trace.history)
    )
    (!traces)

(*
  filename = prefix ^ "_trace.js"
  let f = open_out filename

  fprintf  f "var trace = {};";
  let print_step i ast =
     fprintf f "traces[%d] = {" i;
     code_to_js f i ast;
     fprintf f "};";
     in
  List.iteri print_step !traces



  var trace = {};
  trace[0] = {`
  trace[1] = {..};
  trace[2] = {..};
 *)

(******************************************************************************)
(*                                   Reparse                                  *)
(******************************************************************************)

(* [reparse_trm ctx ast] print [ast] in a temporary file and reparses it using Clang. *)
let reparse_trm (ctx : context) (ast : trm) : trm =
  let in_prefix = ctx.directory ^ "tmp_" ^ ctx.prefix in
  output_prog ctx in_prefix ast;
  let (_, t) = parse (in_prefix ^ ctx.extension) in
  (*let _ = Sys.command ("rm " ^ in_prefix ^ "*") in*)
  t

(* [reparse()] function takes the current AST, prints it to a file, and parses it
   as if it was a fresh input. Doing so ensures in particular that all the type
   information is properly set up. *)
let reparse () : unit =
 List.iter (fun trace ->
    trace.cur_ast <- reparse_trm trace.context trace.cur_ast)
    !traces

(* Work-around for a name clash *)
let reparse_alias = reparse


(******************************************************************************)
(*                                   Dump                                     *)
(******************************************************************************)

(* [dump_diff_and_exit()] invokes [output_prog] on the current AST an also on the
   last item from the history, then it interrupts the execution of the script.
   This function is useful for interactively studying the effect of one particular
   transformation from the script.
   If option [-dump-last nb] was provided, output files are produced for the last [nb] step. *)
(* LATER for mli: dump_diff_and_exit : unit -> unit *)
let dump_diff_and_exit () : unit =
  print_info None "Exiting script\n";
  close_logs ();
  let trace =
    match !traces with
    | [] -> fail None "No trace"
    | [tr] -> tr
    | trs -> Printf.eprintf "Warning: considering the last branch of all switches.\n";
             List.hd (List.rev trs)
    in
  let ctx = trace.context in
  let prefix = ctx.directory ^ ctx.prefix in
  (* Common printinf function *)
  let output_ast ?(ast_and_enc:bool=true) filename_prefix ast_opt =
    output_prog_opt ~ast_and_enc ctx filename_prefix ast_opt;
    print_info None "Generated: %s%s\n" filename_prefix ctx.extension;
    in
  (* CPP and AST output for BEFORE *)
  let astBefore =
    match trace.history with
    | t::_ -> Some t (* the most recently saved AST *)
    | [] -> Printf.eprintf "Warning: only one step in the history; consider previous step blank.\n"; None
    in
  output_ast (prefix ^ "_before") astBefore;
  (* CPP and AST for BEFORE_N *)
  if !Flags.dump_last <> Flags.dump_last_default then begin
    let nb_requested = !Flags.dump_last in
    let nb_available = List.length trace.history in
    (* if nb_requested < nb_available
       then Printf.eprintf "Warning: not enought many steps for [dump_last]; completing with blank files.\n"; *)
    for i = 0 to nb_requested-1 do
      let astBeforeI = if i < nb_available then Some (List.nth trace.history i) else None in
      output_ast ~ast_and_enc:false (prefix ^ "_before_" ^ string_of_int i) astBeforeI
    done;
  end;
  (* CPP and AST and Javscript for AFTER *)
  let astAfter = trace.cur_ast in
  output_ast (prefix ^ "_after") (Some astAfter);
  print_info None "Writing ast and code into %s.js " prefix;
  output_js 0 prefix astAfter;
  (* Exit *)
  exit 0



(* [check_time line]: compute the time it takes to execute the transformation at line 
  [line] in a ml script
*)
let check_time (line : int) : unit = 
  if !Flags.analyse_time then 
    let t = Unix.gettimeofday () in
    let dt = ((t -. !last_time)) in
    last_time := t;
    let txt = List.nth !ml_file (line -1) in
    Printf.printf "\n%d: %f\n %s\n" line dt txt; 
    else ()

(* [check_exit_and_step()] performs a call to [check_exit], to check whether
   the program execution should be interrupted based on the command line argument
   [-exit-line], then it performas a call to [step], to save the current AST
   in the history, allowing for a visualizing the diff if the next call to
   [check_exit_and_step] triggers a call to [dump_diff_and_exit].
   If the optional argument [~reparse:true] is passed to the function,
   then the [reparse] function is called, replacing the current AST with
   a freshly parsed and typechecked version of it. *)
let check_exit_and_step ?(line : int = -1) ?(reparse : bool = false) () : unit =
  if !Flags.analyse_time then check_time line;
  let should_exit =
    match Flags.get_exit_line() with
    | Some li -> (line > li)
    | _ -> false
    in
  if should_exit then begin
     dump_diff_and_exit();
  end else begin
    if reparse
      then reparse_alias();
    step();
 end


(* [!!] is a prefix notation for the operation [check_exit_and_step].
   By default, it performs only [step]. The preprocessor of the OCaml script file
   can add the [line] argument to the call to [check_exit_and_step], in order
   to allow for checking the exit line. Concretely, if the user has the cursor
   one line N when invoking the Optitrust "view_diff" command, then the tool
   will display the difference between the state of the AST at the first "!!"
   that occurs strictly after line N, and the state at the previous "!!",
   which could be on line N or before (or could correspond to the input AST
   loaded by [Trace.init] if there is no preceeding '!!'.).
   Use [!!();] for a step in front of another language construct, e.g., a let-binding. *)
let (!!) (x:'a) : 'a =
  check_exit_and_step ();
  x

(* [!!!] is similar to [!!] but forces a [reparse] prior to the [step] operation. *)
let (!!!) (x:'a) : 'a =
  check_exit_and_step ~reparse:true ();
  x

(* [dump ~prefix] invokes [output_prog] to write the contents of the current AST.
   If there are several traces (e.g., due to a [switch]), it writes one file for each.
   If the prefix is not provided, the input file basename is used as prefix,
   and in any case "_out" is appended to the prefix.

   If you use [dump] in your script, make sure to call [!! Trace.dump] with the
   prefix [!!] in order for the diff visualization to work well for the last
   command before the call to dump.

   WILL BE DEPRECATED: If the command line argument [-dump-trace] was provided, then the
   function writes all the ASTs from the history into javascript files. *)
(* LATER for mli: val dump : ?prefix:string -> unit -> unit *)

let dump ?(prefix : string = "") () : unit =
  (* Dump full trace if requested *)
  if !Flags.dump_all then
     dump_trace_to_js ~prefix (); (* dump_trace ~prefix () *)
  (* Dump final result, for every [switch] branch *)
  List.iter
    (fun trace ->
      let ctx = trace.context in
      let prefix =
        if prefix = "" then ctx.directory ^ ctx.prefix else prefix
      in
      output_prog ctx (prefix ^ "_out") (trace.cur_ast)
    )
    (!traces)

(* [only_interactive_step line f] invokes [f] only if the argument [line]
   matches the command line argument [-exit-line]. If so, it calls the
   [step] function to save the current AST, then calls [f] (for example
   to add decorators to the AST in the case of function [show]), then
   calls [dump_diff_and_exit] to visualize the effect of [f]. *)

let only_interactive_step (line : int) ?(reparse : bool = false) (f : unit -> unit) : unit =
  if (Flags.get_exit_line() = Some line) then begin
    if reparse
      then reparse_alias();
    step();
    f();
    dump_diff_and_exit()
  end
  else
    begin
    check_exit_and_step();
    f()
    end

(* TODO: Arthur make sure to document that reparse invalidates the marks *)

(* [ast] returns the current ast; must be done as part of a call to [Trace.call]. *)
let ast () : trm =
  if !call_depth = 0
    then failwith "[get_the_ast] can only be invoked inside a call to [Trace.call].";
   match !traces with
   | [tr] -> tr.cur_ast
   | [] -> assert false (* [!traces] can never be empty *)
   | _ -> failwith "[get_the_ast] can only be invoked inside a call to [Trace.call] and not after a switch."

(* only for implementing [iteri_on_transformed_targets]. don't use it otherwise *)
let set_ast (t:trm) : unit =
  assert (!call_depth > 0);
  match !traces with
  | [tr] -> tr.cur_ast <- t
  | _ -> assert false

(* get the current context *)
let get_context () : context = 
  match !traces with 
  | [tr] -> tr.context
  | _ -> fail None "get_context: couldn't get the current context"

(* LATER:  need to reparse to hide spurious parentheses *)
(* LATER: add a mechanism for automatic simplifications after every step *)

