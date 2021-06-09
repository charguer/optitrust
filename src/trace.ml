open Ast
open Ast_to_c
open Ast_to_text
open Ast_to_js
open Clang.Ast
open Clang_to_ast


(******************************************************************************)
(*                             Context management                             *)
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
   saved as "interesting intermediate steps", via the [Trace.save] function. *)
type trace = {
  mutable context : context;
  mutable cur_ast : trm;
  mutable history : trm list; }

let trm_dummy : trm =
  trm_val (Val_lit Lit_unit)

let trace_dummy : trace =
  { context = context_dummy;
    cur_ast = trm_dummy; (* dummy *)
    history = []; }

(* Optitrust actually manages list of traces, because it supports a [switch]
   command that allows branching in the transformation script, to produce
   several possible output files. *)
type traces = trace list

(* The internal state of Optitrust thus consists of a reference on a list
   of traces, of type [traces] *)
let traces : traces ref =
  ref [trace_dummy]

(* Return whether the current internal state has never been modified -- only exposed for [Run.ml] *)
let was_traces_modified () : bool =
  match !traces with
  | [tr] -> (tr != trace_dummy)
  | _ -> true

(* Return the current internal state -- only exposed for [Run.ml] *)
let get_traces () : traces =
  !traces

(* Sets the internal state to single trace -- only exposed for [Run.ml] *)
let set_trace (trace : trace) : unit =
  traces := [ trace ]

(*
  list of log channels
  not in trace because close_logs may be called at a point where it may be
  modified
 *)
let logs : (out_channel list) ref = ref []

let close_logs () : unit =
  List.iter (fun clog -> close_out clog) !logs

(* let write_log (log : string) : unit =
  List.iter (fun (ctx, _) -> Generic.write_log ctx.clog log) !trace *)

(* The [reset] operation restores the global state (object [traces]) in
   its original form. This operation is useful to perform several
   independent operations within a same file. *)
let reset () : unit =
  close_logs ();
  logs := [];
  traces := [trace_dummy]

(* get the sequence of includes at the beginning of the file *)
let get_includes (filename : string) : string =
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

(* parse program file and return includes and AST *)
let parse (filename : string) : string * trm =
  print_info None "Parsing %s...\n" filename;
  let includes = get_includes filename in
  let command_line_args =
    List.map Clang.Command_line.include_directory
      (Clang.default_include_directories ())
  in
  let ast = parse_file ~command_line_args filename in
  (* Format.eprintf "%a@."
   *   (Clang.Ast.format_diagnostics Clang.not_ignored_diagnostics) ast; *)
  print_info None "Parsing Done. Translating AST...\n";
  let t = translate_ast ast in
  print_info None "Translation done.\n";
  (includes, t)

(******************************************************************************)
(*                                   Output                                   *)
(******************************************************************************)

let failure_expected f =
  begin try f(); failwith "should have failed"
  with TransfoError _ -> () end

let write_log (clog : out_channel) (log : string) : unit =
  output_string clog log; flush clog

(* trm_to_log: Generate logging for a given ast node
      params:
        t: ast
      returns:
        unit
*)
(* TODO: Replace looggin everywhere with a simple call to this function *)
let trm_to_log (clog : out_channel) (exp_type : string) (t : trm) : unit =
  let log : string =
    let loc : string =
    match t.loc with
    | None -> ""
    | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in
    Printf.sprintf
    (" -expression\n%s\n" ^^
    " %s is a %s\n"
    )
    (ast_to_string t) loc exp_type
    in write_log clog log

(* clean up a C++ file using clang format *)
let cleanup_cpp_file_using_clang_format filename =
   ignore (Sys.command ("clang-format -i " ^ filename))

(* Dump a program in: raw ast format; in undecoded C++ format; in decoded C++ format;
   The C code are pretty-printed using the clang-format tool. *)
(* LATER: find a way to remove extra parentheses
   other possibility: use operator priorities in ast_to_doc to determine when
   to put parentheses *)

let output_prog (ctx : context) (out_prefix : string) (ast : trm) : unit =
  let file_ast = out_prefix ^ ".ast" in
  (* let file_js = out_prefix ^ ".js" in
  let out_js = open_out file_js in  *)
  let file_enc = out_prefix ^ "_enc" ^ ctx.extension in
  let file_prog = out_prefix ^ ctx.extension in
  let out_ast = open_out file_ast in
  let out_enc = open_out file_enc in
  let out_prog = open_out file_prog in
  let close_channels() =
    close_out out_ast;
    close_out out_enc;
    close_out out_prog;
    (* close_out out_js; *)
    in
  try
    (* print the raw ast *)
    (* Output the current ast into json format *)
    (* ast_to_js  *)
    print_ast (* ~only_desc:true *) out_ast ast;
    output_string out_ast "\n";
    (* Print ast and source code in jacascript format *)
    (* ast_to_js out_js (-1) ast;
    code_to_js out_js (-1) ast; *)
    (* print C++ code without decoding *)
    output_string out_enc ctx.includes;
    ast_to_undecoded_doc out_enc ast;
    output_string out_enc "\n";
    (* print C++ code with decoding *)
    output_string out_prog ctx.includes;
    ast_to_doc out_prog ast;
    (* output_string out_prog "\n"; *)
    (* ast_json_to_doc out_json ast; *)

    close_channels();
    (* beautify the C++ code *)
    cleanup_cpp_file_using_clang_format file_enc;
    cleanup_cpp_file_using_clang_format file_prog
  with
  | Failure s ->
      close_channels();
      failwith s


(* TODO: Move apply_to_top to a better place *)
  (* apply_to_top: add the given ast to the ast stack
*)
(* LATER: the context argument of f seems to never be used... *)
let apply_to_top (f : context -> trm -> trm) : unit =
  List.iter (fun trace ->
    trace.cur_ast <- f trace.context trace.cur_ast)
    !traces

(* The [save] function takes the current AST and adds it to the
   history (in every branch). *)
let save () : unit =
  List.iter (fun trace ->
    trace.history <- trace.cur_ast::trace.history)
    !traces

(* print ast in temporary file and parse it again *)
let reparse_trm (ctx : context) (ast : trm) : trm =
  let in_prefix = ctx.directory ^ "tmp_" ^ ctx.prefix in
  output_prog ctx in_prefix ast;
  let (_, t) = parse (in_prefix ^ ctx.extension) in
  (*let _ = Sys.command ("rm " ^ in_prefix ^ "*") in*)
  t

(* The [reparse] function takes the current AST (in every branch),
   prints it to a file, and parses it as if it was a fresh input.
   Doing so ensures in particular that all the type information is
   properly set up. *)
let reparse () : unit =
 List.iter (fun trace ->
    trace.cur_ast <- reparse_trm trace.context trace.cur_ast)
    !traces

(*
  outputs a javascript file which contains the ast encoded as json
  for each transformation step also the initial source code together
  with the transformed versions
 *)

let output_js (index : int) (out_prefix : string )(ast : trm) : unit =
  let file_js = out_prefix ^ ".js" in
  let out_js = open_out file_js in

  try
    ast_to_js out_js index ast;
    output_string out_js "\n";
    Json.code_to_js out_js index ast;
    close_out out_js;

  with
  | Failure s ->
    close_out out_js;
    failwith s