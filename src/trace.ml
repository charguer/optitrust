open Ast
open Clang.Ast
open Clang_to_ast
(******************************************************************************)
(*                             Context management                             *)
(******************************************************************************)

type context =
  {extension : string; directory : string; prefix : string; includes : string;
   clog : out_channel}

let init_ctx : context =
  {extension = ".cpp"; directory = ""; prefix = ""; includes = "";
   clog = stdout}

(* list of context, AST stack *)
let trace : (context * (trm Stack.t)) list ref =
  ref [(init_ctx, Stack.create ())]

(* Return the current state of the trace *)
let get_trace () : (context * (trm Stack.t)) list = 
  !trace

(* Modify the current trace  *)
let set_trace (ctx : context) (astStack : trm Stack.t) : unit =
  trace := [ctx, astStack]

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

(* restore the initial state *)
let reset () : unit =
  trace := [(init_ctx, Stack.create ())];
  close_logs ();
  logs := []


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





