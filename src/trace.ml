open Ast
open Clang.Ast
open Clang_to_ast
open Tools
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
  List.iter (fun (ctx, _) -> Transformations.write_log ctx.clog log) !trace *)

(* restore the initial state *)
let reset () : unit =
  trace := [(init_ctx, Stack.create ())];
  close_logs ();
  logs := []

(* function that executes the script to deal with errors *)
let run (script : unit -> unit) : unit =
  Arg.parse
    Flags.spec
    (fun _ -> raise (Arg.Bad "Error: no argument expected"))
    ("usage: no argument expected, only options");
  let script : unit -> unit =
    (fun () ->
      try script (); close_logs () with
      | Failure s ->
         close_logs ();
         failwith s
    )
  in
  if !Flags.repeat_io then script ()
  else
    (*FANCY
   try script () with
    | _ ->
       (*
         if an error occurs, restart with printing/parsing at each step to have
         appropriate location computation and error messages
        *)
       print_info None "Error while executing the transformation script.\n";
       print_info None "Restarting with printing/parsing at each step...\n";
       Flags.repeat_io := true;
       reset ();
       script ()
     *)
    script()

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

(*
  mandatory first instruction of a transformation script
  set environment for script execution on given program file
  expect a clean context
*)
let set_init_source (filename : string) : unit =
  match !trace with
  | [(_, astStack)] when Stack.is_empty astStack ->
     Stack.push (trm_lit Lit_unit) astStack;
     let basename = Filename.basename filename in
     let extension = Filename.extension basename in
     let directory = (Filename.dirname filename) ^ "/" in
     let prefix = Filename.remove_extension basename in
     let clog = open_out (directory ^ prefix ^ ".log") in
     logs := clog :: !logs;
     let (includes, t) = parse filename in
     Stack.push t astStack;
     trace := [({extension; directory; prefix; includes; clog}, astStack)];
     print_info None "Starting script execution...\n"
  | _ -> failwith "set_init_source: context not clean"

(* Change the flag -reapeat-io (default is true)  *)
let set_repeat_io (b:bool) : unit =
  Flags.repeat_io := b

(*
  branching function
  optional argument to choose one branch (-1 to choose none)
 *)
let switch ?(only_branch : int = -1) (cases : (unit -> unit) list) : unit =
  (* close logs: new ones will be opened for each branch *)
  close_logs ();
  logs := [];
  let new_trace =
    foldi
      (fun i tr f ->
        if only_branch = -1 || i = only_branch then
          begin
            let old_trace = !trace in
            let new_trace =
              List.fold_right
                (fun (ctx, astStack) tr ->
                  let prefix = ctx.prefix ^ "_" ^ (string_of_int i) in
                  let clog = open_out (ctx.directory ^ prefix ^ ".log") in
                  (* store new log channel *)
                  logs := clog :: !logs;
                  (*
                    execute each branch in a single context and store the result
                   *)
                  trace := [({ctx with prefix; clog}, Stack.copy astStack)];
                  f ();
                  !trace :: tr;
                )
                old_trace
                []
            in
            trace := old_trace;
            (List.flatten new_trace) :: tr
          end
        else tr
      )
      []
      cases
  in
  trace := List.flatten (List.rev new_trace)




