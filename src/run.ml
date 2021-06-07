open Ast
open Tools
open Output
open Trace

let write_log (log : string) : unit =
  List.iter (fun (ctx, _) -> write_log ctx.clog log) (get_trace())

(* instruction added to interrupt the script early *)
let exit_script () : unit =
  print_info None "Exiting script\n";
  try
    close_logs ();
    List.iter
      (fun (ctx, astStack) ->
        let prefix = ctx.directory ^ ctx.prefix in
        let astAfter = Stack.pop astStack in
        let astBefore = Stack.pop astStack in
        print_info None "Writing ast and code before last transformation...\n";
        output_prog ctx (prefix ^ "_before") astBefore;
        print_info None "Done. Output files: %s_before.ast and %s_before%s.\n"
          prefix prefix ctx.extension;
        print_info None "Writing ast and code into %s.js " prefix;
        output_js (-1) prefix astAfter;
        print_info None "Writing ast and code after last transformation...\n";
        output_prog ctx (prefix ^ "_after") astAfter;
        print_info None "Done. Output files: %s_after.ast and %s_after%s.\n"
          prefix prefix ctx.extension;

        (* DEPRECATED --but keep for potential future use
        let file_before = prefix ^  "_before" ^ ctx.extension in
        let file_after = prefix ^  "_after" ^ ctx.extension in
        let file_diff =  prefix ^  "_diff.base64" in
        let _ = Sys.command ("meld " ^ file_before " " ^ file_after) in
        ...
        let _ = Sys.command ("git diff --no-index -U10 " ^ file_before " " ^ file_after ^ " | base64 -w 0 > " ^ file_diff) in
        *)
        ()
      )
      (get_trace());
    exit 0
  with
  | Stack.Empty -> fail None ("exit_script: script must be interrupted after " ^
                                "the initial source file is set.")


let dump_trace_to_js ?(out_prefix : string = "") () : unit =
  (* Initialize var content and source as empty arrays *)
  (* let () = initialization out_prefix in *)
  let dump_stack (out_prefix : string)
    (astStack : trm Stack.t) : unit =
    let nbAst = Stack.length astStack in
    let i = ref(nbAst - 2) in
    (* exceptions:
     - i = 0 -> program before tranformation -> prefix_input
     - i = nbAst -2 -> result program -> prefix_input
     - i = -1 -> empty program -> no output
    *)
    Stack.iter
      (fun ast ->
        if !i = -1 then ()
        else
          output_js !i out_prefix ast;
        i := !i - 1;
      )
      astStack
    in
    List.iter
      (fun (ctx, astStack) ->
        let out_prefix =
          if out_prefix = "" then ctx.directory ^ ctx.prefix else out_prefix
        in
        dump_stack out_prefix astStack

      )
      (get_trace())

(*
  outputs code at each step using given prefix for filename
  out_prefix_in.cpp is the program before transformation
  out_prefix_out.cpp is the program after transformation
*)
(* ----------------DEPRECATED------------------- *)
(* let dump_trace ?(out_prefix : string = "") () : unit =
  let dump_stack (ctx : context) (out_prefix : string)
    (astStack : trm Stack.t) : unit =
    let nbAst = Stack.length astStack in
    let i = ref (nbAst - 2) in
    (* exceptions:
     - i = 0 -> program before transformation -> prefix_input
     - i = nbAst -2 -> result program -> prefix_output
     - i = -1 -> empty program -> no output *)
    Stack.iter
      (fun ast ->
        if !i = 0 then
          output_prog ctx (out_prefix ^ "_in") ast
        else if !i = nbAst - 2 then
          output_prog ctx (out_prefix ^ "_out") ast
        else if !i = -1 then
          ()
        else
          output_prog ctx (out_prefix ^ "_" ^ string_of_int !i) ast;
        i := !i - 1
      )
      astStack
  in
  List.iter
    (fun (ctx, astStack) ->
      let out_prefix =
        if out_prefix = "" then ctx.directory ^ ctx.prefix else out_prefix
      in
      dump_stack ctx out_prefix astStack
    )
    (get_trace()) *)
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

(* outputs current code in given file *)
let dump ?(out_prefix : string = "") () : unit =
  (* DEPRECATED -- it uses function dump_trace *)
  (* if !Flags.full_dump then dump_trace ~out_prefix () *)
  if !Flags.full_dump then dump_trace_to_js ~out_prefix()
  else
    List.iter
      (fun (ctx, astStack) ->
        let out_prefix =
          if out_prefix = "" then ctx.directory ^ ctx.prefix else out_prefix
        in
        output_prog ctx (out_prefix ^ "_out") (Stack.top astStack)
      )
      (get_trace())

(*
  mandatory first instruction of a transformation script
  set environment for script execution on given program file
  expect a clean context
*)
let set_init_source (filename : string) : unit =
  match (get_trace()) with
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
     set_trace {extension; directory; prefix; includes; clog} astStack;
     (* trace := [({extension; directory; prefix; includes; clog}, astStack)]; *)
     print_info None "Starting script execution...\n"
  | _ -> failwith "set_init_source: context not clean"

(* TODO: comment *)
let reset () =
  Trace.reset()

(* Wrapper function for unit tests, assuming "foo.ml" to be a script
   operating on "foo.cpp" and dumping the result in "foo_out.cpp".
   The option ast_decode can be used for tests that want to report on
   the "undecoded AST", by copying "foo_out_enc.cpp" onto "foo_out.cpp" *)

let run_unit_test ?(out_prefix : string = "") ?(ast_decode : bool = true) (script : unit -> unit) : unit =
  let basename = Filename.chop_extension Sys.argv.(0) in
  let basename = (* remove "_with_exit" suffix if it ends the basename *)
    let suffix = "_with_exit" in
    let nsuffix = String.length suffix in
    let nbasename = String.length basename in
    if nbasename >= nsuffix && (String.sub basename (nbasename - nsuffix) nsuffix) = suffix
      then String.sub basename 0 (nbasename - nsuffix)
      else basename
      in
  run (fun () ->
    set_init_source (basename ^ ".cpp");
    script();
    flush stdout;
    dump ~out_prefix ();
    if not ast_decode
      then ignore (Sys.command (Printf.sprintf "cp %s_out_enc.cpp %s_out.cpp" basename basename))
  )

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
            let old_trace = get_trace() in
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
                  set_trace {ctx with prefix; clog} (Stack.copy astStack);
                  (* trace := [({ctx with prefix; clog}, Stack.copy astStack)]; *)
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


(******************************************************************************)
(*                        Smart constructors for targets                        *)
(******************************************************************************)

open Target
include Path_constructors

type constr = Target.constr
type target = Target.target
type case_dir = Target.case_dir
type abort_kind = Target.abort_kind
type constr_access = Target.constr_access
type case_kind = Target.case_kind
type enum_const_dir = Target.enum_const_dir
type target_list_pred = Target.target_list_pred
let make_target_list_pred = Target.make_target_list_pred

(******************************************************************************)
(*                              Generic                               *)
(******************************************************************************)

(*Show path using a decorators on both sides of the path
  for example :
  /*@1<*/ x++; /*>1@*/
  This comments can also be nested:
  These comments can be nested, eg if you target for loops
     /*@1<*/ for (int i=0;i<N;i++) {
      /*@2<*/ for (int i=0;i<N;i++) {
        x++;
      } /*>2@*/
    } /*>1@*/ *)
(* delete the label *)
let show_target ?(debug_ast:bool=false) ?(replace_top : bool = false)?(keep_previous : bool = false) (tr : target) : unit =
    apply_to_top ~replace_top (fun _ t ->
    let t =
      if not keep_previous
        then Generic.delete_target_decorators t
        else t
      in
    Generic.show_target ~debug_ast tr t
    )


let show_ast ?(replace_top:bool=false) ?(file:string="_ast.txt") ?(to_stdout:bool=true) (tr : target) : unit =
  apply_to_top ~replace_top(fun _ -> Generic.show_ast ~file ~to_stdout tr)


let clean_target_decorators () : unit =
    apply_to_top ~replace_top:false (fun _ -> Generic.delete_target_decorators)

(*
  split the sequence(s) around the instruction(s) pointed by tr
  property: the result sequence is of the form
    result_label:
    {var (split_name x0) decl
    …
    var (split_name xn) decl
    block1_label: {block 1; split_name x0 = x0; …; split_name xn = xn}
    block2 label:
    {var x0 decl = split_name x0; …; var xn decl = split_name xn; block 2}}
  where x0, …, xn are the vars declared in block1 that are used in block 2
  labels are defined from the labels argument:
    - labels = [] -> (result, result_block1, result_block2)
    - labels = [s] -> (s, s_block1, s_block2)
    - labels = [l1, l2, l3] -> (l1, l2, l3)
 *)
 (* TODO: Remove this later *)
(* let split_sequence ?(replace_top : bool = false) ?(keep_labels : bool = false)
  ?(labels : string list = [])
  ?(split_name : string -> string = fun x -> x ^ "_split")
  (tr : target) : unit =
  let (result_label, block1_label, block2_label) =
    match labels with
    | [] -> ("result", "result_block1", "result_block2")
    | [s] -> (s, s ^ "_block1", s ^ "_block2")
    | [result_label; block1_label; block2_label] ->
       (result_label, block1_label, block2_label)
    | _ -> fail None "split_sequence: bad labels"
  in
  let log : string =
    let ps = target_to_string tr in
    Printf.sprintf
      ("Split_sequence %s:\n" ^^
       "  - the split_name function outputs fresh names\n"
      )
      ps
  in
  write_log log;
  apply_to_top ~replace_top
    (fun ctx t ->
      let t =
        Sequence.split_sequence ctx.clog result_label block1_label
          block2_label split_name tr t
      in
      if keep_labels then t else
        Label.delete_labels [result_label; block1_label; block2_label]
          t
    );
  write_log "\n" *)

(*
  extract a variable from a loop:
  - before:
    optional_label:
    for i = 0 to N / for i = N to 0
      var x
      body(x)
  - after:
    result_label:
    {
      var x[N+1]
      optional_label:
      for i = 0 to N / for i = N to 0
        body(x[i])
    }
  assumptions:
    + no initialisation for x in its declaration
    + x is heap allocated and deleted last in the loop
    + the path points to the loop
 *)
let extract_loop_var ?(replace_top : bool = false) ?(keep_label : bool = false)
  ?(label : string = "") (tr : target) : unit =
  let result_label = if label = "" then "result" else label in
  let log : string =
    Printf.sprintf "Extract_loop_var %s:\n" (target_to_string tr)
  in
  write_log log;
  apply_to_top ~replace_top
    (fun ctx t ->
      let t = Loop.extract_loop_var ctx.clog result_label tr t in
      if keep_label then t else Label.delete_label result_label t
    );
  write_log "\n"

let extract_loop_vars ?(replace_top : bool = false) ?(keep_label : bool = false)
  ?(label : string = "") (tr : target) : unit =
  let result_label = if label = "" then "result" else label in
  let log : string =
    Printf.sprintf "Extract_loop_vars %s:\n" (target_to_string tr)
  in
  write_log log;
  apply_to_top ~replace_top
    (fun ctx t ->
      let t = Loop.extract_loop_vars ctx.clog result_label tr t in
      if keep_label then t else Label.delete_label result_label t
    );
  write_log "\n"

(*
  split the for loop pointed by tr
  assumption: the loop is of the form
  optional_label:
  for i = 0 to N / for i = N to 0
    {block1}
    {block2}
  where block1 and block2 are independent
  result:
    result_label:{
      loop1_label:
      for i = 0 to N / for i = N to 0
        block1
      loop2_label:
      for i = 0 to N / for i = N to 0
        block2
    }
  labels are defined from the labels argument:
    - labels = [] -> (result, result_loop1, result_loop2)
    - labels = [s] -> (s, s_loop1, s_loop2)
    - labels = [l1, l2, l3] -> (l1, l2, l3)
 *)
(* let split_loop_nodep ?(replace_top : bool = false) ?(keep_labels : bool = false)
  ?(labels : string list = []) (tr : target) : unit =
  let (result_label, loop1_label, loop2_label) =
    match labels with
    | [] -> ("result", "result_loop1", "result_loop2")
    | [s] -> (s, s ^ "_loop1", s ^ "_loop2")
    | [result_label; loop1_label; loop2_label] ->
       (result_label, loop1_label, loop2_label)
    | _ -> fail None "split_loop_nodep: bad labels"
  in
  let log : string =
    Printf.sprintf "Split_loop_nodep %s:\n" (target_to_string tr)
  in
  write_log log;
  apply_to_top ~replace_top
    (fun ctx t ->
      let t =
        Loop.split_loop_nodep ctx.clog result_label loop1_label
          loop2_label tr t
      in
      if keep_labels then t else
        Label.delete_labels [result_label; loop1_label; loop2_label] t
    );
  write_log "\n" *)

(* TODO: When implemented in combi, remove it *)
(*
  combine split_sequence, extract_loop_var and split_loop_nodep to split the for
  loop after the instruction pointed by tr in t
 *)
(* let split_loop ?(replace_top : bool = false) ?(keep_labels : bool = false)
  ?(labels : string list = [])
  ?(split_name : string -> string = fun x -> x ^ "_split")
  (tr : target) : unit =
  let (result_label, loop1_label, loop2_label) =
    match labels with
    | [] -> ("result", "result_loop1", "result_loop2")
    | [s] -> (s, s ^ "_loop1", s ^ "_loop2")
    | [result_label; loop1_label; loop2_label] ->
       (result_label, loop1_label, loop2_label)
    | _ -> fail None "split_loop: bad labels"
  in
  let (log_begin, log_end) : string * string =
    let ps = target_to_string tr in
    (Printf.sprintf "##### Start of split_loop %s #####\n\n" ps,
     Printf.sprintf "##### End of split_loop %s #####\n\n" ps)
  in
  write_log log_begin;
  (* split the loop body *)
  split_sequence ~replace_top
    ~keep_labels:true
    ~labels:["split_loop_tmp_result"; "split_loop_tmp_block1";
             "split_loop_tmp_block2"]
    ~split_name
    tr;
  (* make sure at most one ast is added to the stack *)
  let replace_top = true in
  (* label the loop for later calls to Generic *)
  Label.add "split_loop_tmp_loop"
    [cFor ~body:[cLabel "split_loop_tmp_result"
                   ~substr:true ] ""];
  (* remove unnecessary labels *)
  delete_labels ~replace_top ["split_loop_tmp_result"; "split_loop_tmp_block1";
                              "split_loop_tmp_block2"];
  let tr' = [cLabel "split_loop_tmp_loop" ~substr:true] in
  (* extract loop variables *)
  extract_loop_vars ~replace_top ~keep_label:true ~label:result_label tr';
  (* split the loop *)
  split_loop_nodep ~replace_top ~keep_labels:true
    ~labels:["split_loop_tmp_result"; loop1_label; loop2_label] tr';
  (* clean up *)
  delete_labels ~replace_top ["split_loop_tmp_result"; "split_loop_tmp_loop"];
  if not keep_labels then
    delete_labels ~replace_top [result_label; loop1_label; loop2_label];
  write_log log_end *)


(* let move_loop_before ?(replace_top : bool = false) (tr : target) (loop_index : var) : unit =
    let log : string =
      Printf.sprintf "move_loop_before %s:\n" (target_to_string tr)
    in
    write_log log;
    apply_to_top ~replace_top
      (fun ctx -> Loop.move_loop_before ctx.clog tr loop_index);
    write_log "\n"

let move_loop_after ?(replace_top : bool = false) (tr : target) (loop_index : var) : unit =
    let log : string =
      Printf.sprintf "move_loop_after %s:\n" (target_to_string tr)
    in
    write_log log;
    apply_to_top ~replace_top
      (fun ctx -> Loop.move_loop_after ctx.clog tr loop_index);
    write_log "\n"

let move_loop ?(replace_top : bool = false) ?(move_before : string  = "") ?(move_after : string = "" ) (loop_index : string) : unit =
    apply_to_top ~replace_top
      (fun ctx -> Loop.move_loop ctx.clog  ~move_before ~move_after loop_index);
    write_log "\n" *)

(* let inline_record_access ?(replace_top : bool = false) ?(field : string = "") ?(var : string = "") () : unit =
  apply_to_top ~replace_top
    (fun ctx -> Inlining.inline_record_access ctx.clog  field var);
  write_log "\n" *)
  
(* let rewrite ?(replace_top : bool = false) ?(rule : string = "") ?(path : target = [ ]) : () : unit =
  apply_to_top ~replace_top
    (fun ctx -> Generic.rewrite ctx.clog rule path );
  write_log "\n"
*)

let eliminate_goto_next ?(replace_top : bool = false) (_ : unit) : unit =
  let log = "Eliminate_goto_next: no assumptions\n\n" in
  write_log log;
  apply_to_top ~replace_top (fun _ -> Generic_core.eliminate_goto_next)

let group_decl_init ?(replace_top : bool = false) (_ : unit) : unit =
  let log = "Group_decl_init: no assumptions\n\n" in
  write_log log;
  apply_to_top ~replace_top (fun _ -> Generic_core.group_decl_init)

(******************************************************************************)
(*                        Debug                                               *)
(******************************************************************************)

(* include Tools.Debug *)
include Tools (* TODO: make it better *)
