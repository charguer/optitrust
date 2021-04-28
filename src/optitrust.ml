open Clang.Ast
open Ast
open Ast_to_c
open Ast_to_text
open Ast_of_clang
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

(*
  list of log channels
  not in trace because close_logs may be called at a point where it may be
  modified
 *)
let logs : (out_channel list) ref = ref []

let close_logs () : unit =
  List.iter (fun clog -> close_out clog) !logs

let write_log (log : string) : unit =
  List.iter (fun (ctx, _) -> Transformations.write_log ctx.clog log) !trace

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


(******************************************************************************)
(*                                   Output                                   *)
(******************************************************************************)

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
  (* let file_json = out_prefix ^ ".json" in *)
  let file_enc = out_prefix ^ "_enc" ^ ctx.extension in
  let file_prog = out_prefix ^ ctx.extension in
  (* let out_json = open_out file_json in  *)
  let out_ast = open_out file_ast in
  let out_enc = open_out file_enc in
  let out_prog = open_out file_prog in
  let close_channels() =
    close_out out_ast;
    close_out out_enc;
    close_out out_prog
    in
  try
    (* print the raw ast *)
    (* Output the current ast into json format *)
    (* ast_to_js  *)
    print_ast (* ~only_desc:true *) out_ast ast;
    (* output_string out_json "\n"; *)
    output_string out_ast "\n";
    (* print C++ code without decoding *)
    output_string out_enc ctx.includes;
    ast_to_undecoded_doc out_enc ast; 
    output_string out_enc "\n";
    (* print C++ code with decoding *)
    output_string out_prog ctx.includes;
    ast_to_doc out_prog ast;
    output_string out_prog "\n";
    close_channels();
    (* beautify the C++ code *)
    cleanup_cpp_file_using_clang_format file_enc;
    cleanup_cpp_file_using_clang_format file_prog
  with
  | Failure s ->
      close_channels();
      failwith s

(* print ast in temporary file and parse it again *)
let reparse (ctx : context) (ast : trm) : trm =
  let in_prefix = ctx.directory ^ "tmp_" ^ ctx.prefix in
  output_prog ctx in_prefix ast;
  let (_, t) = parse (in_prefix ^ ctx.extension) in
  (*let _ = Sys.command ("rm " ^ in_prefix ^ "*") in*)
  t

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
      !trace;
    exit 0
  with
  | Stack.Empty -> fail None ("exit_script: script must be interrupted after " ^
                                "the initial source file is set.")

(*
  outputs code at each step using given prefix for filename
  out_prefix_in.cpp is the program before transformation
  out_prefix_out.cpp is the program after transformation
*)
let dump_trace ?(out_prefix : string = "") () : unit =
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
    !trace

(* outputs current code in given file *)
let dump ?(out_prefix : string = "") () : unit =
  if !Flags.full_dump then dump_trace ~out_prefix ()
  else
    List.iter
      (fun (ctx, astStack) ->
        let out_prefix =
          if out_prefix = "" then ctx.directory ^ ctx.prefix else out_prefix
        in
        output_prog ctx (out_prefix ^ "_out") (Stack.top astStack)
      )
      !trace

(* Wrapper function for unit tests, assuming "foo.ml" to be a script
   operating on "foo.cpp" and dumping the result in "foo_out.cpp".
   The option ast_decode can be used for tests that want to report on
   the "undecoded AST", by copying "foo_out_enc.cpp" onto "foo_out.cpp" *)

let run_unit_test ?(ast_decode : bool = true) (script : unit -> unit) : unit =
  let basename = Filename.chop_extension Sys.argv.(0) in
  run (fun () ->
    set_init_source (basename ^ ".cpp");
    script();
    dump ();
    if not ast_decode
      then ignore (Sys.command (Printf.sprintf "cp %s_out_enc.cpp %s_out.cpp" basename basename))
  )

(******************************************************************************)
(*                        Smart constructors for paths                        *)
(******************************************************************************)

open Path
include Path_constructors

type path = Path.path
type paths = path list
type case_dir = Path.case_dir
type abort_kind = Path.abort_kind
type constr_access = Path.constr_access
type case_kind = Path.case_kind
type enum_const_dir = Path.enum_const_dir

(******************************************************************************)
(*                              Transformations                               *)
(******************************************************************************)

(* add the given ast to the ast stack *)
let apply_to_top ?(replace_top : bool = false)
  (f : context -> trm -> trm) : unit =
  List.iter
    (fun (ctx, astStack) ->
      let ast =
        if replace_top then Stack.pop astStack else Stack.top astStack
      in
      let ast = f ctx ast in
      let ast = if !Flags.repeat_io then reparse ctx ast else ast in
      Stack.push ast astStack
    )
    !trace

(*
  label the term pointed by the path with label
  if several terms are pointed, then use indices (label_i)
 *)
let add_label ?(replace_top : bool = false) (label : string)
  (pl : paths) : unit =
  apply_to_top ~replace_top (fun _ -> Label.add_label label pl)


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
let show_path ?(debug_ast:bool=false) ?(replace_top : bool = false)?(keep_previous : bool = false) (pl : paths) : unit =
    apply_to_top ~replace_top (fun _ t ->
    let t =
      if not keep_previous
        then Transformations.delete_path_decorators t
        else t
      in
    Transformations.show_path ~debug_ast pl t
    )


let show_ast ?(replace_top:bool=false) ?(file:string="_ast.txt") ?(to_stdout:bool=true) (pl : path list) : unit =
  apply_to_top ~replace_top(fun _ -> Transformations.show_ast ~file ~to_stdout pl)

(* let show_ast ?(file:string="_ast.txt") ?(to_stdout:bool=true) (pl : paths) : unit =
  apply_to_top ~replace_top:false (fun _ t ->
  let p = List.flatten pl in 
  let b = !Flags.verbose in  (* TODO : get it of this stupid thing *)
  Flags.verbose := false;
  let epl = resolve_path p t in 
  Flags.verbose := b;
 
  match epl with 
  | [] ->
    print_info t.loc "show_path: not matching subterm\n";
  t
  | _ ->
     (*
         folding works since no path in epl is the prefix of a subsequent path
      *)
     let out_ast = open_out file in   
     let t= (foldi
       (fun i -> Transformations.apply_local_transformation
         (fun t -> 
            if to_stdout then begin
              print_ast ~only_desc:true stdout t;
              output_string stdout "\n\n";
            end;
            output_string out_ast (Printf.sprintf "=========================Occurence %i======================\n" i);
            print_ast ~only_desc:true out_ast t;
            output_string out_ast "\n\n";
            output_string out_ast (Printf.sprintf "------------------------Occurence %i details---------------\n" i);
            print_ast ~only_desc:false out_ast t;
            output_string out_ast "\n\n";
            t)  
                   
       ) t epl) in
     close_out out_ast;
     t) *)
       


let clean_path_decorators () : unit =
    apply_to_top ~replace_top:false (fun _ -> Transformations.delete_path_decorators)

let delete_label ?(replace_top : bool = false) (label : string) : unit =
  apply_to_top ~replace_top (fun _ -> Label.delete_label label)

(* delete the labels which have a prefix in the list *)
let delete_labels ?(replace_top : bool = false) (sl : string list) : unit =
  apply_to_top ~replace_top (fun _ -> Label.delete_labels sl)

(*
  transformation to swap the two first dimensions of an array
  assumption: x is a type variable that represents a multidimensional array type
  with >= 2 dimensions
  all variables of type x will be swapped
  assumption: x is not used in fun declarations
    -> to swap the first dimensions of a function argument, use swap_coordinates
    on the array on which the function is called: a new function with the
    appropriate type is generated
  function copies are named with name
  possibility: add label on new functions
*)
let swap_coordinates ?(replace_top : bool = false)
  ?(name : var -> var = fun x -> x ^ "_swapped") (x : typvar) : unit =
  let log : string =
    Printf.sprintf
      ("Swap_coordinates %s:\n" ^^
       "  - %s is not used in functions declarations\n" ^^
       "  - the name function outputs fresh names\n"
      )
      x x
  in
  write_log log;
  apply_to_top ~replace_top (fun ctx -> Arrays.swap_coord ctx.clog name x);
  write_log "\n"

(*
  split the sequence(s) around the instruction(s) pointed by pl
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
let split_sequence ?(replace_top : bool = false) ?(keep_labels : bool = false)
  ?(labels : string list = [])
  ?(split_name : string -> string = fun x -> x ^ "_split")
  (pl : paths) : unit =
  let (result_label, block1_label, block2_label) =
    match labels with
    | [] -> ("result", "result_block1", "result_block2")
    | [s] -> (s, s ^ "_block1", s ^ "_block2")
    | [result_label; block1_label; block2_label] ->
       (result_label, block1_label, block2_label)
    | _ -> fail None "split_sequence: bad labels"
  in
  let log : string =
    let ps = string_of_path (List.flatten pl) in
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
          block2_label split_name pl t
      in
      if keep_labels then t else
        Label.delete_labels [result_label; block1_label; block2_label]
          t
    );
  write_log "\n"

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
  ?(label : string = "") (pl : paths) : unit =
  let result_label = if label = "" then "result" else label in
  let log : string =
    Printf.sprintf "Extract_loop_var %s:\n" (string_of_path (List.flatten pl))
  in
  write_log log;
  apply_to_top ~replace_top
    (fun ctx t ->
      let t = Loop.extract_loop_var ctx.clog result_label pl t in
      if keep_label then t else Label.delete_label result_label t
    );
  write_log "\n"

let extract_loop_vars ?(replace_top : bool = false) ?(keep_label : bool = false)
  ?(label : string = "") (pl : paths) : unit =
  let result_label = if label = "" then "result" else label in
  let log : string =
    Printf.sprintf "Extract_loop_vars %s:\n" (string_of_path (List.flatten pl))
  in
  write_log log;
  apply_to_top ~replace_top
    (fun ctx t ->
      let t = Loop.extract_loop_vars ctx.clog result_label pl t in
      if keep_label then t else Label.delete_label result_label t
    );
  write_log "\n"

(*
  split the for loop pointed by pl
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
let split_loop_nodep ?(replace_top : bool = false) ?(keep_labels : bool = false)
  ?(labels : string list = []) (pl : paths) : unit =
  let (result_label, loop1_label, loop2_label) =
    match labels with
    | [] -> ("result", "result_loop1", "result_loop2")
    | [s] -> (s, s ^ "_loop1", s ^ "_loop2")
    | [result_label; loop1_label; loop2_label] ->
       (result_label, loop1_label, loop2_label)
    | _ -> fail None "split_loop_nodep: bad labels"
  in
  let log : string =
    Printf.sprintf "Split_loop_nodep %s:\n" (string_of_path (List.flatten pl))
  in
  write_log log;
  apply_to_top ~replace_top
    (fun ctx t ->
      let t =
        Loop.split_loop_nodep ctx.clog result_label loop1_label
          loop2_label pl t
      in
      if keep_labels then t else
        Label.delete_labels [result_label; loop1_label; loop2_label] t
    );
  write_log "\n"

(*
  combine split_sequence, extract_loop_var and split_loop_nodep to split the for
  loop after the instruction pointed by pl in t
 *)
let split_loop ?(replace_top : bool = false) ?(keep_labels : bool = false)
  ?(labels : string list = [])
  ?(split_name : string -> string = fun x -> x ^ "_split")
  (pl : paths) : unit =
  let (result_label, loop1_label, loop2_label) =
    match labels with
    | [] -> ("result", "result_loop1", "result_loop2")
    | [s] -> (s, s ^ "_loop1", s ^ "_loop2")
    | [result_label; loop1_label; loop2_label] ->
       (result_label, loop1_label, loop2_label)
    | _ -> fail None "split_loop: bad labels"
  in
  let (log_begin, log_end) : string * string =
    let ps = string_of_path (List.flatten pl) in
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
    pl;
  (* make sure at most one ast is added to the stack *)
  let replace_top = true in
  (* label the loop for later calls to transformations *)
  add_label ~replace_top "split_loop_tmp_loop"
    [cFor ~body:[cLabel ~strict:true ~label:"split_loop_tmp_result"
                   ~exact:false ()] ()];
  (* remove unnecessary labels *)
  delete_labels ~replace_top ["split_loop_tmp_result"; "split_loop_tmp_block1";
                              "split_loop_tmp_block2"];
  let pl' = [cLabel ~label:"split_loop_tmp_loop" ~exact:false ()] in
  (* extract loop variables *)
  extract_loop_vars ~replace_top ~keep_label:true ~label:result_label pl';
  (* split the loop *)
  split_loop_nodep ~replace_top ~keep_labels:true
    ~labels:["split_loop_tmp_result"; loop1_label; loop2_label] pl';
  (* clean up *)
  delete_labels ~replace_top ["split_loop_tmp_result"; "split_loop_tmp_loop"];
  if not keep_labels then
    delete_labels ~replace_top [result_label; loop1_label; loop2_label];
  write_log log_end

(*
  context must contain the declaration of variables that are used in s
  warning: do not forget semicolons
 *)
let term (ctx : context) ?(context : string = "") (s : string) : trm =
  let context = if context = "" then ctx.includes else context in
  (* parse_string outputs a translation_unit, i.e. a list of declarations *)
  (* Printf.printf "context: %s\n" context; *)
  let command_line_args =
    List.map Clang.Command_line.include_directory
      (ctx.directory :: Clang.default_include_directories ())
  in
  let ast =
    parse_string ~filename:("input_string" ^ ctx.extension) ~command_line_args
      (Printf.sprintf
         {|
          %s
          void f(void){
            #pragma clang diagnostic ignored "-Wunused-value"
            %s;
          }
          |}
         context
         s
      )
  in
  let t = translate_ast ast in
  (* ast_to_doc stdout t; print_newline ();
   * print_ast ~only_desc:true stdout t; print_newline (); *)
  let term_from_f (def_f : trm) : trm =
    match def_f.desc with
    | Trm_decl (Def_fun (_, _, _, body)) ->
       begin match body.desc with
        | Trm_seq [t] -> t
        | _ -> fail def_f.loc "term_from_f: unexpected body"
        end
    | _ -> fail def_f.loc "term_from_f: expected definition"
  in
  let rec get_term (t : trm) : trm =
    match t.desc with
    (*
      if the context contains heap allocated variables, t contains a deletion
      list
     *)
    | Trm_seq (t' :: _) when t.annot = Some Delete_instructions -> get_term t'
    (* otherwise find the declaration of f *)
    | Trm_seq tl -> get_term (List.hd (List.rev tl))
    (* once the declaration is found, look for the term inside *)
    | Trm_decl _ -> term_from_f t
    | _ -> fail t.loc "get_term: unexpected result"
  in
  get_term t

(* output the code of declarations in t up to the end of the explicit path *)
let get_context (ctx : context) (dl : expl_path) (t : trm) : string =
  let (_, decl_l) = resolve_explicit_path dl t in
  ctx.includes ^ ast_to_string (trm_seq ~annot:(Some No_braces) decl_l)

let tile_array ?(replace_top : bool = false)
  ?(name : var -> var = fun x -> x ^ "_tiled") ?(block_name : typvar = "")
  ~block_size:(b : string) (x : typvar) : unit =
  let block_name = if block_name = "" then x ^ "_block" else block_name in
  let log : string =
    Printf.sprintf
      ("Tile_array ~block_size:%s %s:\n" ^^
       "  - %s is not used in functions declarations\n" ^^
       "  - %s is a valid expression at the declaration of %s\n" ^^
       "  - %s is a fresh name\n" ^^
       "  - the name function outputs fresh names\n"
      )
      b x x b x block_name
  in
  write_log log;
  apply_to_top ~replace_top
    (fun ctx t ->
      match path_to_decl x t with
      | None -> fail t.loc ("tile_array: unable to find declaration of " ^ x)
      | Some dl ->
         let context = get_context ctx dl t in
         Arrays.tile ctx.clog name block_name (term ctx ~context b) x t
    );
  write_log "\n"

(*
  todo: uncomment the 3 following functions once users have access to the ast
 *)
(*
  insert t_inserted either before the position pointed at by insert_before or
  after the position pointed at by insert_after
  both must be resolved as paths to a seq element
  bonus: check that inserting the given trm gives a valid ast
 *)
(* let insert_trm ?(replace_top : bool = false) ?(insert_before : paths = [])
 *   ?(insert_after : paths = []) ~term:(t_inserted : trm) (_ : unit) : unit =
 *   apply_to_top ~replace_top
 *     (fun _ ->
 *       Transformations.insert_trm ~insert_before ~insert_after t_inserted) *)

(*
  replace occurrences of t_before with t_after
  paths point at subterms in which all occurences will be replaced
  the empty path means all occurences will be replaced (default behaviour)
  assumption: t_before and t_after are equivalent (in terms of value and of side
  effects)
 *)
(* let change_trm ?(replace_top : bool = false)
 *   ?(change_at : paths list = [[]]) ~before:(t_before : trm)
 *   ~after:(t_after : trm) (_ : unit) : unit =
 *   apply_to_top ~replace_top
 *     (fun _ -> Transformations.change_trm ~change_at t_before t_after) *)

(* same as change_trm but for types *)
(* let change_typ ?(replace_top : bool = false)
 *   ?(change_at : paths list = [[]]) ~before:(ty_before : typ)
 *   ~after:(ty_after : typ) (_ : unit) : unit =
 *   apply_to_top ~replace_top
 *     (fun _ -> Transformations.change_typ ~change_at ty_before ty_after) *)

(*
  find the definition x = dx pointed at by pl and replace occurrences of dx with
  x
  paths in fold_at point at subterms in which all occurences will be replaced
  the empty path means all occurences will be replaced (default behaviour)
  as_reference option for variable declarations: if dx = &dx' replace dx' with
  *x instead of &dx' with x
 *)
let fold_decl ?(replace_top : bool = false) ?(as_reference : bool = false)
  ?(fold_at : paths list = [[]]) ~decl_path:(pl : paths)
  (_ : unit) : unit =
  let log : string =
    Printf.sprintf "Fold_decl ~decl_path:%s:\n"
      (string_of_path (List.flatten pl))
  in
  write_log log;
  apply_to_top ~replace_top
    (fun ctx -> Declaration.fold_decl ctx.clog ~as_reference ~fold_at pl);
  write_log "\n"

(*
  insert a definition x = dx either before the position pointed at by
  insert_before or after the position pointed at by insert_after
  both must be resolved as paths to a seq element
  x may be a const variable or not (not const by default)
  option: make x a reference (x = &dx)
  assumptions:
    - no conflicts with the new name x
    - for a given seq, the insertion path points to at most one of its elements
    - if x is a reference, dx denotes a memory cell
  todo: allow for dx of type trm (and use it to implement string version)
 *)
let insert_decl ?(replace_top : bool = false) ?(insert_before : paths = [])
  ?(insert_after : paths = []) ?(const : bool = false)
  ?(as_reference : bool = false) ~name:(x : var) ~value:(dx : string)
  (_ : unit) : unit =
  let p =
    match insert_before, insert_after with
    | [], _ :: _ -> List.flatten insert_after
    | _ :: _, [] -> List.flatten insert_before
    | [], [] -> fail None "insert_decl: please specify an insertion point"
    | _ -> fail None "insert_decl: cannot insert both before and after"
  in
  let log : string =
    let ps = string_of_path p in
    Printf.sprintf
      ("Insert_decl ~name:%s ~value:%s:\n" ^^
       "  - %s is fresh\n" ^^
       "  - %s is a valid expression at the insertion point(s)\n" ^^
       "  - %s points at at most one element per seq\n"
      )
      x dx x dx ps ^
    if as_reference then
      Printf.sprintf "  - %s denotes a memory cell\n" dx
    else ""
  in
  write_log log;
  let insert_aux (ctx : context) (t : trm) : trm =
    let b = !Flags.verbose in
    Flags.verbose := false;
    let epl = resolve_path p t in
    Flags.verbose := b;
    List.iter
      (fun dl ->
        let (t', _) = resolve_explicit_path dl t in
        let log : string =
          let loc : string =
            match t'.loc with
            | None -> ""
            | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
          in
          Printf.sprintf
            ("  - expression\n%s\n" ^^
             "    %sis located inside a sequence\n"
            )
            (ast_to_string t') loc
        in
        write_log log
      )
      epl;
    match epl with
    | [] ->
       print_info t.loc "insert_decl: no matching subterm\n";
       t
    | dl :: _ ->
       let dl =
         match List.rev dl with
         | Dir_nth n :: dl' ->
            let n =
              match insert_before, insert_after with
              (* insert after: add 1 to n *)
              | [], _ :: _ -> n + 1
              (* insert before: n is the position of the definition *)
              | _ :: _, [] -> n
              | [], [] ->
                 fail t.loc "insert_decl: please specify an insertion point"
              | _ ->
                 fail t.loc "insert_decl: cannot insert both before and after"
            in
            List.rev (Dir_nth n :: dl')
         | _ -> fail t.loc "insert_decl: expected a path to a seq"
       in
       let context = get_context ctx dl t in
       let def_x = term ctx ~context dx in
       Declaration.insert_decl ~insert_before ~insert_after ~const
         ~as_reference x def_x t
  in
  apply_to_top ~replace_top insert_aux;
  write_log "\n"

(* same as insert_definition but for a constant *)
let insert_const ?(replace_top : bool = false) ?(insert_before : paths = [])
  ?(insert_after : paths = []) ~name:(name : var) ~value:(value : string)
  (_ : unit) : unit =
  let (log_begin, log_end) : string * string =
    (Printf.sprintf "##### Start of insert_const ~name:%s ~value:%s #####\n\n"
       name value,
     Printf.sprintf "##### End of insert_const ~name:%s ~value:%s #####\n\n"
       name value)
  in
  write_log log_begin;
  insert_decl ~replace_top ~insert_before ~insert_after ~const:true ~name ~value
    ();
  write_log log_end

(*
  combine insert_definition and fold_decl
  assumption: if x is not a reference, no effects for dx and it has the same
  value through all its occurences
 *)
let insert_and_fold ?(replace_top : bool = false)
  ?(insert_before : paths = []) ?(insert_after : paths = [])
  ?(const : bool = false) ?(as_reference : bool = false)
  ?(fold_at : paths list = [[]]) ~name:(x : var) ~value:(dx : string)
  (_ : unit) : unit =
  let p =
    match insert_before, insert_after with
    | [], _ :: _ -> List.flatten insert_after
    | _ :: _, [] -> List.flatten insert_before
    | [], [] ->
       fail None "insert_and_fold: please specify an insertion point"
    | _ -> fail None "insert_and_fold: cannot insert both before and after"
  in
  let log : string =
    let ps = string_of_path p in
    Printf.sprintf
      ("Insert_and_fold ~name:%s ~value:%s:\n" ^^
       "  - %s is fresh\n" ^^
       "  - %s is a valid expression at the insertion point(s)\n" ^^
       "  - %s points at at most one element per seq\n"
      )
      x dx x dx ps ^
    if as_reference then
      Printf.sprintf "  - %s denotes a memory cell\n" dx
    else
      Printf.sprintf
        ("  - %s has no effects and keep the same value " ^^
           "through all its occurences\n"
        )
        dx
  in
  write_log log;
  let insert_aux (ctx : context) (t : trm) : trm =
    let b = !Flags.verbose in
    Flags.verbose := false;
    let epl = resolve_path p t in
    Flags.verbose := b;
    List.iter
      (fun dl ->
        let (t', _) = resolve_explicit_path dl t in
        let log : string =
          let loc =
            match t'.loc with
            | None -> ""
            | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
          in
          Printf.sprintf
            ("  - expression\n%s\n" ^^
             "    %sis located inside a seq\n"
            )
            (ast_to_string t') loc
        in
        write_log log
      )
      epl;
    match epl with
    | [] ->
       print_info t.loc "insert_and_fold: no matching subterm\n";
       t
    | dl :: _ ->
       let dl =
         match List.rev dl with
         | Dir_nth n :: dl' ->
            let n =
              match insert_before, insert_after with
              (* insert after: add 1 to n *)
              | [], _ :: _ -> n + 1
              (* insert before: n is the position of the definition *)
              | _ :: _, [] -> n
              | [], [] ->
                 fail t.loc "insert_and_fold: please specify an insertion point"
              | _ ->
                 fail t.loc
                   "insert_and_fold: cannot insert both before and after"
            in
            List.rev (Dir_nth n :: dl')
         | _ -> fail t.loc "insert_and_fold: expected a path to a seq"
       in
       let context = get_context ctx dl t in
       let def_x = term ctx ~context dx in
       Declaration.insert_and_fold ctx.clog ~insert_before ~insert_after
         ~const ~as_reference ~fold_at x def_x t
  in
  apply_to_top ~replace_top insert_aux;
  write_log "\n"

(*
  context must contain the declaration of type variables that are used in s
  warning: do not forget semicolons
 *)
let type_ (ctx : context) ?(context : string = "") (s : string) : typ =
  let context = if context = "" then ctx.includes else context in
  let t = term ctx ~context ("typedef " ^ s ^ " x") in
  match t.desc with
  | Trm_decl (Def_typ (_, tx)) -> tx
  | _ -> fail t.loc "type_: unexpected output"

(*
  insert a type declaration x = dx either before the position pointed at by
  insert_before or after the position pointed at by insert_after
  both must be resolved as paths to a seq element
  assumption: no conflicts with the new name x
  todo: allow for dx of type typ
 *)
let insert_typedef ?(replace_top : bool = false)
  ?(insert_before : paths = []) ?(insert_after : paths = [])
  ~name:(x : typvar) ~value:(dx : string) (_ : unit) : unit =
  let p =
    match insert_before, insert_after with
    | [], _ :: _ -> List.flatten insert_after
    | _ :: _, [] -> List.flatten insert_before
    | [], [] ->
       fail None "insert_typedef: please specify an insertion point"
    | _ -> fail None "insert_typedef: cannot insert both before and after"
  in
  let log : string =
    let ps = string_of_path p in
    Printf.sprintf
      ("Insert_typedef ~name:%s ~value:%s:\n" ^^
       "  - %s is fresh\n" ^^
       "  - %s is a valid expression at the insertion point\n" ^^
       "  - %s points at at most one element per seq\n"
      )
      x dx x dx ps
  in
  write_log log;
  let insert_aux (ctx : context) (t : trm) : trm =
    let b = !Flags.verbose in
    Flags.verbose := false;
    let epl = resolve_path p t in
    Flags.verbose := b;
    List.iter
      (fun dl ->
        let (t', _) = resolve_explicit_path dl t in
        let log : string =
          let loc : string =
            match t'.loc with
            | None -> ""
            | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
          in
          Printf.sprintf
            ("  - expression\n%s\n" ^^
             "    %sis located inside a seq\n"
            )
            (ast_to_string t') loc
        in
        write_log log
      )
      epl;
    match epl with
    | [] ->
       print_info t.loc "insert_typedef: no matching subterm\n";
       t
    | dl :: _ ->
       let dl =
         match List.rev dl with
         | Dir_nth n :: dl' ->
            let n =
              match insert_before, insert_after with
              (* insert after: add 1 to n *)
              | [], _ :: _ -> n + 1
              (* insert before: n is the position of the definition *)
              | _ :: _, [] -> n
              | [], [] ->
                 fail t.loc "insert_typedef: please specify an insertion point"
              | _ ->
                 fail t.loc
                   "insert_typedef: cannot insert both before and after"
            in
            List.rev (Dir_nth n :: dl')
         | _ -> fail t.loc "insert_typedef: expected a path to a seq"
       in
       let context = get_context ctx dl t in
       let def_x = type_ ctx ~context dx in
       Declaration.insert_typedef ~insert_before ~insert_after x def_x t
  in
  apply_to_top ~replace_top insert_aux

let insert_and_fold_typedef ?(replace_top : bool = false)
  ?(insert_before : paths = []) ?(insert_after : paths = [])
  ?(fold_at : paths list = [[]]) ~name:(x : typvar) ~value:(dx : string)
  (_ : unit) : unit =
  let p =
    match insert_before, insert_after with
    | [], _ :: _ -> List.flatten insert_after
    | _ :: _, [] -> List.flatten insert_before
    | [], [] ->
       fail None "insert_and_fold_typedef: please specify an insertion point"
    | _ ->
       fail None
         "insert_and_fold_typedef: cannot insert both before and after"
  in
  let log : string =
    let ps = string_of_path p in
    Printf.sprintf
      ("Insert_and_fold_typedef ~name:%s ~value:%s:\n" ^^
       "  - %s is fresh\n" ^^
       "  - %s is a valid expression at the insertion point\n" ^^
       "  - %s points at at most one element per seq\n"
      )
      x dx x dx ps
  in
  write_log log;
  let insert_aux (ctx : context) (t : trm) : trm =
    let b = !Flags.verbose in
    Flags.verbose := false;
    let epl = resolve_path p t in
    Flags.verbose := b;
    List.iter
      (fun dl ->
        let (t', _) = resolve_explicit_path dl t in
        let log : string =
          let loc : string =
            match t'.loc with
            | None -> ""
            | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
          in
          Printf.sprintf
            ("  - expression\n%s\n" ^^
             "    %sis located inside a seq\n"
            )
            (ast_to_string t') loc
        in
        write_log log
      )
      epl;
    match epl with
    | [] ->
       print_info t.loc "insert_and_fold_typdef: no matching subterm\n";
       t
    | dl :: _ ->
       let dl =
         match List.rev dl with
         | Dir_nth n :: dl' ->
            let n =
              match insert_before, insert_after with
              (* insert after: add 1 to n *)
              | [], _ :: _ -> n + 1
              (* insert before: n is the position of the definition *)
              | _ :: _, [] -> n
              | [], [] ->
                 fail t.loc
                   "insert_and_fold_typedef: please specify an insertion point"
              | _ ->
                 fail t.loc ("insert_and_fold_typedef: cannot insert both " ^
                               "before and after")
            in
            List.rev (Dir_nth n :: dl')
         | _ -> fail t.loc "insert_and_fold_typedef: expected a path to a seq"
       in
       let context = get_context ctx dl t in
       let def_x = type_ ctx ~context dx in
       Declaration.insert_and_fold_typedef ctx.clog ~insert_before
         ~insert_after ~fold_at x def_x t
  in
  apply_to_top ~replace_top insert_aux;
  write_log "\n"

(* todo: inlining with flag "error if occurrence" + flag "delete"? *)
let remove_decl ?(replace_top : bool = false) ~decl_path:(pl : paths)
  (_ : unit) : unit =
  let log : string =
    let ps = string_of_path (List.flatten pl) in
    Printf.sprintf
      ("Remove_decl ~decl_path:%s\n" ^^
       "  - %s points at exactly one program point\n"
      )
      ps ps
  in
  write_log log;
  apply_to_top ~replace_top
    (fun ctx -> Declaration.remove_decl ctx.clog pl);
  write_log "\n"

let inline_decl ?(replace_top : bool = false) ?(delete_decl : bool = false)
  ?(inline_at : paths list = [[]]) ?(fun_result : var = "res") ?(fun_args : var list = [])
  ?(fun_return_label : label = "exit") ~decl_path:(pl : paths)
  (_ : unit) : unit =
  let log : string =
    let ps = string_of_path (List.flatten pl) in
    Printf.sprintf
      ("Inline_decl ~decl_path:%s:\n" ^^
       "  - %s points at exactly one program point\n"
      )
      ps ps
  in
  write_log log;
  apply_to_top ~replace_top
    (fun ctx ->
      Inlining.inline_decl ctx.clog ~delete_decl ~inline_at ~fun_result ~fun_args
       ~fun_return_label pl);
  write_log "\n"

let fields_reorder ?(replace_top : bool = false) (pl : paths) ?(struct_fields : fields = []) ?(move_before : field = "") ?(move_after : field = "")(_ : unit) : unit =
  let log : string =
    let ps = string_of_path (List.flatten pl) in
    Printf.sprintf
      ("Inline_decl ~decl_path %s:\n" ^^
       " - %s points at exactly one program point\n"
      )
      ps ps
  in
  write_log log;
  apply_to_top ~replace_top
    (fun ctx ->
      Struct.fields_reorder ctx.clog  ~struct_fields pl ~move_before ~move_after
    );
  write_log "\n"

(*
  transform a pre-tiled loop of the form
    optional_label:
    for i = 0; i < N; i++
      int i1 = i / block_size
      int i2 = i % block_size
      body
  into a loop of the form
    optional_label:
    for i1 = 0; i1 < N / block_size; i1++
      for i2 = 0; i2 < block_size; i2++
        i = i1 * block_size + i2 // only if i is used in body
        body
  assumption: N is divisible by block_size
  todo: label i as "generated variable" + implement clean up transformation
  "remove all unused generated variables"
 *)
let tile_loop ?(replace_top : bool = false)
  (pl : paths) : unit =
  let log : string =
    Printf.sprintf "Tile_loop %s:\n" (string_of_path (List.flatten pl))
  in
  write_log log;
  apply_to_top ~replace_top
    (fun ctx -> Loop.tile_loop ctx.clog pl);
  write_log "\n"

let loop_coloring ?(replace_top : bool = false) (pl : paths) (c: var) (new_var : var): unit =
    let log : string =
      Printf.sprintf "Transform_loop %s:\n" (string_of_path (List.flatten pl))
    in
    write_log log;
    apply_to_top ~replace_top
      (fun ctx -> Loop.loop_coloring ctx.clog pl c new_var);
    write_log "\n"

let loop_tile ?(replace_top : bool = false) (pl : paths) (b: var) (new_var : var): unit =
    let log : string =
      Printf.sprintf "Transform_loop %s:\n" (string_of_path (List.flatten pl))
    in
    write_log log;
    apply_to_top ~replace_top
      (fun ctx -> Loop.loop_tile ctx.clog pl b new_var);
    write_log "\n"

let loop_swap ?(replace_top : bool = false) (pl : paths) : unit =
    let log : string =
      Printf.sprintf "Swap_loop %s:\n" (string_of_path (List.flatten pl))
    in
    write_log log;
    apply_to_top ~replace_top
      (fun ctx -> Loop.loop_swap ctx.clog pl );
    write_log "\n"

let move_loop_before ?(replace_top : bool = false) (pl : paths) (loop_index : var) : unit =
    let log : string =
      Printf.sprintf "move_loop_before %s:\n" (string_of_path (List.flatten pl))
    in
    write_log log;
    apply_to_top ~replace_top
      (fun ctx -> Loop.move_loop_before ctx.clog pl loop_index);
    write_log "\n"

let move_loop_after ?(replace_top : bool = false) (pl : paths) (loop_index : var) : unit =
    let log : string =
      Printf.sprintf "move_loop_after %s:\n" (string_of_path (List.flatten pl))
    in
    write_log log;
    apply_to_top ~replace_top
      (fun ctx -> Loop.move_loop_after ctx.clog pl loop_index);
    write_log "\n"

let move_loop ?(replace_top : bool = false) ?(move_before : string  = "") ?(move_after : string = "" ) (loop_index : string) : unit =
    apply_to_top ~replace_top
      (fun ctx -> Loop.move_loop ctx.clog  ~move_before ~move_after loop_index);
    write_log "\n"

let inline_struct ?(replace_top : bool = false) ?(struct_name : string = "") ?(struct_fields : fields = []) (): unit =
  apply_to_top ~replace_top
    (fun ctx -> Inlining.inline_struct ctx.clog struct_name ~struct_fields);
  write_log "\n"

let inline_record_access ?(replace_top : bool = false) ?(field : string = "") ?(var : string = "") () : unit =
  apply_to_top ~replace_top
    (fun ctx -> Inlining.inline_record_access ctx.clog  field var);
  write_log "\n"

let make_explicit_record_assignment?(replace_top : bool = false) ?(struct_name : string = "") (pl : paths) : unit =
  apply_to_top ~replace_top
    (fun ctx -> Struct.make_explicit_record_assigment ctx.clog ~struct_name pl);
  write_log "\n"

let detach_expression ?(replace_top : bool = false) ?(label : string = "detached") ?(keep_label : bool = false) (pl : paths) : unit =
  apply_to_top ~replace_top
    (fun ctx -> Transformations.detach_expression ctx.clog ~label ~keep_label  pl);
    write_log "\n"

let remove_instruction ?(replace_top : bool = false) (pl : paths) : unit = 
  apply_to_top ~replace_top
    (fun ctx -> Transformations.remove_instruction ctx.clog pl);
  write_log "\n"

let remove_instructions ?(replace_top : bool = false) (isntruction_list : paths list) : unit = 
  apply_to_top ~replace_top
    (fun ctx -> Transformations.remove_instructions ctx.clog isntruction_list );
  write_log "\n"

let undetach_expression ?(replace_top : bool = false) (pl : paths) : unit =
  apply_to_top ~replace_top
    (fun ctx -> Transformations.undetach_expression ctx.clog pl);
    write_log "\n"

let make_implicit_record_assignment ?(replace_top : bool = false) ?(struct_name : string = "") (pl : paths)  : unit = 
  apply_to_top ~replace_top 
  (fun ctx -> Struct.make_implicit_record_assignment ctx.clog struct_name pl);
  write_log "\n"

let create_subsequence ?(replace_top : bool = false) ?(start : paths = []) ?(stop : paths = []) ?(stop_before : bool = false) ?(stop_after : bool = false) ?(label : string = "") ?(braces : bool = false) () : unit = 
  apply_to_top ~replace_top
    (fun ctx -> Sequence.create_subsequence ctx.clog start stop stop_before stop_after label braces);
  write_log "\n"

let array_to_variables ?(replace_top : bool = false) (dcl_path : paths) (new_vars : var list) : unit = 
  apply_to_top ~replace_top 
    (fun ctx -> Arrays.array_to_variables ctx.clog dcl_path new_vars);
    write_log "\n"

let local_other_name ?(replace_top : bool = false) ?(section_of_interest : label = "") ?(new_var_type : typvar = "") ?(old_var : var = "") ?(new_var : var = "") () : unit =
  apply_to_top ~replace_top
    (fun ctx -> Transformations.local_other_name ctx.clog section_of_interest new_var_type old_var new_var );
    write_log "\n"

let const_non_const ?(replace_top : bool = false) (pl : path list) : unit = 
  apply_to_top ~replace_top
    (fun ctx -> Transformations.const_non_const ctx.clog pl );
  write_log "\n"

let delocalize ?(replace_top : bool = false) ?(section_of_interest : label = "") ?(array_size : string = "") ?(neutral_element : int = 0) ?(fold_operation : string = "") () : unit = 
  apply_to_top ~replace_top
    (fun ctx -> Transformations.delocalize ctx.clog section_of_interest array_size neutral_element fold_operation);
    write_log "\n"

(* let rewrite ?(replace_top : bool = false) ?(rule : string = "") ?(path : paths = [ ]) : () : unit = 
  apply_to_top ~replace_top
    (fun ctx -> Transformations.rewrite ctx.clog rule path );
  write_log "\n"
*)

let aos_to_soa ?(replace_top : bool = false)
  ?(name : var -> var = fun x -> x ^ "_swapped") (x : typvar) : unit =
  let log : string =
    Printf.sprintf
      ("Aos_to_soa %s:\n" ^^
       "  - %s is not used in functions declarations\n" ^^
       "  - the name function outputs fresh names\n"
      )
      x x
  in
  write_log log;
  apply_to_top ~replace_top (fun ctx -> Arrays.aos_to_soa ctx.clog name x);
  write_log "\n"


let eliminate_goto_next ?(replace_top : bool = false) (_ : unit) : unit =
  let log = "Eliminate_goto_next: no assumptions\n\n" in
  write_log log;
  apply_to_top ~replace_top (fun _ -> Declaration.eliminate_goto_next)

let group_decl_init ?(replace_top : bool = false) (_ : unit) : unit =
  let log = "Group_decl_init: no assumptions\n\n" in
  write_log log;
  apply_to_top ~replace_top (fun _ -> Declaration.group_decl_init)

let inline_seq ?(replace_top : bool = false) ~seq_path:(pl : paths)
  (_ : unit) : unit =
  let log : string =
    Printf.sprintf "Inline_seq ~seq_path:%s:\n"
      (string_of_path (List.flatten pl))
  in
  write_log log;
  apply_to_top ~replace_top (fun ctx -> Inlining.inline_seq ctx.clog pl);
  write_log "\n"

(* todo: generalise to all attributes *)
let add_attribute ?(replace_top : bool = false) (s : string)
  (pl : paths) : unit =
  let log : string =
    let ps = string_of_path (List.flatten pl) in
    Printf.sprintf
      ("Add_attribute %s %s:\n" ^^
       "  - %s denotes an attribute\n"
      )
      s ps s
  in
  write_log log;
  apply_to_top ~replace_top
    (fun ctx -> Transformations.add_attribute ctx.clog (Identifier s) pl);
  write_log "\n"

  
