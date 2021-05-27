open Ast
open Ast_to_c
open Trace
open Ast_to_text
open Ast_to_js

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
let trm_to_log (clog : out_channel) (t : trm) : unit =
  let log : string =
    let loc : string =
    match t.loc with 
    | None -> ""
    | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in 
    Printf.sprintf
    (" -expression\n%s\n" ^^
    " %s is sequence of terms \n"
    )
    (ast_to_string t) loc 
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

(* print ast in temporary file and parse it again *)
let reparse (ctx : context) (ast : trm) : trm =
  let in_prefix = ctx.directory ^ "tmp_" ^ ctx.prefix in
  output_prog ctx in_prefix ast;
  let (_, t) = parse (in_prefix ^ ctx.extension) in
  (*let _ = Sys.command ("rm " ^ in_prefix ^ "*") in*)
  t

(* TODO: Move apply_to_top to a better place *)
  (* apply_to_top: add the given ast to the ast stack 

*)
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
    (get_trace())


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
      !trace;
    exit 0
  with
  | Stack.Empty -> fail None ("exit_script: script must be interrupted after " ^
                                "the initial source file is set.")


let dump_trace_to_js ?(out_prefix : string = "") () : unit =
  (* Initialize var content and source as empty arrays *)
  let () = initialization out_prefix in
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
      !trace

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
    !trace *)

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
    flush stdout;
    dump ();
    if not ast_decode
      then ignore (Sys.command (Printf.sprintf "cp %s_out_enc.cpp %s_out.cpp" basename basename))
  )