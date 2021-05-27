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



