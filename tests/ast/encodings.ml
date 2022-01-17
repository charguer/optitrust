open Optitrust
open Target
open CRawAst_to_ast


let check_id (test_name : string) (f : trm -> trm) : trm -> trm =
  fun t1 ->
    let t2 = f t1 in
    check test_name t1 t2;
    t2

let _ = Run.script_cpp ~filename:"c_ast.cpp" (fun () ->

  !! Trace.apply stackvar_elim;
  !! Trace.apply stackvar_intro;
  !! Trace.apply (check_id "roundtrip_stackvar" (fun t -> stackvar_intro (stackvar_elim t)));

  (* Note: address_elim might not work in the presence of stack variables *)
  !! Trace.apply stackvar_elim;

  !! Trace.apply address_elim;
  !! Trace.apply address_intro;
  !! Trace.apply (check_id "roundtrip_address" (fun t -> address_intro (address_elim t)));
)
(* use ctrl+shift+f6  for viewing encodings *)




let ast_to_file (filename : string) (t : trm) : unit =
  let out = open_out filename in
  Ast_to_text.print_ast out t;
  close_out out
  (* TODO: move to Ast_to_text.print_ast_to_file *)

let rawC_to_file (filename : string) (t : trm) : unit =
  let out = open_out filename in
  Ast_to_c.print_ast ~decode:false out t;
  close_out out
  (* TODO: move to Ast_to_text.print_ast_to_file *)


let check (test_name : string) (t1 : trm) (t2 : trm) : unit =
  (* LATER: see if we can have a ast comparison function *)
  let success = Ast_to_text.ast_to_string t1 = Ast_to_text.ast_to_string t2 in
  Printf.printf "Checking %s:\n" test_name (if success then "succcess" else Printf.sprintf "failure (see %s_ast_{1,2}.{ast,cpp})" filename;
  if not success then begin
      rawC_to_file (test_name "_ast_1.cpp");
      rawC_to_file (test_name "_ast_2.cpp");
      ast_to_file test_name "_ast_1.ast");
      ast_to_file (test_name "_ast_2.ast");
  end

let test_encodings () =
  let clang_ast = Clang.Ast.parse_file "c_ast.cpp" in
  let raw_ast = Clang_to_astRawC.translate_ast clang_ast in
  (* let ° be the usual function composition symbol *)
  (* test1: check if stackvar_intro ° stackvar_elim = id *)
  (* TODO: keep code for printing result of   Ast_to_text.print_ast_to_file (stackvar_elim raw_ast) *)
  let raw_ast1 = stackvar_intro (stackvar_elim raw_ast) in

  if Ast_to_text.ast_to_string raw_ast1 = Ast_to_text.ast_to_string raw_ast then Printf.printf "Test1 passed\n" else Printf.printf "Test1 failed\n";

  (* test2: check if caddress_intro ° caddress_elim = id *)
  let raw_ast2 = caddress_intro false (caddress_elim false raw_ast) in
  let out1 = open_out "before_enc.ast" in
  let out2 = open_out "after_enc.ast" in
  Ast_to_text.print_ast out1 raw_ast;
  Ast_to_text.print_ast out2 raw_ast2;
  close_out out1;
  close_out out2;
  if Ast_to_text.ast_to_string raw_ast2 = Ast_to_text.ast_to_string raw_ast then Printf.printf "Test2 passed\n" else Printf.printf "Test2 failed\n"
  (* test3: check if decode ° encode = id, where encode applies stackvar_elim followed by caddress_intro *)
  (* let raw_ast3 = decode (encode raw_ast) in *)
  (* if raw_ast3 = raw_ast then Printf.printf "Test3 passed\n" else Printf.printf "Test3 failed\n" *)

let _ = test_encodings ()
