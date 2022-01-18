open Optitrust
open Target
open Ast
open Ast_to_rawC
open CRawAst_to_ast


let check (test_name : string) (t1 : trm) (t2 : trm) : unit =
  (* LATER: see if we can have a ast comparison function *)
  let success = Ast_to_text.ast_to_string t1 = Ast_to_text.ast_to_string t2 in
  Printf.printf "Checking %s: %s\n" test_name (if success then "succcess" else Printf.sprintf "failure (see %s_ast_{1,2}.{ast,cpp})" test_name);
  if not success then begin
      Ast_to_rawC.ast_to_file (test_name ^ "_ast_1.cpp") t1;
      Ast_to_rawC.ast_to_file (test_name ^ "_ast_2.cpp") t2;
      Ast_to_text.ast_to_file (test_name ^ "_ast_1.ast") t1;
      Ast_to_text.ast_to_file (test_name ^ "_ast_2.ast") t2;
  end


let check_id (test_name : string) (f : trm -> trm) (t : trm) : unit =
  let t1 = f t in 
  check test_name t t1

(* let check_id (test_name : string) (f : trm -> trm) : trm -> trm =
  fun t1 ->
    let t2 = f t1 in
    check test_name t1 t2;
    t2 *)




(* use ctrl+shift+f6  for viewing encodings *)
let test_encodings () =
  let clang_ast = Clang.Ast.parse_file "c_ast.cpp" in
  let raw_ast = Clang_to_astRawC.translate_ast clang_ast in

  (* test1: check if stackvar_intro ° stackvar_elim = id *)
  check_id "Stack variables" (fun t -> stackvar_intro (stackvar_elim t)) raw_ast;
  
  (* test2: check if caddress_intro ° caddress_elim = id *)
  check_id "Addresses" (fun t -> caddress_intro (caddress_elim (stackvar_elim t))) raw_ast
  
  (* test3: check if decode ° encode = id, where encode applies stackvar_elim followed by caddress_intro *)
  (* check_id "Round trip" (cfeatures_intro cfeatures elim) raw_ast; *)


let _ = test_encodings ()


let _ = Run.script_cpp ~raw_ast:false (* ~filename:"c_ast.cpp" *) (fun () ->

  !! Trace.apply stackvar_elim;
  !! Trace.apply stackvar_intro;
  
  (* !! Trace.apply (check_id "roundtrip_stackvar" (fun t -> stackvar_intro (stackvar_elim t))); *)

  (* Note: address_elim might not work in the presence of stack variables *)
  !! Trace.apply stackvar_elim;

  !! Trace.apply caddress_elim;
  !! Trace.apply caddress_intro;
  (* !! Trace.apply (check_id "roundtrip_address" (fun t -> caddress_intro (caddress_elim t))); *)
)
