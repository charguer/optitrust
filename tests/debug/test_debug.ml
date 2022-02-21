open Optitrust
open Target
open Ast
open Ast_fromto_AstC


let _ = Flags.dump_ast_details := true


let _ = Run.script_cpp (fun _ ->
  
  
  (* !! Trace.apply infix_elim ; *)
  (* !! Trace.apply stackvar_elim; *)
  (* !! Trace.apply caddress_elim; *)

  !! Struct.set_explicit [sInstr "b = a"];
  !! Variable.reuse ~space:((expr "v->x")) [cVarDef "y"];

)
