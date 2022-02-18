open Optitrust
open Target
open Ast

let addr (t : trm) : trm = 
  (* trm_address_of t *)t

let _ = Flags.dump_ast_details := true


let _ = Run.script_cpp (fun _ ->

  
  !! Variable.reuse ~space:((expr "v->x")) [cVarDef "y"];

)
