open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

   
   let alloc_instr = [cTopFunDef "allocate"; cWriteVar "b"] in 
   !! Matrix.delocalize "b" ~into:"y" ~init_zero:true ~acc_in_place:false  ~acc:"sum" ~dim:(var "N0") ~index:"i0" ~ops:(Local_arith (Lit_int 0, Binop_add)) ~alloc_instr ~labels:["alloc";"";"dealloc"] [cFor "j"];

   
)

