open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   !!  Matrix.delocalize "deposit" ~into:"depositCorners"
      ~acc:"sum" ~ops:(Ast.Local_arith (Lit_float 0., Binop_add))
      ~dim:(var "nbCorners") ~index:"idCorner" ~indices:["idCell"]
      ~alloc_instr:[cVarDef "deposit"] [cFor "idCell"];
  )
