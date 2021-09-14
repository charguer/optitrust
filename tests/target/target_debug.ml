open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  
  show [nbMulti; cPrimFun (Prim_binop Binop_set)];
  (* show [sInstr "t[i] ="; dRHS]; *)
  (* show [sInstr "t[i] ="; dLHS]; *)
  (* show [nbMulti;cLabel "__TEMP_LABEL";dBody; cSeq ()];   *)
  (* TODO ARTHUR: report the "show" in the AST not in the diff. *)
)
