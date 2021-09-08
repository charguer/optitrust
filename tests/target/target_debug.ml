open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  show [nbMulti; cFunDef "main"; dBody; dNth 0];
  (* show [nbMulti;cLabel "__TEMP_LABEL";dBody; cSeq ()];   *)
  (* TODO ARTHUR: report the "show" in the AST not in the diff. *)
)
