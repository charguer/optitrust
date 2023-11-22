open Optitrust
open Prelude

let has_stackvar (t : trm) : bool =
  Trm.trm_get_cstyles t = [Stackvar]

let _ = Run.script_cpp ~capture_show_in_batch:true (fun () ->
  !! ShowAt.trm ~msg:"AST" [];
  (*!! ShowAt.trm ~msg:"for trm" [cFor "i"]; --> need decoding of nonroot*)
  !! ShowAt.(trm ~style:Internal) ~msg:"var-trm-internal" [cVarDef "x"];
  !! ShowAt.(trm ~style:Internal) ~msg:"for-trm-internal" [cFor "i"];
  (* too verbose
     !! ShowAt.(trm ~style:InternalAst) ~msg:"for-trm-internal-ast" [cFor "i"]; *)
  !! ShowAt.(trm ~style:InternalAstOnlyDesc) ~msg:"for-trm-internal-desc" [cFor "i"];
  !! ShowAt.desc ~msg:"desc" [cFor "i"];
  !! ShowAt.typ ~msg:"typ" [nbAny; cVar "x"]; (* cInContracts *)
  !! Marks.add "mymark1" [cForBody "i"; cVar "x"];
  !! Marks.add "mymark2" [cForBody "i"; cVar "x"];
  !! ShowAt.marks ~msg:"marks" [cForBody "i"; cVar "x"];
  !! ShowAt.cstyle ~msg:"cstyle-item" [nbAny; Constr_pred has_stackvar];
  !! ShowAt.annot ~msg:"annot" [dRoot];

)
