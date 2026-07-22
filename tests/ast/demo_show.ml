open Optitrust
open Prelude

let has_reference (t : trm) : bool =
  Trm.trm_get_cstyles t = [Reference]

let _ = Run.script_cpp ~capture_show_in_batch:true (fun () ->
  !! Show.At.trm ~msg:"AST" [];
  (*!! Show.At.trm ~msg:"for trm" [cFor "i"]; --> need decoding of nonroot*)
  (* TODO: ensure a deterministic printing of identifiers
  !! Show.At.(trm ~style:(Style.internal ())) ~msg:"var-trm-internal" [cVarDef "x"];
  !! Show.At.(trm ~style:(Style.internal ())) ~msg:"for-trm-internal" [cFor "i"];
  *)
  (* too verbose
     !! Show.At.(trm ~style:(Style.internal_ast ())) ~msg:"for-trm-internal-ast" [cFor "i"]; *)
  !! Show.At.(trm ~style:(Style.internal_ast_only_desc ())) ~msg:"for-trm-internal-desc" [cFor "i"];
  !! Show.At.desc ~msg:"desc" [cFor "i"];
  !! Show.At.typ ~msg:"typ" [nbAny; cVar "x"]; (* cInContracts *)
  !! Marks.add "mymark1" [cForBody "i"; cVar "x"];
  !! Marks.add "mymark2" [cForBody "i"; cVar "x"];
  !! Show.At.marks ~msg:"marks" [cForBody "i"; cVar "x"];
  !! Show.At.cstyle ~msg:"cstyle-item" [nbAny; Constr_pred has_reference];
  !! Show.At.annot ~msg:"annot" [dRoot];

)
