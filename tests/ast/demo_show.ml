open Optitrust
open Prelude

let has_stackvar (t : trm) : bool =
  Trm.trm_get_cstyles t = [Stackvar]

let _ = Run.script_cpp ~capture_show_in_batch:true (fun () ->

  !! Show.ast ~msg:"AST1" ();
  !! ShowAt.ast ~msg:"AST2" [];
  !! ShowAt.trm ~msg:"for trm" [cFor "i"];
  !! ShowAt.desc ~msg:"desc" [cFor "i"];
  !! ShowAt.typ ~msg:"typ" [nbAny; cVar "x"]; (* cInContracts *)
  !! Marks.add "mymark1" [cFor "i"; cVar "x"];
  !! Marks.add "mymark2" [cFor "i"; cVar "x"];
  !! ShowAt.marks ~msg:"marks" [cFor "i"; cVar "x"];
  !! ShowAt.cstyle ~msg:"cstyle-item" [nbAny; Constr_pred has_stackvar];
  !! ShowAt.annot ~msg:"annot" [dRoot];

)
