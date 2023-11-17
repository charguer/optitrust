open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->
  !! Show.ast ~msg:"AST1" ();
  !! ShowAt.ast ~msg:"AST2" [];
  !! ShowAt.trm ~msg:"for trm" [cFor "i"];
  !! ShowAt.desc ~msg:"desc" [cFor "i"];
  !! ShowAt.typ ~msg:"typ" [cVar "x"];
  !! Marks.add "mymark1" [cVar "x"];
  !! Marks.add "mymark2" [cVar "x"];
  !! ShowAt.marks ~msg:"marks" [cVar "x"];
  !! ShowAt.annot ~msg:"annot" [dRoot];
)
