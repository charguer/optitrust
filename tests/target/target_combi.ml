open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  (* Loop in a function *)
  show [ cFunDef "main"; cFor "i" ];

  (* Loop in a loop *)
  show [ cFor "i"; cFor "j" ];

  (* Def in depth *)
  show [ cFunDef "f"; cFor "i"; cFor "j"; cVarDef "k" ];

  (* Top-level functions *)
  show [ cTopFun "f"; cVarDef "k" ];

  (* Loops immediately inside a function *) (* TODO: ARTHUR: think about how to fix this *)
  show [ cMulti; cFunDef ""; cStrict; cBody; cStrict; cSeq(); cStrict; Constr_dir (Dir_nth 0); cStrict; cFor "" ];

  (* TODO: add tests using cStrict *)
)


