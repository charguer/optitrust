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
  show [ cTopFun "f"; cVarDef "k" ]; (* TODO: cTopFun is not working properly *)

  (* Loops immediately inside a function *)
  show [ cMulti; cFunDef ""; cFor "" ]; (* cStrict is not working properly *)
  
  (* TODO: add tests using cStrict *)
)


