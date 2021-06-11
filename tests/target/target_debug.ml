open Optitrust
open Target

let _ = Run.script_cpp (fun () ->



  (* Top-level functions *)
  show [ cTopFun "f"; cVarDef "k" ]; (* TODO: cTopFun is not working properly *)
  
  show [ cMulti; cFunDef ""; cFor "" ]; (* cStrict is not working properly *)
 

  (* Loops immediately inside a function *)
  (* TODO: see the 4 solutions *)

  (* TODO: add tests using cStrict *)
)


