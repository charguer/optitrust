open Optitrust
open Prelude
let _ = Flags.check_validity := false

let _ = Run.script_cpp (fun _ ->



  !! Variable_basic.bind "k" [cForBody "i"; cWrite ~lhs:[cVar "c"] (); cBinop ~lhs:[cVar "i"] Binop_add ];
  !! Loop.fold_instrs ~index:"k" [sInstr "c[k] +="];

  ;)
