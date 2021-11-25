open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Function_basic.beta [cFun "f"];
  show [cFunDef ""];

  !! Function.beta ();
)
