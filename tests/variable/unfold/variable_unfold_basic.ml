open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  (* for variables *)
  !! Variable_basic.unfold ~at:[cVarDef "b"] [cVarDef "a"];

  (* for references *)
  !! Variable_basic.unfold [cVarDef "e"];

  (* for functions *)
  !! Variable_basic.unfold [cFunDef "f"];

  (* failure for non const variables *)
  !! Trace.failure_expected (fun () ->
            Variable_basic.unfold [cVarDef "c"];)

)
