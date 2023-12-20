open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  (* for variables *)
  !! Variable_basic.inline [cVarDef "a"];

  (* for functions *)
  !! Variable_basic.inline ~accept_functions:true [cFunDef "f"];

  (* if accept_functions is false then transformation will fail *)
  Trace.failure_expected (fun _e -> true) (fun () ->
     Variable_basic.inline [cFunDef "f"];);

  (* tranformation fails for non const variables *)
  Trace.failure_expected (fun _e -> true) (fun () ->
    Variable_basic.inline [cVarDef "c"];)

)
