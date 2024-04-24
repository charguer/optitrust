open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  (* for variables *)
  !! Variable_basic.inline [cVarDef "a"];

  (* for functions *)
  (* FIXME *)
  (*!! Variable_basic.inline [cFunDef "f"];*)

  (* tranformation fails for non const variables *)
  (* FIXME *)
  (*Trace.failure_expected (fun _e -> true) (fun () ->
    Variable_basic.inline [cVarDef "c"];)
  *)
)
