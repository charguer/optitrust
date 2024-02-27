open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  (* for variables *)
  !! Variable_basic.unfold ~at:[cVarDef "b"] [cVarDef "a"];

  (* TODO: Sort tests correctly between unfold and inline *)

  (* for references *)
  !! Variable_basic.inline ~delete_decl:false [cVarDef "e"];

  (* for functions *)
  (*!! Variable_basic.inline ~delete_decl:false [cFunDef "f"];*)

  (* failure for non const variables *)
  (*!! Trace.failure_expected (fun _e -> true) (fun () ->
      Variable_basic.inline ~delete_decl:false [cVarDef "c"];)*)
)
