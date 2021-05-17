open Optitrust


let _ = run_unit_test (fun () ->
  let show = show_target in
  set_repeat_io false;
  (* show [cVarDef "r2"] ~debug_ast:true; *)
  show [cMulti; cVar "r2"];
  (* show [ cAnyNb; cSeq ~args:[cTrue;cTrue;cTrue]() ]; *)
  (* show [ cMulti; cFunDef "f" ]; *)
(*  show [ cTopFun "f" ];*)
(*    show [ cTopFun "main" ];*)
)
