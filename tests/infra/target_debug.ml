open Optitrust


let _ = run_unit_test (fun () ->
  let show = show_target in
  set_repeat_io false;
  show [cInstr "v2 = v"];
  (* show [ cAnyNb; cSeq ~args:[cTrue;cTrue;cTrue]() ]; *)
  (* show [ cMulti; cFunDef "f" ]; *)
  (*  show [ cTopFun "f" ];*)
  (*    show [ cTopFun "main" ];*)
)
