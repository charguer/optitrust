open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp ( fun _ ->
  (* 1. Parallel loops can be fusioned *)
  !! Loop_basic.fusion [cFunDef "parallel"; cFor ~body:[sInstr "t[i] +="] ""];
  !! Loop_basic.fusion [cFunDef "parallel"; cFor ~body:[cVarDef "y"] ""];
  !! Loop_basic.fusion [cFunDef "parallel"; cFor ~body:[sInstr "MFREE1(5, m1)"] ""];

  (* 2. Loops where instrs before/after split commute through all iterations can be fusioned. *)
  !! Loop_basic.fusion [cFunDef "commute"; cFor "i"; occFirst];
  !! Loop_basic.fusion [cFunDef "commute"; cFor "j"; occFirst];
  !! Loop_basic.fusion [cFunDef "commute"; cFor "k1"; occFirst];
  !! Loop_basic.fusion [cFunDef "commute"; cFor "k1"; occFirst];

  (* 3. Uninit tests. *)
  !! Loop_basic.fusion [cFunDef "uninit"; cFor "i"; occFirst];

  (* 3. Wrong fusions are rejected. *)
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop_basic.fusion [cFunDef "wrong_rw_rw"; cFor "i"; occFirst]);
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop_basic.fusion [cFunDef "wrong_rw_ro"; cFor "i"; occFirst]);
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop_basic.fusion [cFunDef "wrong_ro_rw"; cFor "i"; occFirst]);

  (* TODO:
  !! Loop_basic.fission_basic [cFunBody "ghosts"; cFor "k"; tBefore];
  *)
)
