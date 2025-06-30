open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp ( fun _ ->
     !! Loop_basic.fission_basic [cFunBody "ghost_pure_dep"; cLabel "split"; tBefore];
     Resources.ensure_computed ();
     (*
     !! Loop_basic.fission_basic [cFunBody "ghost_pure_nondep"; cLabel "split"; tBefore];

  (* 1. Parallel loops can be fissioned, unless binders are broken. *)
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop_basic.fission_basic [tBefore; cFunDef "parallel"; sInstr "t[i] +="]);
  !! Loop_basic.fission_basic [tAfter; cFunDef "parallel"; sInstr "t[i] +="];
  !! Loop_basic.fission_basic [tBefore; cFunDef "parallel"; cVarDef "z"];
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop_basic.fission_basic [tBefore; cFunDef "parallel"; sInstr "free(m1)"]);
  !! Loop_basic.fission_basic [tAfter; cFunDef "parallel"; sInstr "free(m1)"];

  (* 2. Loops where instrs before/after split commute through all iterations can be fissioned. *)
  !! Loop_basic.fission_basic [cFunDef "commute"; cFor "i"; dBody; dAfter 0];
  !! Loop_basic.fission_basic [cFunDef "commute"; cFor "j"; dBody; dAfter 0];
  !! Loop_basic.fission_basic [cFunDef "commute"; cFor "k1"; tAfter; sInstr "x += 1"];
  !! Loop_basic.fission_basic [cFunDef "commute"; cFor "k1"; tBefore; sInstr "z += 1"];

  (* 3. Uninit tests. *)
  !! Loop_basic.fission_basic [tAfter; cFunDef "uninit"; sInstr "t[i] = i"];

  (* 3. Wrong fissions are rejected. *)
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop_basic.fission_basic [cFunDef "wrong_rw_rw"; cFor ""; dBody; dAfter 0]);
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop_basic.fission_basic [cFunDef "wrong_rw_ro"; cFor ""; dBody; dAfter 0]);
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop_basic.fission_basic [cFunDef "wrong_ro_rw"; cFor ""; dBody; dAfter 0]);

  !! Loop_basic.fission_basic [cFunDef "empty"; cForBody "i"; tFirst];
  (* Testing fission with multiple targets *)
  !! Loop_basic.fission_basic [cFunDef "testAllInstr"; cForBody "i"; tBetweenAll];
  (* ~indices:[2;3] *)
  !! Loop_basic.fission_basic [cFunDef "testAllInstr2"; cForBody "i"; tBefore; cOr [
    [dSeqNth 2];
    [dSeqNth 3]
  ]];

  (* ~indices: *)
  !! Loop_basic.fission_basic [cFunDef "testAllInstrContracts"; cForBody "i"; tBefore; cOr [
    [dSeqNth 2];
    [dSeqNth 4]
  ]];

  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Loop_basic.fission_basic [cFunBody "ghosts"; cFor "k"; tBefore]);*)
)
