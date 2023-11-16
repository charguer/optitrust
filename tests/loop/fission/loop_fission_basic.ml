open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp ( fun _ ->
  (* 1. Parallel loops can be fissioned, unless binders are broken. *)
  !! Trace.failure_expected (fun () ->
    Loop_basic.fission [tBefore; cFunDef "parallel"; sInstr "t[i] +="]);
  !! Loop_basic.fission [tAfter; cFunDef "parallel"; sInstr "t[i] +="];
  !! Loop_basic.fission [tBefore; cFunDef "parallel"; cVarDef "z"];
  !! Trace.failure_expected (fun () ->
    Loop_basic.fission [tBefore; cFunDef "parallel"; sInstr "MFREE1(5, m1)"]);
  !! Loop_basic.fission [tAfter; cFunDef "parallel"; sInstr "MFREE1(5, m1)"];

  (* 2. Loops where instrs before/after split commute through all iterations can be fissioned. *)
  !! Loop_basic.fission [cFunDef "commute"; cFor "i"; dBody; dAfter 0];
  !! Loop_basic.fission [cFunDef "commute"; cFor "j"; dBody; dAfter 0];
  !! Loop_basic.fission [cFunDef "commute"; cFor "k1"; tAfter; sInstr "x += 1"];
  !! Loop_basic.fission [cFunDef "commute"; cFor "k1"; tBefore; sInstr "z += 1"];

  (* 3. Uninit tests. *)
  !! Loop_basic.fission [tAfter; cFunDef "uninit"; sInstr "t[i] = i"];

  (* 3. Wrong fissions are rejected. *)
  !! Trace.failure_expected (fun () ->
    Loop_basic.fission [cFunDef "wrong_rw_rw"; cFor ""; dBody; dAfter 0]);
  !! Trace.failure_expected (fun () ->
    Loop_basic.fission [cFunDef "wrong_rw_ro"; cFor ""; dBody; dAfter 0]);
  !! Trace.failure_expected (fun () ->
    Loop_basic.fission [cFunDef "wrong_ro_rw"; cFor ""; dBody; dAfter 0]);

  (* Testing fission with multiple targets *)
  !! Loop_basic.fission [cFunDef "testAllInstr"; cForBody "i"; tBetweenAll];
  (* ~indices:[2;3] *)
  !! Loop_basic.fission [cFunDef "testAllInstr2"; cForBody "i"; tBefore; cOr [
    [dSeqNth 2];
    [dSeqNth 3]
  ]];

  (* ~indices: *)
  !! Loop_basic.fission [cFunDef "testAllInstrContracts"; cForBody "i"; tBefore; cOr [
    [dSeqNth 2];
    [dSeqNth 4]
  ]];
)
