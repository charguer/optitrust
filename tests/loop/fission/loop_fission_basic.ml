open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp ( fun _ ->

  !! Loop_basic.fission [tAfter; cFunDef "test"; sInstr "t[i] +="];

  !! Loop_basic.fission [tBefore; cFunDef "test"; cVarDef "z"];

  !! Loop_basic.fission_all_instrs [cFunDef "testAllInstr"; cFor "i"];
  !! Loop_basic.fission_all_instrs ~indices:[2;3] [cFunDef "testAllInstr2"; cFor "i"];

  !! Loop_basic.fission_all_instrs ~indices:[2;4] [cFunDef "testAllInstrContracts"; cFor "i"];


)
