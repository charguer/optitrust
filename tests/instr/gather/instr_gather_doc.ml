open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Instr.(gather_targets ~dest:GatherAtFirst) [cVarDef ~regexp:true "a."];

)
