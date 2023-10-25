open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 2 [cVarDef "a"];
  !! Sequence_basic.intro_between [tBefore; cVarDef "d1"] [tAfter; cVarDef "d3"];
  !! Sequence_basic.intro_after [cVarDef "f"];

)
