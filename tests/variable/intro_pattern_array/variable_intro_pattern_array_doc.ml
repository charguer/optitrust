open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable.intro_pattern_array ~pattern_vars:"int k" ~pattern:"2 * k" [nbMulti; cWrite(); dRHS];

)
