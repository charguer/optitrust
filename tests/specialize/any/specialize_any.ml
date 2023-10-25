open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! Specialize_basic.any (lit "2") [sInstr "corners"; cAny];

  !! Specialize_basic.any (var "i") [cAny];

)
