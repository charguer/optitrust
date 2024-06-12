open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

    !! Arith_basic.shift ~reparse:true (lit "4") [cReadVar "x"];

)
