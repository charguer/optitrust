open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Arith_basic.scale ~reparse:true (lit "4") [cReadVar "x"];

)
