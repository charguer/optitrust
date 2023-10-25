open Optitrust
open Target
open Prelude


let _ = Run.script_cpp (fun _ ->
    !! Variable.insert_and_fold ~typ:(ty "const int") ~name:"a" ~value:(expr "x*y") [tBefore; cVarDef "r"];
  )
