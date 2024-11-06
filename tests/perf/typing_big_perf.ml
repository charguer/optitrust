open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.report_exectime := true

let _ = Run.script_cpp (fun () ->

  !! (for i = 0 to 1000 do
     Typedef_basic.insert ("T" ^ string_of_int i) (Typedef_alias typ_int) [tBefore; cFunDef "mm1024"];
  done)

)

