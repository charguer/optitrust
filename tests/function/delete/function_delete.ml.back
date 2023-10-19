open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
   !! Function.delete [cFunDef "f"];
   !! Trace.failure_expected (fun () ->
     !! Function.delete [cFunDef "g"]);
)
