
open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Loop.reorder_at ~order:["c";"b"] [cFunBody "f1"; sInstr "y++"];
  !! Trace.failure_expected (fun () ->
    Loop.reorder_at ~order:["c";"b"] [cFunBody "f1_wrong"; sInstr "x += c"]);
)
