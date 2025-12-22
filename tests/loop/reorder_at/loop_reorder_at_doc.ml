
open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Loop.reorder_at ~order:["c";"b"] [cFunBody "f1"; cForBody "c"; dSeqNth 0];
)
