
open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  (* TODO: minimize contracts *)
  !! Loop.reorder_at ~order:["c";"b"] [cFunBody "f1"; cForBody "c"; dSeqNth 0];
  !! Loop.reorder_at ~order:["k";"i";"j"] [cFunBody "f2"; cForBody "k"; sInstr "sum +="];
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop.reorder_at ~order:["c";"b"] [cFunBody "f1_wrong"; sInstr "x += c"]);
)
