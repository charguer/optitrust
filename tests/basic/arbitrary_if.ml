open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Generic.arbitrary_if ~single_branch:true "x > 0" [sInstr "x = 5"];
)