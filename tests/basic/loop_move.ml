open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  Loop.move "by" ~after:"cx";
)