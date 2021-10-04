open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  !! Marks.add "_my_mark" [cFor "i"];
  show [cMark "_my_mark"];

)
