open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  
  !! Generic.add_mark "_my_mark" [cFor "i"];
  show [cMark "_my_mark"];
  
)
