open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 
  
  !! Generic.add_mark "my_mark1" [nbMulti; cFor "i"]; 
  show [cMark "my_mark1"];
  !! Generic.add_mark "my_mark2" [nbMulti; cFor "i"];
  !! Generic.remove_mark "my_mark1" [nbMulti; cFor "i"];
  !! Generic.add_mark "my_mark4" [nbMulti; cFor "i"];
  !! Generic.clear_marks [nbMulti; cFor "i"];
)
