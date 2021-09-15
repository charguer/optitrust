open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 
  
  !! Generic.add_mark "M1" [nbMulti; cFor "i"]; 
  !! Generic.add_mark "M2" [nbMulti; cFor "i"];
  !! Generic.add_mark "M3" [nbMulti; cFunDef "main"; cFor "i"];
  !! Generic.remove_mark "M1" [nbMulti; cFor "i"];
  !! Generic.add_mark "M4" [nbMulti; cFor "i"];
  !! Generic.clear_marks [nbMulti; cFor "i"];
)
