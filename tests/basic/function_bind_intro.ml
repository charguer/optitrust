open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Function_basic.bind_intro ~fresh_name:"s" [cFun "h"];
  (* same with a mark *)
  let my_mark = "__my_mark" in
  !! Function_basic.bind_intro ~my_mark ~fresh_name:"r" [cFun "g"];
  !! Marks.remove my_mark [cMark my_mark];
)
