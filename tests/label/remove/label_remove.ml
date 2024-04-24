open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->

  !! Label_basic.remove [cLabel "start"];
  !! List.iter (fun l -> Label_basic.remove [cLabel l]) ["loop";"cond";"incr_1";"incr_2"];
  !! Trace.failure_expected (fun _e -> true) (fun () ->
            Label_basic.remove [cLabel "foo"];)

)
