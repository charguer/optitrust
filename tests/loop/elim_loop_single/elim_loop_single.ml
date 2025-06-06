open Optitrust
open Prelude
let _ =
  Run.script_cpp (fun _ ->
    !! Loop.elim_loop_single [cFunBody "main"; cFor "i"];
    !! Loop.elim_loop_single [cFunBody "main2"; cFor "i"];
    !! Trace.failure_expected
        (fun _e -> true)
        (fun _ -> Loop.elim_loop_single [cFunBody "main3"; cFor "i"];)
  )
