open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.collapse [cFunDef "consts"; cFor "i"];
  !! Loop_basic.collapse [cFunDef "from_zero"; cFor "i"];
  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Loop_basic.collapse [cFunDef "from_zero"; cFor "i"]);
)
