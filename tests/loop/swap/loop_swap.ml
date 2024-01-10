open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Loop.swap_basic [cFunBody "g"; cFor "a"];
  !! Loop.swap_basic [cFunBody "g"; cFor "a"];
  !! Loop.swap_basic [cFunBody "g"; cFor "c"];
  (* TODO?
    This is a reorder_at?
    !! Loop.swap [cFunBody "g"; cFor "b"]; *)

  !! Trace.failure_expected
    (function | Scope_computation.InvalidVarId _ -> true | _ -> false) (fun () ->
    Loop.swap [cFunBody "g"; cFor "i"]);

  !! Loop.swap_basic [cFunBody "f"; cFor "x"];

  !! Loop.swap [cFunBody "seq_reads"; cFor "i"];

  !! Loop.swap [cFunBody "ghost_pairs"; cFor "j"];
  !! Loop.swap [cFunBody "ghost_pairs"; cFor "i"];
)
