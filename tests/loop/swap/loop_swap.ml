open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Loop.swap_basic [cFunBody "demo_both_par"; cFor "i"];
  !! Loop.swap_basic [cFunBody "demo_outer_par"; cFor "i"];

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

  !! Loop.swap [cFunBody "par_reads"; cFor "i"];

  !! Loop.swap [cFunBody "indep_reads"; cFor "i"];

  (* FIXME: Cannot swap with a parallel focus, we need some kind of move out. *)
  (*!! Loop.swap [cFunBody "ghost_pairs"; cFor "i"];*)

  !! Loop.swap [cFunBody "ghost_pure"; cFor "i"];
)
