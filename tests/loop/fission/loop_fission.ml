open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp ( fun _ ->
  !! Loop.fission [cFunBody "pure"; cForBody ~body:[cVarDef "d"] "i"; tBetweenAll];
  !! Loop.fission ~nest_of:2 [cFunBody "pure"; nbMulti; cForBody ~body:[cVarDef "x"] "j"; tBetweenAll];
  !! Loop.fission ~nest_of:3 [cFunBody "pure"; nbMulti; tAfter; cFor ~body:[cVarDef "y"] "i"; cVarDef "b"];

  !! Loop.fission ~nest_of:3 [cFunBody "seq_ro_par_rw"; tAfter; sInstr " = x"];

  !! Loop.fission ~nest_of:2 [cFunBody "ghost_scope"; tAfter; sInstr "y = x"];
  !! Loop.fission [cFunBody "ghost_pure"; cLabel "split"; tBefore];

  (* no op on edges *)
  !! Loop.fission [cFunBody "edges"; cForBody "i"; tFirst];
  !! Loop.fission [cFunBody "edges"; cForBody "i"; tLast];
  (* error out of edges *)
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop.fission [cFunBody "edges"; cForBody "i"; dBefore (-1)]);
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Loop.fission [cFunBody "edges"; cForBody "i"; dAfter 2]);

)
