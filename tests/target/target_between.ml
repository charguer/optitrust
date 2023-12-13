open Optitrust
open Target

let _ = Flags.keep_marks_added_by_target_show := true

let _ = Run.script_cpp (fun () ->
  (* There should be exactly one result to each of the commands;
      if it is not the case, we'll get an error. *)

  (* Before *)
  show [ tBefore; cVarDef "r1" ];
  show [ tBefore; cVarDef "r2" ];
  show [ tBefore; cVarDef "m1" ];
  show [ tBefore; cVarDef "m2" ];

  (* After *)
  show [ tAfter; cVarDef "r1" ];
  show [ tAfter; cVarDef "r2" ];
  show [ tAfter; cVarDef "m1" ];
  show [ tAfter; cVarDef "m2" ];

  (* First *)
  show [ tFirst; cFor "i"; dBody ];
  show [ tFirst; cIf(); dElse ];

  (* Last *)
  show [ tLast; cIf(); dElse ];
  show [ tLast; cFor "i"; dBody];
  show [ tLast; cIf(); dThen ];
  show [ tLast; cIf(); dElse ];

  (* Nested paths *)
  show [ tLast; cFor"i"; cIf() ;dThen];
  (* Top level paths *)
  show [tBefore; cTopFunDef "main"];
  show [tAfter; cTopFunDef "main"];

)
