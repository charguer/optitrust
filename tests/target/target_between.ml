open Optitrust
open Target

let aim tg = 
  if Flags.get_exit_line() <> None then Marks.clean [dRoot];
  Marks.add_between (Mark.next()) tg

let _ = Run.script_cpp (fun () ->
  (** There should be exactly one result to each of the commands;
      if it is not the case, we'll get an error. *)

  (* Before *)
  !! aim [ tBefore; cVarDef "r1" ];
  !! aim [ tBefore; cVarDef "r2" ];
  !! aim [ tBefore; cVarDef "m1" ];
  !! aim [ tBefore; cVarDef "m2" ];

  (* After *)
  !! aim [ tAfter; cVarDef "r1" ];
  !! aim [ tAfter; cVarDef "r2" ];
  !! aim [ tAfter; cVarDef "m1" ];
  !! aim [ tAfter; cVarDef "m2" ];

  (* First *)
  !! aim [ tFirst; cFor "i"; dBody ];
  !! aim [ tFirst; cIf(); dElse ];

  (* Last *)
  !! aim [ tLast; cIf(); dElse ];
  !! aim [ tLast; cFor "i"; dBody];
  !! aim [ tLast; cIf(); dThen ];
  !! aim [ tLast; cIf(); dElse ];

  (* Nested paths *)
  !! aim [ tLast; cFor"i"; cIf() ;dThen];
  (* Top level paths *)
  !! aim [tBefore; cTopFunDef "main"];
  !! aim [tAfter; cTopFunDef "main"];
)


