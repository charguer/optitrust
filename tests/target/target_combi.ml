open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  (* TODO: replace nbMulti with nbExact *)

  (* Loop in a function *)
  show [ cFunDef "main"; cFor "i" ];

  (* Loop in a loop *)
  show [ cFor "i"; cFor "j" ];

  (* Def in depth *)
  show [ cFunDef "f"; cFor "i"; cFor "j"; cVarDef "k" ];

  (* Top-level functions *)
  show [ cTopFunDef "f"; cVarDef "k" ];

  (* Mutliple *)
  show [ nbMulti; cFunDef "f"; sInstr "j++" ];

  show [ nbMulti; cFun "f" ];

  (* Inside loop bodies *)
  show [ nbMulti; cFor "j"; sInstr "i++" ];

  show [ nbMulti; cFor "i"; cFor "j" ];

  show [ nbMulti; cFunDef ""; dBody; cStrict; cFor "" ];

  show [ nbAny; cFor "i"; sExpr "i++" ];

  show [ nbAny; cFor "i"; dStep; sInstr "i++" ];

  show [ nbAny; cFor "i"; sInstr "i++" ];

  show [ nbAny; cFor "i"; dBody; sInstr "i++" ];

  show [ nbMulti; cFor "i"; dBody; cStrict; cFor "j" ];

  show [ cFor "i"; cStrict; cFor "k" ];

  show [ nbExact 3; cTopFunDef "main"; cFor "" ];

  show [ nbExact 2; cTopFunDef "main"; cStrict; cFor "" ];

  show [ nbExact 3; cTopFunDef "main"; dBody; cFor "" ];

  show [ cTopFunDef "main"; dBody; cFor "i" ];

  show [ cTopFunDef "main"; cStrict; cFor "j" ];

  show [ cTopFunDef "main"; cIf (); dCond];

  show [ cTopFunDef "main"; cThen ];

  show [ cTopFunDef "main"; cThen; sInstr "j++"];

  show [ cTopFunDef "main"; cThen; cStrict; sInstr "j++" ];

  show [ cTopFunDef "main"; cFor "j"; cIf (); dThen ];

  show [ cTopFunDef "main"; cFor "j"; cStrict; cIf (); dThen; cStrict; sInstr "j++" ];

  show [ sInstr "i++" ];

  show [ cTopFunDef "main"; cFor "i"; cForNestedAtDepth 0 ];
  show [ cTopFunDef "main"; cFor "i"; cForNestedAtDepth 1 ];

)
