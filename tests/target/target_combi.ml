open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  (* Loop in a function *)
  show [ cFunDef "main"; cFor "i" ];

  (* Loop in a loop *)
  show [ cFor "i"; cFor "j" ];

  (* Def in depth *)
  show [ cFunDef "f"; cFor "i"; cFor "j"; cVarDef "k" ];

  (* Top-level functions *)
  show [ cTopFun "f"; cVarDef "k" ];

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
  show [ nbExact 3; cTopFun "main"; cFor "" ];

  show [ nbExact 2; cTopFun "main"; cStrict; cFor "" ];
  show [ nbExact 2; cTopFun "main"; dBody; cStrict; cFor "" ];
  show [ cTopFun "main"; dBody; cStrict; cFor "i" ];
  show [ cTopFun "main"; cStrict; cFor "j" ];

  show [ cTopFun "main"; cThen ];
  show [ cTopFun "main"; cThen; sInstr "j++"];
  show [ cTopFun "main"; cThen; cStrict; sInstr "j++" ];
  show [ cTopFun "main"; cFor "j"; cIf (); dThen ];
  show [ cTopFun "main"; cFor "j"; cStrict; cIf (); dThen; cStrict; sInstr "j++" ];

  show [ sInstr "i++" ]

)


