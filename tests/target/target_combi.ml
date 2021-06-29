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

  (* Loops immediately inside a function *) (* TODO: ARTHUR: think about how to fix this *)
  show [ nbMulti; cFunDef ""; dBody; ];
  (*show [ nbMulti; cFunDef ""; dBody; cSeq (); cStrict; dNth 0; cStrict; cFor "" ];*)


  (* Directions and strictness *)

  show [ nbMulti; cFor "j"; sInstr "i++" ];
  show [ nbAny; cFor "i"; cStrict; sInstr "i++" ]; (* can't match anymore on the full comonents of the loops *)
  show [ nbMulti; cFor "i"; cFor "j" ];
  show [ nbMulti; cFor "i"; cStrict; dBody; cFor "j" ];
  (* show [ nbMulti; cFor "i"; cStrict; cSeq (); cFor "j" ]; *) (* There is a problem when using cSeql and cStrict *)

  show [ nbEx 0; cFor "i"; cStrict; cFor "j" ];

  show [ cTopFun "main"; cStrict; cFor "i" ];
  show [ cTopFun "main"; cStrict; cFor "j" ];

  show [ cTopFun "main"; dThen ];
  show [cTopFun "main"; dThen; sInstr "j++"];
  show [ cTopFun "main"; dThen; cStrict; cSeq();sInstr "j++" ];
  show [ cTopFun "main"; cFor "j"; dBody; cIf ();dThen]; (* dThen ]; *)
  show [ cTopFun "main"; cFor "j"; cStrict; cIf (); dThen; cStrict; sInstr "j++" ];

  show [ nbMulti; sInstr "i++" ]


  (* Strictness *)
)


