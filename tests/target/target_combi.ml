open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  (* Loop in a function *)
  show [ cFunDef "main"; cForSimple "i" ];

  (* Loop in a loop *)
  show [ cForSimple "i"; cForSimple "j" ];

  (* Def in depth *)
  show [ cFunDef "f"; cForSimple "i"; cForSimple "j"; cVarDef "k" ];

  (* Top-level functions *)
  show [ cTopFun "f"; cVarDef "k" ];

  (* Loops immediately inside a function *) (* TODO: ARTHUR: think about how to fix this *)
  show [ nbMulti; cFunDef ""; dBody; ];
  (*show [ nbMulti; cFunDef ""; dBody; cSeq (); cStrict; dNth 0; cStrict; cForSimple "" ];*)


  (* Directions and strictness *)

  show [ nbMulti; cForSimple "j"; sInstr "i++" ];
  show [ nbAny; cForSimple "i"; cStrict; sInstr "i++" ]; (* can't match anymore on the full comonents of the loops *)
  show [ nbMulti; cForSimple "i"; cForSimple "j" ];
  show [ nbMulti; cForSimple "i"; cStrict; dBody; cForSimple "j" ];
  (* show [ nbMulti; cForSimple "i"; cStrict; cSeq (); cForSimple "j" ]; *) (* There is a problem when using cSeql and cStrict *)

  show [ nbEx 0; cForSimple "i"; cStrict; cForSimple "j" ];

  show [ cTopFun "main"; cStrict; cForSimple "i" ];
  show [ cTopFun "main"; cStrict; cForSimple "j" ];

  show [ cTopFun "main"; dThen ];
  show [cTopFun "main"; dThen; sInstr "j++"];
  show [ cTopFun "main"; dThen; cStrict; cSeq();sInstr "j++" ];
  show [ cTopFun "main"; cForSimple "j"; dBody; cIf ();dThen]; (* dThen ]; *)
  show [ cTopFun "main"; cForSimple "j"; cStrict; cIf (); dThen; cStrict; sInstr "j++" ];

  show [ nbMulti; sInstr "i++" ]


  (* Strictness *)
)


