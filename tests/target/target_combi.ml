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
  show [ cMulti; cFunDef ""; cStrict; cBody; cStrict; cSeq(); cStrict; Constr_dir (Dir_nth 0); cStrict; cFor "" ];


  (* Directions and strictness *)

  show [ cMulti; cFor "i"; cInstr "i++" ];
  show [ cMulti; cFor "i"; cStrict; cInstr "i++" ]; (* should not match i++ inside loop on j *)
  show [ cMulti; cFor "i"; cFor "j" ];
  show [ cMulti; cFor "i"; cBody; cStrict; cFor "j" ];
  show [ cMulti; cFor "i"; cStrict; cSeq; cStrict; cFor "j" ];

  show [ cNb 0; cMulti; cFor "i"; cStrict; cFor "j" ];
  (* beware that this is not working, due to the intermediate cSeq *)

  show [ cTopFun "main"; cStrict; cFor "i" ];
  show [ cTopFun "main"; cStrict; cFor "j" ];

  show [ cTopFun "main"; cThen ];
  show [ cTopFun "main"; cThen; cStrict; cInstr "j++" ];
  show [ cTopFun "main"; cFor "j"; cStrict; cIf; dThen ];
  show [ cTopFun "main"; cFor "j"; cStrict; cIf; dThen; cStrict; cInstr "j++" ];

  show [ cMulti; cInstr "i++" ]


  (* Strictness *)
)


