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
  show [ nbMulti; cFunDef ""; dBody; cSeq (); cStrict; Constr_dir (Dir_nth 0); cStrict; cFor "" ];


  (* Directions and strictness *)

  show [ nbMulti; cFor "j"; sInstr ~substr:false "i++" ];
  show [ nbMulti; cFor "i"; cStrict; sInstr "i++" ]; (* should not match i++ inside loop on j *)
  show [ nbMulti; cFor "i"; cFor "j" ];
  show [ nbMulti; cFor "i"; dBody; cStrict; cFor "j" ];
  show [ nbMulti; cFor "i"; cStrict; cSeq (); cStrict; cFor "j" ];

  show [ nbEx 0; nbMulti; cFor "i"; cStrict; cFor "j" ];

  show [ cTopFun "main"; cStrict; cFor "i" ];
  show [ cTopFun "main"; cStrict; cFor "j" ];

  show [ cTopFun "main"; dThen ];
  show [ cTopFun "main"; dThen; cStrict; sInstr "j++" ];
  show [ cTopFun "main"; cFor "j"; cStrict; cIf (); dThen ];
  show [ cTopFun "main"; cFor "j"; cStrict; cIf (); dThen; cStrict; sInstr "j++" ];

  show [ nbMulti; sInstr "i++" ]


  (* Strictness *)
)


