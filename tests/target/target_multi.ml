open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  (* One (ExpectedOne is the default) *)
  show [ cFor "j" ];

  (* Multi *)
  show [ nbMulti; cFor "i" ];
  show [ nbMulti; cCall "f" ];
  show [ nbMulti; cFunDef "main"; cFor "i" ];

  (* Nb *)
  show [ nbExact 0; cFunDef "main"; cFor "j" ]; (* zero match *)
  show [ nbExact 1; cFunDef "main"; cFor "i" ];
  show [ nbExact 2; cFor "i" ];

  (* Any *)
  show [ nbAny; cFunDef "main"; cFor "j" ]; (* zero match *)
  show [ nbAny; cFunDef "main"; cFor "i" ];
  show [ nbAny; cFor "i" ];
  show [nbMulti;cAnd [[cReturn];[cTypDef "vect"];[cFunDef "main"; cFor "i"]]];

)


