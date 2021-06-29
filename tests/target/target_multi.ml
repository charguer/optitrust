open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  (* One (ExpectedOne is the default) *)
  show [ cForSimple "j" ];

  (* Multi *)
  show [ nbMulti; cForSimple "i" ];
  show [ nbMulti; cCall "f" ];
  show [ nbMulti; cFunDef "main"; cForSimple "i" ];

  (* Nb *)
  show [ nbEx 0; cFunDef "main"; cForSimple "j" ]; (* zero match *)
  show [ nbEx 1; cFunDef "main"; cForSimple "i" ];
  show [ nbEx 2; cForSimple "i" ];

  (* Any *)
  show [ nbAny; cFunDef "main"; cForSimple "j" ]; (* zero match *)
  show [ nbAny; cFunDef "main"; cForSimple "i" ];
  show [ nbAny; cForSimple "i" ];

)


