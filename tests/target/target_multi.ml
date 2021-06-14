open Optitrust 
open Target

let _ = Run.script_cpp (fun () ->
  
  (* One (ExpectedOne is the default) *)
  show [ cFor "j" ];

  (* Multi *)
  show [ cMulti; cFor "i" ];
  show [ cMulti; cCall "f" ]; (* This fails because there are two calls on f *)
  show [ cMulti; cFunDef "main"; cFor "i" ];

  (* Nb *)
  show [ cNb 0; cFunDef "main"; cFor "j" ]; (* Doesn't work properly! *)
  show [ cNb 1; cFunDef "main"; cFor "i" ];
  show [ cNb 2; cFor "i" ];

  (* Any *)
  show [ cAnyNb; cFunDef "main"; cFor "j" ];
  show [ cAnyNb; cFunDef "main"; cFor "i" ];
  show [ cAnyNb; cFor "i" ];

)


