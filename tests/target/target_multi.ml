open Optitrust
open Target

let _ = Flags.keep_marks_added_by_target_show := true

let _ = Run.script_cpp (fun () ->

  (* This is a demo for the [nb*] constraints, so we are purposely not precise about the number of occurrences *)

  (* One (ExpectedOne is the default) *)
  show [ cFor "j" ];

  (* Multi *)
  show [ nbMulti; cFor "i" ];
  show [ nbMulti; cCall "f" ];
  show [ nbMulti; cFunDef "main"; cFor "i" ];
  show [ nbMulti; cVarDef "s"; dBody; cStrict; cTrue ];

  (* NbExact*)
  show [ nbExact 0; cFunDef "main"; cFor "j" ]; (* zero match *)
  show [ nbExact 1; cFunDef "main"; cFor "i" ];
  show [ nbExact 2; cFor "i" ];

  (* Any *)
  show [ nbAny; cFunDef "main"; cFor "j" ]; (* zero match *)
  show [ nbAny; cFunDef "main"; cFor "i" ];
  show [ nbAny; cFor "i" ];

  (* Or constraint *)
  show [ nbExact 3; cOr [[cReturn]; [cTypDef "vect"]; [cFunDef "main"; cFor "i"]]];

  (* TopFun *)
  show [ nbExact 1; cTopFunDef "f" ];
  show [ nbExact 2; cOr[ [cTopFunDef "f"]; [cTopFunDef "main"] ]];

  (* Nested *)
  show [ nbMulti; cFor "" ];
)
