open Optitrust

let _ = run_unit_test (fun () ->
  let show = Tr.target_show in

  (* One (ExpectedOne is the default) *)
  show [ cFor "j" ];

  (* Multi *)
  show [ cMulti; cFor "i" ];
  show [ cMulti; cFunDef "main"; cFor "i" ];

  (* Nb *)
  show [ cNb 0; cFunDef "main"; cFor "j" ];
  show [ cNb 1; cFunDef "main"; cFor "i" ];
  show [ cNb 2; cFor "i" ];

  (* Any *)
  show [ cAnyNb; cFunDef "main"; cFor "j" ];
  show [ cAnyNb; cFunDef "main"; cFor "i" ];
  show [ cAnyNb; cFor "i" ];

)


