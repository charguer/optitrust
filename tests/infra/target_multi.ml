open Optitrust

let _ = run_unit_test (fun () ->
  let show = Tr.target_show in

  (* One (ExpectedOne is the default) *)
  show [ cFor "j" ];

  (* Multi *)
  show [ cMulti; cFor "i" ];
  show [ cMulti; cFun "main"; cFor "i" ];

  (* Nb *)
  show [ cNb 0; cFun "main"; cFor "j" ];
  show [ cNb 1; cFun "main"; cFor "i" ];
  show [ cNb 2; cFor "i" ];

  (* Any *)
  show [ cAnyNb; cFun "main"; cFor "j" ];
  show [ cAnyNb; cFun "main"; cFor "i" ];
  show [ cAnyNb; cFor "i" ];

)


