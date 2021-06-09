open Optitrust open Run

let _ = run_unit_test (fun () ->
  (* There should be exactly one result to each of the commands;
      if it is not the case, we'll get an error. *)
  let show = Generic.target_show in

  (* Loop in a function *)
  show [ cFunDef "main"; cFor "i" ];

  (* Loop in a loop *)
  show [ cFor "i"; cFor "i" ];

  (* Def in depth *)
  show [ cFunDef "f"; cFor "i"; cFor "i"; cVarDef "k" ];

  (* Top-level functions *)
  show [ cTopFun "f"; cVarDef "k" ];

  (* Loops immediately inside a function *)
  show [ cFunDef ""; cStrict; cFor "" ];
)


