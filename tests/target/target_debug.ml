open Optitrust open Run

let _ = run_unit_test (fun () ->
  (* There should be exactly one result to each of the commands;
      if it is not the case, we'll get an error. *)
  let show = Generic.target_between_show in



  Generic.target_show [cMulti;cVarDef "r1";cBody];
  (* Generic.target_show [cMulti;cFor "i";cBody]; *)

  show [ cFirst; cFor "i"; cBody ]; (* beware, we'd like to interpret the loop as a sequence here (?) *)
  show [ cFirst; cThen ]; (* beware, we'd like to interpret the then as a sequence here (?) *)
  show [ cFirst; cElse ];

  (* Last *)
  show [ cLast; cFor "i" ];

  (* Nested paths *)
  show [ cLast; cFor "i"; cThen ];

)


