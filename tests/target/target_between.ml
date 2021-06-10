open Optitrust open Target

let _ = Run.script_cpp (fun () ->
  (** There should be exactly one result to each of the commands;
      if it is not the case, we'll get an error. *)
  let show = Generic.target_between_show in (* TODO: probably you'll need target_between_show *)

  (* Before *)
  show [ cBefore; cVarDef "r1" ];
  show [ cBefore; cVarDef "r2" ];
  show [ cBefore; cVarDef "m1" ];
  show [ cBefore; cVarDef "m2" ];

  (* After *)
  show [ cAfter; cVarDef "r1" ];
  show [ cAfter; cVarDef "r2" ];
  show [ cAfter; cVarDef "m1" ];
  show [ cAfter; cVarDef "m2" ];

  (* First *)
  show [ cFirst; cFor "i"; cStrict; cBody ]; (* beware, we'd like to interpret the loop as a sequence here (?) *)
  show [ cFirst; cThen ]; (* beware, we'd like to interpret the then as a sequence here (?) *)
  show [ cFirst; cElse ];

  (* Last *)
  show [ cLast; cFor "i"; cStrict; cBody];
  show [ cLast; cThen ];
  show [ cLast; cElse ];

  (* Nested paths *)
  show [ cLast; cFor "i"; cBody; cThen ];

)


