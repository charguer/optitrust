open Optitrust

let _ = run_unit_test (fun () ->
  (** There should be exactly one result to each of the commands;
      if it is not the case, we'll get an error. *)
  let show = Tr.target_between_show in (* TODO: probably you'll need target_between_show *)

  (* TODO: the implementation of target_show, when the target_struct features
     a target_relative that is not "TargetAt", should invoke the transformation
     "seq_insert" to insert an empty instruction (trm_unit, displayed just a semicolumn).
     and this empty instruction should be the one highlighted by target_show. *)

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
  show [ cFirst; cFor "i" ]; (* beware, we'd like to interpret the loop as a sequence here (?) *)
  show [ cFirst; cThen ]; (* beware, we'd like to interpret the then as a sequence here (?) *)
  show [ cFirst; cElse ];

  (* Last *)
  show [ cLast; cFor "i" ];
  show [ cLast; cThen ];
  show [ cLast; cElse ];

  (* Nested paths *)
  show [ cLast; cFor "i"; cThen ];

)


