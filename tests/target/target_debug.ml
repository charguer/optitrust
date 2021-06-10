open Optitrust open Run

let _ = run_unit_test (fun () ->
  (** There should be exactly one result to each of the commands;
      if it is not the case, we'll get an error. *)
  let show = Generic.target_between_show in (* TODO: probably you'll need target_between_show *)

  (* TODO: the implementation of target_show, when the target_struct features
     a target_relative that is not "TargetAt", should invoke the transformation
     "seq_insert" to insert an empty instruction (trm_unit, displayed just a semicolumn).
     and this empty instruction should be the one highlighted by target_show. *)


  (* show [ cFirst; cThen ]; *) (* beware, we'd like to interpret the then as a sequence here (?) *)
  (* show [ cFirst; cElse ]; *) (* Doesn't work *)

  (* Last *)
  
  (* Nested paths *)
  (* show [ cLast; cFor "i"; cThen ]; *)

)


