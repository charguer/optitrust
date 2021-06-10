open Optitrust open Run

let _ = run_unit_test (fun () ->
  (* There should be exactly one result to each of the commands;
      if it is not the case, we'll get an error. *)
  let show = Generic.target_show in

  (* Loop in a function *)

  (* Loop in a loop *)

  (* Def in depth *)

  (* Top-level functions *)

  (* Loops immediately inside a function *)
show [cMulti;cFunDef ""; cFor "" ]; (* cStrict is not working properly *)
)


