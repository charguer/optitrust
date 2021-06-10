open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  let show = Generic.target_show in

  (* Loop in a function *)

  (* Loop in a loop *)

  (* Def in depth *)

  (* Top-level functions *)

  (* Loops immediately inside a function *)
show [cMulti;cFunDef ""; cFor "" ]; (* cStrict is not working properly *)
)


