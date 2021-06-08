open Optitrust open Run

let _ = Printexc.record_backtrace true

let _ = run_unit_test (* FOR DEBUG:~ast_decode:false *)   (fun () ->
  let show = show_target in
  show [ cMulti;cVar "r2" ]; (* Doesn't work properly*)
  (* show [ cVar "r2" ]; Doesn't work properly *)
  (* show [ cVar "f" ]; Doesn't work properly *)
  (* show [ cVar "g" ]; Doesn't work properly *)

  (* Loops *)
  (* show [ cFor "i" ]; *)
  (* show [ cFor "j" ]; *)
  (* show [ cFor ~cond:[cInstr "j < 5"] "" ]; *)

  (* Abort *)
  (* show [ cBreak ]; *)
  (* show [ cContinue ]; *)
  (* show [ cReturn ]; *)

  (* Labels *)
  (* show [ cLabel "lbl1" ]; *)
  (* show [ cLabel "lbl2" ]; *)

  (* Calls *)
  (* show [ cCall "f" ]; *)
  (* show [ cCall ~args:[cInt 2] "" ]; *)

  (* Var/Fun definitions *)
  (* show [ cFunDef "main" ]; *)
  (* show [ cFunDef "f" ]; *)
)
