open Optitrust open Run

let _ = Printexc.record_backtrace true

let _ = run_unit_test (* FOR DEBUG:~ast_decode:false *)   (fun () ->
  let show = show_target in

  (* Loops *)
  (* show [cFor ~body:[cInstr "i <"] "" ]; *)

  (* Abort *)

  (* Labels *)

  show [ cInstr "+= 2" ];
  (* show [ cExpr "j <" ];  *)
  (* show [ cExpr "vect v2" ]; Does not work *)
  (* show [ cStrFull "int r = 3;" ]; with or without the ; ? *)
  (* show [ cInstr "i++" ]; *)
  (* show [ cInstr "+=" ]; *)
  (* show [ c ~sub:false "+=" ]; *)
  (* show [ cRegexp "f\\(.\\)" ]; *)
)
