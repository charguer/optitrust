open Optitrust
let _ = Printexc.record_backtrace true

let _ = run_unit_test (* FOR DEBUG:~ast_decode:false *)   (fun () ->
  let show = show_target in
  set_repeat_io false;


  show [cMulti; cVar "x" ];

  (*show [ cExpr "j <" ]; (* Does not work *)*)
  (* show [ cExpr "vect v2" ]; (* Does not work *) *)


  (* show [ cStrFull "int r = 3;" ]; (* with or without the ; ? *) *)

  (* show [cVarDef "x"]; *)
(*
  Debug.backtrace (fun () ->
    show [cMulti; cVar "x" ];);
*)
  dump();

  (* show [ cTypDef "intstar" ]; *)
  (* show [ cMulti; cFunDef "f" ]; *)
  (*  show [ cTopFun "f" ];*)
  (*    show [ cTopFun "main" ];*)
)
