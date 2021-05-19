open Optitrust
let _ = Printexc.record_backtrace true

let _ = run_unit_test (* FOR DEBUG: *)~ast_decode:false   (fun () ->
  let show = show_target in
  set_repeat_io false;

  (* show [cVarDef "x"]; *)

  Debug.backtrace (fun () ->
    show [cMulti; cVar "x" ];);

  dump();

  (* show [ cTypDef "intstar" ]; *)
  (* show [ cMulti; cFunDef "f" ]; *)
  (*  show [ cTopFun "f" ];*)
  (*    show [ cTopFun "main" ];*)
)
