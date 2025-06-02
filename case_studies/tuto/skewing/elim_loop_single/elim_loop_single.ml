open Optitrust
open Prelude
(** [elim_loop_single_on t]: Reverse the transformation loop_single_on
    Expects a trm_for
    Transform:
      for(int i = k; i <k + 1 ; k++){
      tl
      }
    into:
      int i = k;
      tl
      *)
let elim_loop_single_on t : trm =
  let error = "elim_loop_single_on: Expect the target to point to a for loop" in
  let l_range, body, _contract = trm_inv ~error trm_for_inv_instrs t in
  if l_range.direction <> DirUp then
    trm_fail t "elim_loop_single_on: Expect the direction to be DirUp";
  if not (are_same_trm l_range.step (trm_step_one ())) then
    trm_fail t "elim_loop_single_on: Expect a step of one for the loop";
  if not (are_same_trm (trm_add_int l_range.start (trm_int 1)) l_range.stop)
  then trm_fail t "elim_loop_single_on: Expected a loop with a unique step";
  let index = trm_let (l_range.index, typ_int) l_range.stop in
  trm_seq (Mlist.push_front index body)

let elim_loop_single (tg : target) =
  apply_at_target_paths elim_loop_single_on tg;
  Rewrite.compute_inside []

let _ =
  Run.script_cpp (fun () ->
      !!elim_loop_single [ cFunBody "main"; cFor "k" ];
      !!Trace.failure_expected
        (fun _e -> true)
        (fun _ -> elim_loop_single [ cFunBody "main2"; cFor "k" ]))
