open Optitrust
open Prelude

let _ = Flags.check_validity := false

let if_loop_switch_on (t : trm) : trm =
  let error = "if_loop_switch: Expected a for loop" in
  let l_range, body, contract = trm_inv ~error trm_for_inv t in
  let instrs, res = trm_inv trm_seq_inv body in
  let cond, then_, else_ = trm_inv trm_if_inv (Mlist.nth instrs 0) in
  let l_value, r_value = trm_inv trm_eq_inv cond in
  let l_value_var = trm_inv trm_var_inv l_value in

  if l_value_var <> l_range.index then
    trm_fail t
      "if_loop_switch: Expected same trm for loop index and if condition";
  let new_cond =
    trm_and
      (trm_ge ~typ:typ_int l_value l_range.start)
      (trm_le ~typ:typ_int l_value l_range.stop)
  in
  let new_if = trm_if new_cond then_ else_ in
  let new_lrange : loop_range =
    {
      index = l_range.index;
      start = r_value;
      stop = trm_add_int r_value  (trm_int 1) ;
      direction = DirUp;
      step = trm_step_one ();
    }
  in
  let new_instrs = Mlist.push_front new_if (Mlist.pop_front instrs) in
  trm_for new_lrange (trm_seq ?result:res new_instrs) ~contract:contract

let if_loop_switch (tg : target) = apply_at_target_paths if_loop_switch_on tg

let elim_loop_single_on t : trm =
  let error = "Final nam: Expect the target to point to a for loop" in
  let l_range, body, _contract = trm_inv ~error trm_for_inv_instrs t in
  if l_range.direction <> DirUp then
    trm_fail t "Final nam: Expect the direction to be DirUp";
  if not (are_same_trm l_range.step (trm_step_one ())) then
    trm_fail t "Final nam: Expect a step of one for the loop";
  if not (are_same_trm (trm_add_int l_range.start (trm_int 1)) l_range.stop)
  then trm_fail t "Final nam: Expected a loop with a unique step";
  let index = trm_let (l_range.index, typ_int) l_range.start in
  trm_seq (Mlist.push_front index body)

let elim_loop_single (tg : target) =
  apply_at_target_paths elim_loop_single_on tg

let _ =
  Run.script_cpp (fun _ -> Loop.reorder ~order:[ "k"; "i"; "j" ] [ cFor "i" ];
  if_loop_switch [cFor "j"];
  elim_loop_single [cFor "j"];
  Variable.inline [cVarDef "j"];
  )
