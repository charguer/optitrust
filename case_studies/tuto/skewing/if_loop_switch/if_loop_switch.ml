open Optitrust
open Prelude

let _ = Flags.check_validity := false
(** [if_loop_switch t]:
  Transform :
{
    for(int i = a ; i < b ; i++)
      {
      if (a == c)
        seq
      }
}
  into:
{
      for (int i = c ; i < c+1; i++){
      if(i>= a and i < b )
        seq
      }
 }     *)
let if_loop_switch_on (t : trm) : trm =
  let error = "if_loop_switch: Expected a for loop" in
  let l_range, body, contract = trm_inv ~error trm_for_inv t in
  let error = "if_loop_switch: Expected instrs inside the body" in
  let instrs, res = trm_inv ~error trm_seq_inv body in
  let error = "if_loop_switch: Expected if as the first instr" in
  let cond, then_, else_ = trm_inv ~error trm_if_inv (Mlist.nth instrs 0) in
  let error = "if_loop_switch: Expected an equality condition" in
  let l_value, r_value = trm_inv ~error trm_eq_inv cond in
  let error =
    "if_loop_switch: Expected a var as the left part of the equality condition"
  in
  let l_value_var = trm_inv ~error trm_var_inv l_value in

  if l_value_var <> l_range.index then
    trm_fail t
      "if_loop_switch: Expected same trm for loop index and if condition left \
       value";
  if Option.is_none l_value.typ then
    trm_fail t "if_loop_switch: The index should be typed ";
  let typ = Option.get l_value.typ in
  let new_cond =
    trm_and
      (trm_ge ~typ l_value l_range.start)
      (trm_le ~typ l_value l_range.stop)
  in
  let new_if = trm_if new_cond then_ else_ in
  let new_lrange : loop_range =
    {
      index = l_range.index;
      start = r_value;
      stop = trm_add_int (trm_int 1) r_value;
      direction = DirUp;
      step = trm_step_one ();
    }
  in
  let new_instrs = Mlist.push_front new_if (Mlist.pop_front instrs) in
  trm_for new_lrange (trm_seq ?result:res new_instrs) ~contract

let if_loop_switch (tg : target) = apply_at_target_paths if_loop_switch_on tg
let _ = Run.script_cpp (fun _ -> if_loop_switch [ cFor "i" ])
