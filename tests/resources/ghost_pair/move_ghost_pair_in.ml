open Optitrust
open Target
open Ast
open Typ
open Prelude
open Resources

let move_ghost_pair_in_on (i : int) (t : trm) : trm =
  let remove_ressource_in_contract (to_remove : resource_item list)
  (* This function is incomplete. In order to handle more advanced ghosts behavior, invaraint; and iter_contract must be updated *)
        (loop_contract : loop_contract) : loop_contract =
    {
      loop_ghosts = loop_contract.loop_ghosts;
      invariant = loop_contract.invariant;
      parallel_reads = [];
      iter_contract = loop_contract.iter_contract;
      strict = false;
    }
  in
  let tl, res = trm_inv trm_seq_inv t in
  let lbefore, t_for, lafter = Mlist.get_item_and_its_relatives i tl in
  let ghost_begin =
    match Mlist.last lbefore with
    | Some x -> x
    | None ->
        trm_fail t
          "Move_ghost_pair_in: Exepcted a ghost pair just before the loop"
  in
  (* Ghost informations : make sur it's a ghost instruction *)
  let ghost_name, _ghost_fn, _ghost_args, _ghost_bind =
    match Resource_trm.ghost_begin_inv ghost_begin with
    | Some (var, gcall) ->
        (var, gcall.ghost_fn, gcall.ghost_args, gcall.ghost_bind)
    | _ ->
        trm_fail t
          "Move_ghost_pair_in: Expected a ghost pair juste before the loop"
  in
  let res_before = before_trm ghost_begin in
  let res_after = after_trm ghost_begin in
  let _com, _consumed, produced =
    Resource_formula.filter_common_resources res_before.linear res_after.linear
  in
  let ghost_end = Mlist.nth lafter 0 in
  let _name_bis =
    match Resource_trm.ghost_end_inv ghost_end with
    | Some v -> if v <> ghost_name then trm_fail t "Move ghost_pair_in: Not the same ghost instruction" else v
    | _ ->
        trm_fail t
          "Move_ghost_pair_in: Expected a ghost end juste after the loop"
  in
  let range, body_instrs, contract =
    trm_inv
      ~error:
        "Move_ghost_pair_in: expeted the target to be pointing at a for loop"
      trm_for_inv_instrs t_for
  in
  let new_contract = remove_ressource_in_contract produced contract in
  let new_body =
    trm_seq_helper [ Trm ghost_begin; TrmMlist body_instrs; Trm ghost_end ]
  in
  trm_seq_helper
    [
      TrmMlist (Mlist.pop_back lbefore);
      Trm (trm_for ~contract:new_contract range new_body);
      TrmMlist (Mlist.pop_front lafter);
    ]

(** [move_ghost_pair_in tg]: Expects the target to point at a loop
Will try to ove the first ghost pairs inside the loop body :
Transform :
__ghost_begin(gp)
for {
 body}
 __ghost_end(gp)
 Into :
 for {
__ghost_begin(gp)
 body
 __ghost_end(gp)
 }
 This transformation is for now incomplete and only handles ghost that produces/consumes sreads. The par_reads are deleted and recomputed after the ghosts has been moved inside the for's body
    *)
let move_ghost_pair_in tg =
  Trace.justif "Only moving ghost code around";
  apply_at_target_paths_in_seq move_ghost_pair_in_on tg

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ =
  Run.script_cpp (fun _ ->
      !!move_ghost_pair_in [ cFunDef "simple_ghost_in"; cFor "i" ];
      !!move_ghost_pair_in [ cFunDef "delete_sreads"; cFor "i" ];

      (* !!move_ghost_pair_in [ cFunDef "ghost_in_invariant"; cFor "i" ]; *)
      !!())
