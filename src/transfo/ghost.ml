open Prelude
open Resource_formula
open Resource_contract
open Resources

let rec contains_only_ghost_code (t: trm): bool =
  match t.desc with
  | Trm_apps _ when trm_has_cstyle GhostCall t -> true
  | Trm_seq seq -> List.for_all contains_only_ghost_code (Mlist.to_list seq)
  | Trm_for (_, body, _) -> contains_only_ghost_code body
  | _ -> false

let embed_loop_on (t: trm): trm =
  let t = loop_minimize_on t in
  if not (contains_only_ghost_code t) then failwith "Ghost.embed_loop_on: the loop contains non ghost code";
  let range, body, contract = trm_inv ~error:"Ghost.embed_loop_on: can only be applied on a for loop" trm_for_inv t in
  let outer_contract = loop_outer_contract range (Option.get contract) in
  trm_ghost_varargs (trm_fun [] None ~contract:(FunSpecContract outer_contract) (trm_seq (Mlist.of_list [trm_copy t]))) []

let%transfo embed_loop (tg: target): unit =
  recompute_all_resources ();
  Target.apply_at_target_paths embed_loop_on tg