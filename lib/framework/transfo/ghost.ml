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

let embed_loop_on (mark : mark) (t: trm): trm =
  let t = loop_minimize_on t in
  if not (contains_only_ghost_code t) then failwith "Ghost.embed_loop_on: the loop contains non ghost code";
  let range, body, contract = trm_inv ~error:"Ghost.embed_loop_on: can only be applied on a for loop" trm_for_inv t in
  let outer_contract = contract_outside_loop range contract in
  trm_add_mark mark (Resource_trm.ghost (ghost_closure_call outer_contract (trm_seq (Mlist.of_list [trm_copy t]))))

let%transfo embed_loop ?(mark : mark = "") (tg: target): unit =
  Resources.ensure_computed ();
  Target.apply_at_target_paths (embed_loop_on mark) tg;
  Resources.justif_correct "only changed ghost code"

(** <private> *)
let farthest_commuting_pos (i : int) (direction : int) (seq : trm) : int =
  let error = "Ghost_pair.move_in_seq: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let instr = Mlist.nth instrs i in
  let current_i = ref i in
  let dest_offset, interference_with_instr =
    match direction with
    | -1 -> 0, fun next -> collect_trm_interferences next instr
    | 1 -> 1, fun next -> collect_trm_interferences instr next
    | _ -> failwith "Ghost.farthest_commuting_pos: expected -1 or 1 direction"
  in
  let commutes_with_next () : bool =
    let next_i = !current_i + direction in
    let commutes = match Mlist.nth_opt instrs next_i with
    | Some next ->
      let interference = interference_with_instr next in
      let commutes = Var_map.is_empty interference in
      (* DEBUG:
      if not commutes then
        print_string (string_of_interference interference); *)
      commutes
    | None ->
      false
    in
    if commutes then current_i := next_i;
    commutes
  in
  while commutes_with_next () do () done;
  !current_i + dest_offset

(** Moves instruction at index [i] in sequence [seq] as far down as possible, as long as effects commute. *)
let move_down_in_seq (i : int) (seq : trm) : trm =
  let desired_pos = farthest_commuting_pos i 1 seq in
  if desired_pos <= i + 1 then
    seq
  else
    Instr_core.move_at desired_pos i seq

(** Moves instruction at index [i] in sequence [seq] as far up as possible, as long as effects commute. *)
let move_up_in_seq (i : int) (seq : trm) : trm =
  let desired_pos = farthest_commuting_pos i (-1) seq in
  if desired_pos >= i then
    seq
  else
    Instr_core.move_at desired_pos i seq
