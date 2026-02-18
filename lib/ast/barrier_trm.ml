open Ast
open Trm
open Typ
let magic_barrier_var = toplevel_var "magic_barrier"
let magic_barrier (): trm =
  trm_apps (trm_var magic_barrier_var) []

let magic_barrier_inv (t: trm): unit option =
  match (trm_apps_inv t) with
  | Some ({desc = Trm_var v}, _) when (var_eq v magic_barrier_var) -> Some ()
  | _ -> None

let all_mem_ok_var = toplevel_var "all_mem_ok"

let barrier_seq (barrier_fn: trm -> trm) (hs: trms): trm =
  trm_add_cstyle BarrierSequence (trm_seq (Mlist.of_list (List.map barrier_fn hs)))

let barrier_seq_inv (t: trm): trm option =
  if (not (trm_has_cstyle BarrierSequence t)) then None
  else match (trm_seq_inv t) with
  | Some (instrs, _) ->
    let instrs = Mlist.to_list instrs in
    let open Option.Monad in
    let* hd = List.nth_opt instrs 0 in
    let* (barrier_fn, _) = trm_apps_inv hd in
    let* barrier_fn_v = trm_var_inv barrier_fn in
    if (List.for_all (fun instr -> match (trm_apps_inv instr) with
      | Some ({desc = Trm_var v}, []) when (var_eq v barrier_fn_v) ->
        true
      | _ -> false) instrs) then Some barrier_fn else None
  | _ -> None

let magic_barrier_to_seq (barrier_fn: trm -> trm) (resource_filter: trm -> bool) (t: trm): trm =
  let before = Option.unsome t.ctx.ctx_resources_before in
  let usage = Option.unsome t.ctx.ctx_resources_usage in

  let hs = List.filter_map (fun (v,h) ->
    let v_usage = (Var_map.find_opt v usage) in
    if ((v_usage = Some ConsumedFull || v_usage = Some ConsumedUninit) && (resource_filter h)) then
      Some h
    else
      None) before.linear in
  barrier_seq barrier_fn hs
