open Ast
open Trm
open Typ

(* ------------------------------ Barriers --------------------------- *)
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

(* ------------------------ Kernel launches -------------------------- *)
let var_kernel_launch = toplevel_var "kernel_launch"
let var_kernel_setup_end = toplevel_var "kernel_setup_end"
let var_kernel_teardown_begin = toplevel_var "kernel_teardown_begin"
let var_kernel_kill = toplevel_var "kernel_kill"

(* --------------------- GPU memory operations ----------------------- *)

let var_gmem = toplevel_var "GMem"
let var__gmem_get = toplevel_var "__gmem_get"
let var__gmem_set = toplevel_var "__gmem_set"
let var_gmem_free = toplevel_var "gmem_free"
let var__gmem_malloc nb_dims =
  toplevel_var (sprintf "__gmem_malloc%d" nb_dims)
let var_memcpy_host_to_device nb_dims =
  toplevel_var (sprintf "memcpy_host_to_device%d" nb_dims)
let var_memcpy_device_to_host nb_dims =
  toplevel_var (sprintf "memcpy_device_to_host%d" nb_dims)

let var_smem = toplevel_var "SMem"
let var__smem_get = toplevel_var "__smem_get"
let var__smem_set = toplevel_var "__smem_set"
let var__smem_free nb_dims =
  toplevel_var (sprintf "__smem_free%d" nb_dims)
let var__smem_malloc nb_dims =
  toplevel_var (sprintf "__smem_malloc%d" nb_dims)

let var__treg_ref = toplevel_var "__treg_ref"
let var__treg_ref_s = toplevel_var "__treg_ref_s"
let var__treg_ref_uninit0_s = toplevel_var "__treg_ref_uninit0_s"
let var__treg_ref_uninit = Matrix_trm.toplevel_var_with_dim "__treg_ref_uninit%d"
let var__treg_ref_uninit_inv = Matrix_trm.toplevel_var_with_dim_inv var__treg_ref_uninit
