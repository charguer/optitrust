open Ast
open Trm
open Typ
open Contextualized_error


(* Convert seq to function call, and return declarations
  Match t with trm_seq kernel_start(...) S kernel_end()
-> (vars,typs) = Foreach resource in t.ctx.pure, if it is in t.ctx.usage_pre
  f = trm_fun (vars,typs) void S
  fs := f :: !fs
  args = [tpb, bpg] |> apply_cuda_kernel_launch attribute
  call = trm_apps f.name (args ++ vars)
  return call ??

*)

let flatten_thread_loops (grid_size: var) (tid: var) (t: trm): trm =
  let grid_size = trm_var grid_size in
  let tid = trm_var tid in
  let rec aux (dims: trm list) (t: trm) =
    match (trm_for_inv_instrs t) with
    | Some (range, GpuThread, instrs, _) ->
      let size_from_dims dims = (trm_trunc_div_int grid_size (List.fold_left (trm_mul_int) (trm_lit (Lit_int (typ_int,1))) dims)) in
      let decl = trm_let (range.index, typ_int) (trm_trunc_div_int (trm_trunc_mod_int tid (size_from_dims dims)) (size_from_dims (range.stop::dims))) in
      let instrs = Mlist.map (fun instr -> aux (range.stop::dims) instr) instrs in
      Nobrace.trm_seq (Mlist.push_front decl instrs)
    | _ -> trm_map (aux dims) t
  in Nobrace.remove_after_trm_op (aux []) t

let rec generalize_mem_ops (t: trm): trm = Pattern.pattern_match t [
  Pattern.(trm_apps2 (trm_var_with_name "__GMEM_SET") !__ !__) (fun p v () ->
    let p = generalize_mem_ops p in
    let v = generalize_mem_ops v in
    trm_set p v);
  Pattern.(trm_apps1 (trm_var_with_name "__GMEM_GET") !__) (fun p () ->
    let p = generalize_mem_ops p in
    trm_get p);
  Pattern.__ (fun () -> trm_map generalize_mem_ops t)
]

let lower_kernel_body (grid_size: var) (tid: var) (t: trm): trm = t |> (flatten_thread_loops grid_size tid ) |> generalize_mem_ops

let lower_fn (bound_vars_typs: typ varmap ref) (k_id: int ref) (t: trm): trm =
  let rec scan_bound_vars t = match (trm_let_inv (t)) with
  | Some (var,typ,_) ->
    bound_vars_typs := Var_map.add var typ !bound_vars_typs;
    trm_iter scan_bound_vars t
  | _ -> trm_iter scan_bound_vars t in
  scan_bound_vars t;
  let bound_vars = Var_set.of_seq (fst (Seq.split (Var_map.to_seq !bound_vars_typs))) in

  let kernels = ref [] in
  let rec lower_kernel_call t = Pattern.pattern_match t [
    Pattern.(trm_seq !__ __) (fun instrs () ->
      (* TODO messy way to extract instructions *)
      let last_instr = Option.unsome_or_else (Mlist.nth_opt instrs (Mlist.length instrs - 1)) (fun () -> raise Pattern.Next) in
      Option.unsome_or_else (Pattern.pattern_match_opt last_instr [
        Pattern.(trm_apps (trm_var_with_name "kernel_end") __ __ __) (fun () -> ());
      ]) (fun () -> raise Pattern.Next);
      let first_instr = Option.unsome_or_else (Mlist.nth_opt instrs 0) (fun () -> raise Pattern.Next) in
      let start_args = Option.unsome_or_else (Pattern.pattern_match_opt first_instr [
        Pattern.(trm_apps (trm_var_with_name "kernel_start") !__ __ __) (fun args () -> args);
      ]) (fun () -> raise Pattern.Next) in
      let _, body = Mlist.extract 1 (Mlist.length instrs - 2) instrs in
      let grid_size = new_var "__grid_sz" in
      let tid = new_var "__tid" in
      let threadIdx = new_var "threadIdx" in
      let blockDim = new_var "blockDim" in
      let blockIdx = new_var "blockIdx" in
      let gridDim = new_var "gridDim" in
      let access_x v = trm_struct_get ~struct_typ:typ_auto (trm_var v) "x" in
      let grid_sz_decl = (trm_let (grid_size,typ_int) (trm_mul_int (access_x gridDim) (access_x blockDim))) in
      let tid_decl = (trm_let (tid,typ_int) (trm_add_int (trm_mul_int (access_x blockIdx) (access_x blockDim)) (access_x threadIdx))) in
      let body = trm_seq ~typ:(typ_unit) (Mlist.push_front grid_sz_decl (Mlist.push_front tid_decl body)) in
      let body = lower_kernel_body grid_size tid body in
      let start_args = List.map (fun arg -> trm_add_cstyle CudaKernelBracketArg arg) start_args in
      let kernel_args = Var_set.inter (trm_free_vars body) bound_vars in
      let kernel_args = Var_set.fold (fun var acc -> (var,Var_map.find var !bound_vars_typs) :: acc) kernel_args [] in
      let kernel_fn = (trm_add_cstyle CudaGlobal (trm_fun kernel_args (typ_unit) body)) in
      let kernel_var = new_var ("__kernel" ^ (string_of_int (!k_id))) in
      k_id := !k_id + 1;
      kernels := (kernel_var, kernel_fn) :: !kernels;
      trm_apps (trm_var kernel_var) (start_args @ (List.map (fun (var,_) -> trm_var var) kernel_args))
    );
    Pattern.__ (fun () -> trm_map lower_kernel_call t)
  ] in
  let t = lower_kernel_call t in
  let kernel_decls = List.fold_left (fun defns kernel -> Mlist.push_front (trm_let kernel (snd kernel)) defns) (Mlist.of_list [t]) !kernels in
  Nobrace.trm_seq kernel_decls

let lower_to_cuda (t: trm): trm =
  let k_id = ref 0 in
  let rec aux t = match (trm_let_fun_inv t) with
    | Some (_,_,args,_,_)->
      let bound_vars_typs: typ varmap ref = ref (Var_map.of_seq (List.to_seq args)) in
      lower_fn bound_vars_typs k_id t
    | _ -> trm_map aux t in
  Nobrace.remove_after_trm_op aux t
