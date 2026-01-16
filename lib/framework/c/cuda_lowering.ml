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

let flatten_thread_loops (t: trm): trm =
  let grid_size = trm_var (new_var "__grid_sz") in (* TODO add declarations for these *)
  let tid = trm_var (new_var "__tid") in (* TODO *)
  let rec aux (decls: trm list) (dims: trm list) (t: trm) =
    match (trm_for_inv t) with
    | Some (range, GpuThread, body, _) ->
      let size_from_dims dims = (trm_trunc_div_int grid_size (List.fold_left (trm_mul_int) (trm_lit (Lit_int (typ_int,1))) dims)) in
      let decl = trm_let (range.index, typ_int) (trm_trunc_div_int (trm_trunc_mod_int tid (size_from_dims dims)) (size_from_dims (range.stop::dims))) in
      (* TODO: nobrace *)
      aux (decl::decls) (range.stop::dims) body
    | _ -> match (trm_seq_inv t) with
      | Some (ts,res) ->
        let ts = List.fold_left (fun ts decl -> Mlist.push_front decl ts) ts decls in
        let t = trm_alter ~desc:(Trm_seq (ts,res)) t in
        trm_map (aux [] dims) t
      | _ -> trm_map (aux decls dims) t
  in aux [] [] t

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

let lower_kernel_body (t: trm): trm = t |> flatten_thread_loops |> generalize_mem_ops

let lower_to_cuda (t: trm): trm =
  let rs = Option.unsome t.ctx.ctx_resources_after in
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
      let _, body = Mlist.extract 1 (Mlist.length instrs - 1) instrs in
      let body = trm_seq ~typ:(typ_unit) body in
      let body = lower_kernel_body body in

      let start_args = List.map (fun arg -> trm_add_attribute CudaKernelBracketArg arg) start_args in
      let bound_vars = Var_set.of_list (List.split_pairs_fst rs.pure) in
      (* TODO: not really working; pulling variables from the pure (ghost) context too
        and doesn't have the type information so it uses auto (not ideal) *)
      let kernel_args = trm_free_vars ~bound_vars body in
      let kernel_args = Var_set.fold (fun v l -> (v,typ_auto) :: l) kernel_args [] in
      let kernel_fn = trm_fun kernel_args (typ_unit) body in
      let kernel_var = new_var ("__kernel" ^ (string_of_int (List.length !kernels))) in
      kernels := (kernel_var, kernel_fn) :: !kernels;
      trm_apps (trm_var kernel_var) (start_args @ (List.map (fun (var,_) -> trm_var var) kernel_args))
    );
    Pattern.__ (fun () -> trm_map lower_kernel_call t)
  ] in
  match (trm_seq_inv (lower_kernel_call t)) with
  | Some (instrs, res) ->
      let predecls,defns = List.fold_left (fun (predecls,defns) kernel ->
        (trm_predecl kernel) :: predecls,(trm_let kernel (snd kernel)) :: defns
        ) ([],[]) !kernels in
      (trm_seq (Mlist.merge (Mlist.merge (Mlist.of_list predecls) instrs) (Mlist.of_list defns)) ?result:res)
  | None -> failwith "expect sequence at toplevel"
