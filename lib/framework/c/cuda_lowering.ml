open Ast
open Trm
open Typ
open Contextualized_error

let trm__syncthreads () = trm_var (new_var "__syncthreads")
let trm__syncwarp () = trm_var (new_var "__syncwarp")

let var__ctx_size () = new_var "__ctx_sz"
let var__tid () = new_var "__tid"



let make_kernel_header (ctx_size: var) (tid: var): trm =
  let var_threadIdx = trm_var (new_var "threadIdx") in
  let var_blockDim = trm_var (new_var "blockDim") in
  let var_blockIdx = trm_var (new_var "blockIdx") in
  let var_gridDim = trm_var (new_var "gridDim") in
  let access_x v = trm_struct_get ~struct_typ:typ_auto v "x" in
  let ctx_sz_decl = (trm_let (ctx_size,typ_int) (trm_mul_int (access_x var_gridDim) (access_x var_blockDim))) in
  let tid_decl = (trm_let (tid,typ_int) (trm_add_int (trm_mul_int (access_x var_blockIdx) (access_x var_blockDim)) (access_x var_threadIdx))) in
  (Nobrace.trm_seq (Mlist.of_list [ctx_sz_decl;tid_decl]))

let flatten_thread_loops (ctx_size: var) (tid: var) (t: trm): trm =
  let ind_vars = ref Var_map.empty in
  let decls = ref [] in

  let rec aux (ctx_size_last: var) (t: trm) =
    match (trm_for_inv_instrs t) with
    | Some (range, GpuThread, instrs, _) ->
      let num_vars = List.length (!decls) / 2 in

      let ctx_size_new = new_var (ctx_size.name ^ "_" ^ (string_of_int num_vars)) in
      let sz_decl = trm_let (ctx_size_new, typ_int) (trm_trunc_div_int (trm_var ctx_size_last) range.stop) in

      let index' = { range.index with name = "__" ^ range.index.name ^ (string_of_int num_vars)} in
      ind_vars := Var_map.add range.index index' !ind_vars;
      let ind_decl = trm_let (range.index, typ_int) (trm_trunc_div_int (trm_trunc_mod_int (trm_var tid) (trm_var ctx_size_last)) (trm_var ctx_size_new)) in
      decls := ind_decl :: sz_decl :: !decls;

      Nobrace.trm_seq (Mlist.map (aux ctx_size_new) instrs)
    | _ ->
      let t = trm_map (aux ctx_size_last) t in
      match (trm_apps_inv t) with
      | Some (f, args) when (trm_has_cstyle CudaDevice t) ->
          let args = (trm_var ctx_size_last) :: (trm_var tid) :: args in
          trm_like ~old:t (trm_apps f args)
      | _ -> t
  in Nobrace.remove_after_trm_op (fun t -> t
    |> (aux ctx_size)
    |> (fun t ->
        let decls = (Mlist.of_list (List.rev !decls)) in
        match (trm_seq_inv t) with
        | Some (tl, result) ->
          let tl = Mlist.merge decls tl in
          trm_alter ~desc:(Trm_seq (tl, result)) t
        | _ -> trm_seq (Mlist.push_back t decls))
    |> (trm_vars_subst !ind_vars)) t

let rec generalize_mem_ops (t: trm): trm = Pattern.pattern_match t [
  Pattern.(trm_apps2 (trm_var_with_name "__gmem_set") !__ !__) (fun p v () ->
    let p = generalize_mem_ops p in
    let v = generalize_mem_ops v in
    trm_set p v);
  Pattern.(trm_apps1 (trm_var_with_name "__gmem_get") !__) (fun p () ->
    let p = generalize_mem_ops p in
    trm_get p);
  (* TODO: check also the variety of other operations that exist on Any cells, like increment, struct access, etc.*)
  Pattern.(trm_get __ ^| trm_set __ __) (fun () -> failwith "Host-side memory operations not allowed in device code");
  Pattern.__ (fun () -> trm_map generalize_mem_ops t)
]

let rec lower_syncs (t: trm): trm = Pattern.pattern_match t [
    Pattern.(trm_apps0 (trm_var_with_name "blocksync")) (fun () -> trm_apps (trm__syncthreads ()) []);
    Pattern.__ (fun () -> trm_map lower_syncs t)
  ]

(* TODO: doesn't perform extensive wellformedness checks; only checks that no host-side get, set, etc. are used
But since the host-side functions are not defined, presumably it would just be garbage on the CUDA side. Should it be more extensive?*)
let lower_device_code (grid_size: var) (tid: var) (t: trm): trm =
  t |> (flatten_thread_loops grid_size tid)
    |> generalize_mem_ops
    |> lower_syncs

let lower_host_fn (bound_vars_typs: typ varmap ref) (k_id: int ref) (t: trm): trm =
  let rec scan_bound_vars t = match t.desc with
    | Trm_let ((var,typ),_) ->
      bound_vars_typs := Var_map.add var typ !bound_vars_typs;
    | Trm_for ({index = var; _},_,_,_) ->
      bound_vars_typs := Var_map.add var (typ_int) !bound_vars_typs;
    (* TODO: Handle other cases. I don't think map_binder in trm_map_vars
    will work because I don't think the ctx can be used to associate vars with types. *)
    | _ -> ();
    trm_iter scan_bound_vars t in
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

      let ctx_size,tid = var__ctx_size (), var__tid () in
      let body = trm_seq ~typ:(typ_unit) body in
      let body = lower_device_code ctx_size tid body in
      let body =
        match (trm_seq_inv body) with
        | Some (tl, result) ->
          let tl = (Mlist.push_front (make_kernel_header ctx_size tid) tl) in
          trm_alter ~desc:(Trm_seq (tl, result)) t
        | _ -> failwith "expected seq" in

      let start_args = List.map (fun arg -> trm_add_cstyle CudaKernelBracketArg arg) start_args in
      let kernel_args = Var_set.inter (trm_free_vars body) bound_vars in
      let kernel_args = Var_set.fold (fun var acc -> (var,Var_map.find var !bound_vars_typs) :: acc) kernel_args [] in

      let kernel_fn = (trm_add_cstyle CudaGlobal (trm_fun kernel_args (typ_unit) body)) in
      let kernel_var = new_var ("__kernel" ^ (string_of_int (!k_id))) in
      incr k_id;
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
  let device_fns: Var_set.t ref = ref Var_set.empty in
  let rec lower_fns t =
    let ot () =
      let open Option.Monad in
      let* xf, _, tf = trm_let_inv t in
      let* args, ret_ty, body, fun_spec = trm_fun_inv tf in
      if (trm_has_cstyle CudaDevice tf) then (
        let ctx_size,tid = var__ctx_size (), var__tid () in
        let args = (ctx_size,typ_int)::(tid,typ_int)::args in
        let body = lower_device_code ctx_size tid body in
        let tf = trm_alter ~desc:(Trm_fun (args,ret_ty,body,fun_spec)) tf in
        device_fns := Var_set.add xf !device_fns;
        Some (trm_alter ~desc:(Trm_let ((xf,tf), tf)) t)
      ) else (
        let bound_vars_typs: typ varmap ref = ref (Var_map.of_seq (List.to_seq args)) in
        Some (lower_host_fn bound_vars_typs k_id t)
      )
    in
    match ot () with
    | Some tv -> tv
    | _ -> trm_map lower_fns t in
  let rec check_device_fn_calls t =
    (match (trm_apps_inv t) with
    | Some ({ desc = Trm_var v }, _) ->
      (if (Var_set.mem v (!device_fns)) <> (trm_has_cstyle CudaDevice t) then
        failwith "Device function call should be to a device function!"
      else trm_iter check_device_fn_calls t)
    | _ -> trm_iter check_device_fn_calls t;)
  in
  let check_device_fn_calls t = check_device_fn_calls t; t in
  Nobrace.remove_after_trm_op (fun t -> t |> lower_fns |> check_device_fn_calls) t
