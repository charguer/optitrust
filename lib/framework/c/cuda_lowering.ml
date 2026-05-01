open Ast
open Trm
open Typ
open Contextualized_error

open Gpu_trm

(* -------------  CUDA-specific variable definitions ----------------- *)

let var__syncthreads = toplevel_var "__syncthreads"
let trm__syncthreads () = trm_var var__syncthreads
let var__syncwarp = toplevel_var "__syncwarp"
let trm__syncwarp () = trm_var (var__syncwarp)

let var__threadIdx = toplevel_var "threadIdx"
let trm__threadIdx () = trm_var (var__threadIdx)
let var__blockIdx = toplevel_var "blockIdx"
let trm__blockIdx () = trm_var (var__blockIdx)

(* hack for accessing method of helper SharedMemory class in optitrust_gpu_cuda.cuh *)
let var__smem_ptr = toplevel_var "smem.ptr"
let trm__smem_ptr () = trm_var (var__smem_ptr)
let var__sharedmemory = toplevel_var "SharedMemory"
let typ__sharedmemory () = trm_var (var__sharedmemory)

(* These are local, not toplevel variables, created for each kernel. *)
let var__smem_buf () = new_var "smem"
let var__ctx_size () = new_var "__ctx_sz"
let var__tid () = new_var "__tid"

(* ------------------------- Lowering -------------------------------- *)

let make_kernel_header (bpg: trm) (tpb: trm) (ctx_size: var) (tid: var): trm =
  let access_x v = trm_struct_get ~struct_typ:typ_auto v "x" in
  let ctx_sz_decl = (trm_let (ctx_size,typ_int) (trm_mul_int bpg tpb)) in
  let tid_decl = (trm_let (tid,typ_int) (trm_add_int (trm_mul_int (access_x (trm__blockIdx ())) tpb) (access_x (trm__threadIdx ())))) in
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

let is_gpu_get_operation (v: var): bool =
  (var_has_name var__gmem_get.name v)
  || (var_has_name var__smem_get.name v)

let is_gpu_set_operation (v: var): bool =
  (var_has_name var__gmem_set.name v)
  || (var_has_name var__smem_set.name v)

let is_any_mem_operation (p: prim): bool =
  match p with
    | Prim_unop (Unop_get)
    | Prim_unop (Unop_post_decr)
    | Prim_unop (Unop_post_incr)
    | Prim_unop (Unop_pre_decr)
    | Prim_unop (Unop_pre_incr)
    | Prim_unop (Unop_struct_access _)
    | Prim_unop (Unop_struct_get _)
    | Prim_binop (Binop_set) -> true
    | _ -> false

let rec generalize_mem_ops (t: trm): trm = Pattern.pattern_match t [
  Pattern.(trm_apps2 (trm_var (check is_gpu_set_operation)) !__ !__) (fun p v () ->
    let p = generalize_mem_ops p in
    let v = generalize_mem_ops v in
    trm_set p v);
  Pattern.(trm_apps1 (trm_var (check is_gpu_get_operation)) !__) (fun p () ->
    let p = generalize_mem_ops p in
    trm_get p);
  Pattern.(trm_prim (check is_any_mem_operation) __) (fun () -> failwith "Host-side memory operations not allowed in device code");
  Pattern.__ (fun () -> trm_map generalize_mem_ops t)
]

let rec lower_syncs (t: trm): trm =
  let t = Option.value (Gpu_trm.barrier_seq_inv t) ~default:t in
  if (trm_has_cstyle BarrierSequence t) then begin failwith "Term was marked as barrier sequence, but did not simplify to a single barrier" end;
  Pattern.pattern_match t [
    Pattern.(trm_apps0 (trm_var_with_name "blocksync")) (fun () -> trm_apps (trm__syncthreads ()) []);
    Pattern.(trm_apps0 (trm_var_with_name "magic_barrier")) (fun () -> failwith "Magic barriers not allowed in CUDA code");
    Pattern.__ (fun () -> trm_map lower_syncs t)
  ]

let rec remove_dmindexs (t: trm): trm = match (Matrix_trm.dmindex_inv t) with
  | Some (_,_) -> trm_int 0
  | _ -> trm_map remove_dmindexs t

(* LATER: More extensive wellformedness checks for better errors.
Currently, this should not generate anything unsound, but a malformed AST may produce
CUDA code that references undefined OptiGPU constructs that were not lowered in this step. *)
(* LATER: Better optimization (CSE, constant folding, etc.). See issue #40 *)
let lower_device_code (grid_size: var) (tid: var) (t: trm): trm =
  t |> (flatten_thread_loops grid_size tid)
    |> generalize_mem_ops
    |> lower_syncs
    |> remove_dmindexs

let lower_smem_alloc (t: trm): trm option =
  Pattern.pattern_match_opt t [
    Pattern.(trm_let !__ (typ_ptr !__) (trm_apps (trm_var !__) !__ __ __)) (fun v typ vf args () ->
      Pattern.when_ (vf.name = sprintf "__smem_malloc%d" (List.length args));
      let smem_ptr_call = (trm_apps (trm__smem_ptr ()) [Matrix_trm.msize args]) in
      (trm_let (v, typ_ptr typ) (trm_cast (typ_ptr typ) smem_ptr_call))
    )
  ]

let lower_host_fn (bound_vars_typs: typ varmap ref) (k_id: int ref) (t: trm): trm =
  let rec scan_bound_vars t = (
    trm_iter scan_bound_vars t;
    match t.desc with
    (* Should handle all cases of binders with typed variables; see `find_var_filter_on` for reference *)
    | Trm_let ((var,typ),body) ->
      bound_vars_typs := Var_map.add var typ !bound_vars_typs;
    | Trm_for ({index = var; _},_,_,_) ->
      bound_vars_typs := Var_map.add var (typ_int) !bound_vars_typs;
    | _ -> ()) in
  scan_bound_vars t;
  let bound_vars = Var_set.of_seq (fst (Seq.split (Var_map.to_seq !bound_vars_typs))) in
  let kernels = ref [] in
  let rec lower_kernel_call t = Pattern.pattern_match t [
    Pattern.(trm_seq !__ __) (fun instrs () ->
      (* first check to see if the sequence is likely to be kernel-related at all *)
      let launch_args = ref [] in
      let kernel_transition_inds = (Array.make 4 (-1)) in
      Mlist.iteri (fun i instr ->
        Pattern.pattern_match instr [
          Pattern.(trm_apps (trm_var_with_name "kernel_launch") !__ __ __) (fun args () ->
            launch_args := args;
            kernel_transition_inds.(0) <- i);
          Pattern.(trm_apps (trm_var_with_name "kernel_setup_end") __ __ __) (fun () -> kernel_transition_inds.(1) <- i);
          Pattern.(trm_apps (trm_var_with_name "kernel_teardown_begin") __ __ __) (fun () -> kernel_transition_inds.(2) <- i);
          Pattern.(trm_apps (trm_var_with_name "kernel_kill") __ __ __) (fun () -> kernel_transition_inds.(3) <- i);
          Pattern.__ (fun () -> ())
        ]
      ) instrs;
      if (kernel_transition_inds.(0) == -1 || (List.length !launch_args == 0) ) then
        raise Pattern.Next;
      if (Array.exists (fun ind -> ind == -1) kernel_transition_inds) then
        failwith "Missing kernel_setup_end, kernel_teardown_begin, or kernel_kill in sequence starting with kernel_launch (setup: %d, teardown: %d, kill: %d)" kernel_transition_inds.(1) kernel_transition_inds.(2) kernel_transition_inds.(3);
      if (not (List.for_all (fun i ->
        kernel_transition_inds.(i) <= kernel_transition_inds.(i+1)) [0;1;2])) then
          failwith "Kernel transition functions in wrong order (launch: %d, setup: %d, teardown: %d, kill: %d)" kernel_transition_inds.(0) kernel_transition_inds.(1) kernel_transition_inds.(2) kernel_transition_inds.(3);
      let launch_args = !launch_args in

      let kernel_body_start = (kernel_transition_inds.(1) + 1) in
      let kernel_body_len = kernel_transition_inds.(2) - kernel_body_start in
      (* TODO: can we completely ignore setup and teardown code? Or do we have to do some syntax check there too? *)
      (* maybe *Any operations are banned inside the entire kernel sequence? *)
      let smem_allocs = Mlist.fold_left (fun instrs instr ->
        match (lower_smem_alloc instr) with
        | Some instr -> instr :: instrs
        | _ -> instrs) [] (snd (Mlist.extract 0 kernel_transition_inds.(1) instrs)) in
      let smem_allocs = match smem_allocs with
        | [] -> []
        | _ -> (
          let smem_var = var__smem_buf () in
          let smem_defn = trm_let (smem_var, typ__sharedmemory ()) (trm_ref_uninit (typ__sharedmemory ())) in
          smem_defn :: smem_allocs
        ) in
      let _, body = Mlist.extract kernel_body_start kernel_body_len instrs in

      let ctx_size,tid = var__ctx_size (), var__tid () in
      let body = trm_seq ~typ:(typ_unit) body in
      let body = lower_device_code ctx_size tid body in
      let body =
        match (trm_seq_inv body) with
        | Some (tl, result) ->
          let tl = (Mlist.insert_sublist_at 0 smem_allocs tl) in
          let tl = (Mlist.push_front (make_kernel_header (List.nth launch_args 0) (List.nth launch_args 1) ctx_size tid) tl) in
          trm_alter ~desc:(Trm_seq (tl, result)) t
        | _ -> failwith "expected seq" in
      (* LATER: run arith_simplify on body here ? *)

      let launch_args = List.map (fun arg -> trm_add_cstyle CudaKernelBracketArg arg) launch_args in
      let kernel_args = Var_set.inter (trm_free_vars body) bound_vars in
      let kernel_args = Var_set.fold (fun var acc -> (var,Var_map.find var !bound_vars_typs) :: acc) kernel_args [] in

      let kernel_fn = (trm_add_cstyle CudaGlobal (trm_fun kernel_args (typ_unit) body)) in
      let kernel_var = new_var ("__kernel" ^ (string_of_int (!k_id))) in
      incr k_id;
      kernels := (kernel_var, kernel_fn) :: !kernels;
      trm_apps (trm_var kernel_var) (launch_args @ (List.map (fun (var,_) -> trm_var var) kernel_args))
    );
    Pattern.__ (fun () -> trm_map lower_kernel_call t)
  ] in
  let t = lower_kernel_call t in
  let kernel_decls = List.fold_left (fun defns kernel -> Mlist.push_front (trm_let kernel (snd kernel)) defns) (Mlist.of_list [t]) !kernels in
  Nobrace.trm_seq kernel_decls

let lower_to_cuda (t: trm): trm =
  let k_id = ref 0 in
  let device_fns: Var_set.t ref = ref Var_set.empty in
  (* Remove all rewrite sequences; CUDA doesn't seem to like the expression sequences we generate. *)
  (* LATER: maybe just force the user to take care of this in the script? that way we
  can move trm_seq_rewrite_flatten out of Gpu_trm *)
  let t = trm_seq_rewrite_flatten t in
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
