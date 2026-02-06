open Prelude
open Target
open Flags

let change_to_thread_for_mode (t: trm): trm =
  (* Does not do any checks - only works when models are enabled!! *)
  assert !Flags.use_resources_with_models;
  let error = "Thread for transformation is invalid: it is not applied on a for loop." in
  let range, _, body, contract = trm_inv ~error trm_for_inv t in
  (trm_like ~old:t (trm_for ~contract ~mode:GpuThread range body))

let ghost_group_to_desyncgroup = toplevel_var "group_to_desyncgroup"

let convert_to_desyncgroup_ghost (n: formula) (items: formula): trm =
  Resource_trm.ghost (ghost_call ghost_group_to_desyncgroup [
    "N", n; "items", items
  ])

(* Path.to_outer_loop *)

(* just use trm_seq_inv to insert stuff
 let instrs, _ = trm_inv ~error:"expected seq" trm_seq_inv body in
    let open Resource_formula in
    let open Resource_trm in
    let instrs2 = instrs |>
      Mlist.push_front (assume (formula_in_range new_j (formula_loop_range rj))) |>
      Mlist.push_front (assume (formula_in_range new_i (formula_loop_range ri)))
    in

  index_in_seq gives a path to the sequence; modify this with Path.apply_on_path

  let (alloc_i, seq_p) = Path.index_in_seq alloc_p in
*)

let extract_ptr_from_resource (f: formula): var option =
  let open Resource_formula in
  let rec nested_group_inv (f: formula): formula =
    Pattern.pattern_match f [
      Pattern.(formula_group __ (formula_range (trm_int (eq 0)) __ (trm_int (eq 1))) !__)
        (fun inner_formula () -> nested_group_inv inner_formula);
      Pattern.(formula_desyncgroup __ __ !__)
        (fun inner_formula () -> nested_group_inv inner_formula);
      Pattern.__ (fun () -> f)
    ] in
  let inner_formula = nested_group_inv f in
  match (formula_repr_inv inner_formula) with
  | Some ({desc = Trm_var v}, _) -> Some v
  | Some (loc, _) -> (
    match (Matrix_trm.access_inv loc) with
    | Some ({desc = Trm_var v}, _, _) -> Some v
    | _ -> None)
  | _ -> None

(* let%transfo thread_for (tg : target) : unit =
  apply_at_target_paths (change_to_thread_for_mode) tg *)

(* Function 1: Convert a loop at the path p to thread for. Insert a ghost group to desyncgroup of every resource before the loop. Don't change the contracts in that loop, but if
  it is specified to be in a loop, then modify its contract to contain DesyncGroups. (using the procedure described in my notes).
  *)
(* return the stopping point, and whether we stopped early *)
let to_thread_for ?(enclosing_loop : string option) (produce_contract_map: trm varmap ref) (loop_path : path): (path * bool) =
  let loop_trm = Path.resolve_path loop_path (Trace.ast ()) in (* TODO: copying everywhere ?? *)
  let ({ index = i; stop = n }, _, _, _contract) = trm_inv
    ~error:"Gpu_basic.convert_thread_for_nest: expected simple loop."
    trm_for_inv loop_trm in
  let (loop_i, seq_path) = Path.index_in_seq loop_path in
  let consume_contract_map = ref Var_map.empty in
  let abstract_ind (f: formula) (i: var) =
    let i_f = new_var i.name in
    let f = trm_subst_var i (trm_var i_f) f in
    Resource_formula.formula_fun [(i_f, typ_int)] f in
  List.iter (fun (_,f) ->
    match (extract_ptr_from_resource f) with
    | Some v ->
        consume_contract_map := Var_map.add v f !consume_contract_map;
    | _ -> ()) _contract.iter_contract.pre.linear;
  List.iter (fun (_,f) ->
    match (extract_ptr_from_resource f) with
    | Some v ->
        produce_contract_map := Var_map.add v f !produce_contract_map;
    | _ -> ()) _contract.iter_contract.post.linear;
  let ghosts = Var_map.fold (fun _ f ghosts ->
    (convert_to_desyncgroup_ghost n (abstract_ind f i)) :: ghosts) !consume_contract_map [] in

  let enclosing_path = match enclosing_loop with
  | Some _ -> Some (Path.to_outer_loop loop_path)
  | _ -> None in
  Target.apply_at_path (change_to_thread_for_mode) loop_path;
  Target.apply_at_path (fun seq ->
    let instrs, ret = trm_inv ~error:"expected seq" trm_seq_inv seq in
    let instrs = Mlist.insert_sublist_at loop_i ghosts instrs in
    trm_alter seq ~desc:(Trm_seq (instrs,ret))) seq_path;
  match enclosing_path with
  | Some p ->
    Target.apply_at_path (fun loop ->
      let range, mode, body, contract = trm_inv ~error:"Gpu_basic.convert_thread_for_nest: expected simple loop." trm_for_inv loop in
      let produce_resources = List.map (fun (vf,f) ->
        match (extract_ptr_from_resource f) with
        | Some v -> (
          match (Var_map.find_opt v !produce_contract_map) with
          | Some f ->
            let i_f = new_var i.name in
            (vf, Resource_formula.formula_desyncgroup i_f n (trm_subst_var i (trm_var i_f) f))
          | _ -> (vf,f))
        | _ -> (vf,f)) contract.iter_contract.post.linear in
      let iter_contract = { contract.iter_contract with post = { contract.iter_contract.post with linear = produce_resources} } in
      (trm_like ~old:loop (trm_for ~contract:{ contract with iter_contract } ~mode range body))
      ) p;
    (p, false)
  | _ -> (seq_path @ [Path.Dir_seq_nth (loop_i + (List.length ghosts))], false) (* TODO probably very fragile and sketchy way of adjusting the path *)

let var_kernel_launch = toplevel_var "kernel_launch"
let var_kernel_setup_end = toplevel_var "kernel_setup_end"
let var_kernel_teardown_begin = toplevel_var "kernel_teardown_begin"
let var_kernel_kill = toplevel_var "kernel_kill"

let ghost_kernel_teardown_sync = toplevel_var "kernel_teardown_sync"
let var_magic_sync = toplevel_var "magicsync"

let call_magic_sync (i: var) (n: formula) (items: formula): trm =
  let i_f = new_var i.name in
  let items = trm_subst_var i (trm_var i_f) items in
  trm_apps (trm_var var_magic_sync) [] ~ghost_args:[(new_var "H"), (Resource_formula.formula_desyncgroup i_f n items)]

(* Function 2: Take a list of loop index names, ordered from inner to outer, and a path to a for loop.
  If the list is empty, then call function 1 with None as the enclosing loop.
  If the list is h :: tl, then call function 1 with h as the enclosing loop, and recurse with the enclosing loop itself as the path, with tl.*)
let rec to_thread_fors (stop_tg: target option) (insert_barrier: bool) (enclosing_loops : string list) (loop_path : path) : unit =
  let produce_contract_map = ref Var_map.empty in
  let loop_path, stop_path, stop_early = match enclosing_loops with
  | [] ->
    let stop_path,stop_early = to_thread_for produce_contract_map loop_path in
    (stop_path, Some stop_path, stop_early)
  | h::tl ->
    let stop_path,stop_early = to_thread_for ~enclosing_loop:h produce_contract_map loop_path in
    if stop_early then
      loop_path, Some stop_path, true
    else (
      to_thread_fors stop_tg insert_barrier tl stop_path; (*TODO*)
      loop_path, None, false
    ) in
  let apply_at_stop p: unit =  (* TODO misleading name *)
    let ind, seq_p = Path.index_in_seq p in
    let loop_trm = Path.resolve_path loop_path (Trace.ast ()) in (* TODO: copying everywhere ?? *)
    let ({ index = i; stop = n }, _, _, _contract) = trm_inv
    ~error:"Gpu_basic.convert_thread_for_nest: expected simple loop."
      trm_for_inv loop_trm in
    if insert_barrier then
      let sync_ghosts = trm_seq (Var_map.fold (fun _ f ghosts ->   (* TODO: nobrace ? *)
        Mlist.push_front (call_magic_sync i n f) ghosts) !produce_contract_map Mlist.empty) in
      Target.apply_at_path (fun seq ->
        let instrs, ret = trm_inv ~error:"expected seq" trm_seq_inv seq in
        trm_alter ~desc:(Trm_seq (Mlist.insert_at (ind+1) sync_ghosts instrs, ret)) seq
      ) seq_p in

  match stop_path with
  | Some stop_path -> (
    match stop_tg with
    | Some stop_tg when not stop_early ->
      Target.iter apply_at_stop stop_tg
    | _ -> apply_at_stop stop_path
  )
  | _ -> ()

let%transfo convert_thread_for_tail_nest ?(stop_tg: target option) ?(insert_barrier: bool = false) (enclosing_loops : string list) (tg : target) : unit =
  Trace.tag_valid_by_composition ();
  let enclosing_loops = List.rev enclosing_loops in
  Target.iter (fun p -> to_thread_fors stop_tg insert_barrier enclosing_loops p) tg

(* tg is the body of the kernel; TODO should targets that are sequences of instructions *)
(* TODO: take another target, which is the launching function, for modifying the contract to include hostctx and __requires for the kernel retiling. *)
let%transfo create_kernel_launch ?(tctx_override: trm list option) (tpbs: trm list) (bpgs: trm list) (smem_szs: trm list) (tg: target): unit =
  let aux (t: trm): trm =
    let tpb = Matrix_trm.msize tpbs in
    let bpg = Matrix_trm.msize bpgs in
    let smem_sz = trm_int 0 in (* TODO *)
    let grid = match tctx_override with
    | Some tctx_override -> Matrix_trm.msize tctx_override
    | _ -> Matrix_trm.msize (bpgs @ tpbs) in

    let launch = trm_apps (trm_var var_kernel_launch) [tpb;bpg;smem_sz] in
    let setup = trm_apps (trm_var var_kernel_setup_end) [] ~ghost_args:[(new_var "tctx_sz", grid)] in
    let setup = match tctx_override with
      | Some _ -> setup
      | _ ->
        let assume_retile = Resource_trm.assume (trm_eq ~typ:typ_int (trm_mul_int bpg tpb) grid) in
        Nobrace.trm_seq (Mlist.of_list [
          assume_retile;
          setup
        ]) in

    let teardown = trm_apps (trm_var var_kernel_teardown_begin) [] ~ghost_args:[(new_var "tctx_sz", grid)] in
    let kill = trm_apps (trm_var var_kernel_kill) [] in

    trm_seq (Mlist.of_list [
      launch;
      setup;
      t;
      teardown;
      kill;
    ]) in
  Target.apply_at_target_paths (fun t ->
    Nobrace.remove_after_trm_op aux t
  ) tg

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


let%transfo convert_to_global_mem ~(var: string) (tg: target): unit =
  let open Resource_formula in
  let rec convert_cell_mem_type f =
    Pattern.pattern_match f [
      Pattern.(trm_cell __) (fun () -> (trm_cell ~mem_typ:(trm_var var_gmem) ()));
      Pattern.(trm_uninit_cell __) (fun () -> (trm_uninit_cell ~mem_typ:(trm_var var_gmem) ()));
      Pattern.__ (fun () -> trm_map convert_cell_mem_type f)
    ] in
  let (var, _) = find_var var tg in
  let rec convert_ops_mem_type t =
    (* let override = match (trm_let_inv t) with
    | Some (let_var,_,_) when (var_eq let_var var) -> true
    | _ -> override in *)
    (* small optimization: dont do search for variable in every term (O(n^2)), cache search result for subterms
      although maybe its not useful because we dont care about all subterms *)
    let t = trm_map convert_ops_mem_type t in
    Pattern.pattern_match t [
      Pattern.(trm_get !__) (fun addr () ->
        Pattern.when_ (is_free_var_in_trm var addr);
        trm_apps (trm_var var__gmem_get) [addr]
      );
      Pattern.(trm_set !__ !__) (fun addr rhs () ->
        Pattern.when_ (is_free_var_in_trm var addr);
        trm_apps (trm_var var__gmem_set) [addr; rhs]
      );
      Pattern.(formula_repr !__ !__ ) (fun l r () ->
        Pattern.when_ (is_free_var_in_trm var l);
        Resource_formula.formula_repr l (convert_cell_mem_type r)
      );
      Pattern.(trm_delete !__) (fun addr () ->
        Pattern.when_ (is_free_var_in_trm var addr);
        trm_apps (trm_var var_gmem_free) [addr]
      );
      (* !(check (fun v -> String.starts_with ~prefix:"MATRIX" v.name)) *)
      Pattern.(trm_apps (trm_var !__) !__ __ __) (fun v args () ->
        if (String.starts_with ~prefix:(sprintf "MATRIX%d_COPY_" (List.length args - 2)) v.name) then (
          let var_ind = List.find_index (fun arg ->
            match trm_var_inv arg with
            | Some v2 when (var_eq var v2) -> true
            | _ -> false) args in
          match var_ind with
          | Some 0 -> trm_apps (trm_var (var_memcpy_host_to_device (List.length args - 2))) args
          | Some 1 -> trm_apps (trm_var (var_memcpy_device_to_host (List.length args - 2))) args
          | _ -> raise Pattern.Next
        (*else if (String.starts_with ~prefix:(sprintf "MATRIX%d_MEMSET_" (List.length args)) v.name) then
          t *)
        ) else raise Pattern.Next
      );
      Pattern.__ (fun () ->
        match (Matrix_trm.let_alloc_inv t) with
        | Some (array_var, typ_array, typ_alloc, trms, init) when (var_eq array_var var) ->
          assert (not init); (* TODO implement CALLOC *)
          let f = trm_add_cstyle (Typ_arguments [typ_alloc]) (trm_var (var__gmem_malloc (List.length trms))) in
          let alloc_instr = trm_apps f trms ~ghost_args:[(new_var "T", typ_alloc)] in
          trm_let (var,typ_array) alloc_instr
        | _ -> t )
    ] in
  Target.apply_at_target_paths (fun t -> convert_ops_mem_type t) tg


(* TODO: support more than kernel teardown sync *)
let%transfo convert_magic_sync (tg: target): unit =
  Target.apply_at_target_paths (fun t ->
    match t.desc with
    | Trm_apps ({desc = Trm_var var_magic_sync}, tl, ga, gb) ->
      trm_add_attribute GhostInstr (trm_alter ~desc:(Trm_apps (trm_var ghost_kernel_teardown_sync, tl, ga, gb)) t)
    | _ -> failwith "Gpu_basic.convert_magic_sync: expected target to point to a magic sync instruction.") tg
