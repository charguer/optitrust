open Prelude
open Target
open Flags

include Gpu_basic

  (* take 4 targets, one for each transition point. If they don't all belong to the same sequence,
  the transfo will complain. Since they belong to the same sequence, we know we can lift those instrs out into their own
  sequence, to make the printers job easier. Or we could just make the printer's job easier by detecting the start within sequences.*)
(* TODO: take another target, which is the launching function, for modifying the contract to include hostctx and __requires for the kernel retiling. *)
(* TODO: cleaner to take a tuple of tpbs bpgs etc.? or a dedicated kernel launch type? less arguments *)
let%transfo create_kernel_launch ?(grid_override: trm list option) ?(setup_end: target option) ?(teardown_begin: target option) (bpgs: trm list) (tpbs: trm list) (smem_szs: trm list)
  (start: target) (stop: target): unit =
  (fun () -> (fun next_m ->
    let tpb = Matrix_trm.msize tpbs in
    let bpg = Matrix_trm.msize bpgs in
    let smem_sz = trm_int 0 in (* TODO *)
    let grid = match grid_override with
    | Some grid_override -> Matrix_trm.msize grid_override
    | _ -> Matrix_trm.msize (bpgs @ tpbs) in

    let launch_mark = next_m () in
    let kill_mark = next_m () in

    let launch = trm_add_mark (launch_mark) (trm_apps (trm_var var_kernel_launch) [bpg;tpb;smem_sz]) in
    let setup = trm_apps (trm_var var_kernel_setup_end) [] ~ghost_args:[(new_var "grid_sz", grid)] in
    let setup = match grid_override with
      | Some _ -> setup
      | _ ->
        let assume_retile = Resource_trm.assume (Resource_formula.formula_is_true (trm_eq ~typ:typ_int (trm_mul_int bpg tpb) grid)) in
        Nobrace.trm_seq (Mlist.of_list [
          assume_retile;
          setup
        ]) in

    let teardown = trm_apps (trm_var var_kernel_teardown_begin) [] ~ghost_args:[(new_var "grid_sz", grid)] in
    let kill = trm_add_mark kill_mark (trm_apps (trm_var var_kernel_kill) []) in

    (* TODO: sanitize and tell the user each target should only have one occurence *)
    Sequence_basic.insert ~reparse:false launch start;
    Sequence_basic.insert ~reparse:false setup (Option.unsome_or_else setup_end (fun () -> [tAfter; cMark launch_mark]));
    Sequence_basic.insert ~reparse:false kill stop;
    Sequence_basic.insert ~reparse:false teardown (Option.unsome_or_else teardown_begin (fun () -> [tBefore; cMark kill_mark]));

    Sequence.intro_between [tBefore; cMark launch_mark] [tAfter; cMark kill_mark]
  ) |> Marks.with_marks) |> Nobrace_transfo.remove_after


(*

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

*)

let%transfo convert_tail_thread_for (loops : int list) (leaf: target) =
  let rec aux loops_incl_leaf leaf_p: unit =
    let convert,loops = match loops_incl_leaf with
    | 0 :: tl -> false, tl
    | 1 :: tl -> true, tl
    | _ -> failwith "Gpu.convert_tail_thread_for: loops should contain only 0 or 1" in
    Marks.with_marks (fun next_m ->
      let barrier_mark = next_m () in
      let leaf_t = (target_of_path leaf_p) in
      if convert then
        seq_for_to_magicthread_for ~barrier_mark leaf_t
      else
        (* TODO: should probably not assume seq for already has a barrier afterwards
        but we would always push it out, in theory *)
        Marks.add barrier_mark (tAfter :: leaf_t);
      (* push_work_before [cMark barrier_mark] ... *)
      Flags.check_validity := true;
      Loop.fission [tBefore; cMark barrier_mark];
      Flags.check_validity := false;
      (* TODO: fission the other side *)
      Barriers.remove_loop_around_barrier [cMark barrier_mark];
    );
    match loops with
    | [] -> ()
    | _ ->
      let _, next_leaf_p = Path.index_in_surrounding_loop leaf_p in
      aux loops next_leaf_p
    in
  Target.iter (aux (1 :: loops)) leaf
