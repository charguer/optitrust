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
let to_thread_for ?(enclosing_loop : string option) (loop_path : path): path =
  let loop_trm = Path.resolve_path loop_path (Trace.ast ()) in (* TODO: copying everywhere ?? *)
  let ({ index = i; stop = n }, _, _, _contract) = trm_inv
    ~error:"Gpu_basic.convert_thread_for_nest: expected simple loop."
    trm_for_inv loop_trm in
  let (loop_i, seq_path) = Path.index_in_seq loop_path in
  let consume_contract_map = ref Var_map.empty in
  let produce_contract_map = ref Var_map.empty in
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
    p
  | _ -> loop_path



(* Function 2: Take a list of loop index names, ordered from inner to outer, and a path to a for loop.
  If the list is empty, then call function 1 with None as the enclosing loop.
  If the list is h :: tl, then call function 1 with h as the enclosing loop, and recurse with the enclosing loop itself as the path, with tl.*)
let rec to_thread_fors (enclosing_loops : string list) (loop_path : path) : unit =
  match enclosing_loops with
  | [] ->
    let _ = to_thread_for loop_path in
    ()
  | h::tl ->
    let loop_path = to_thread_for ~enclosing_loop:h loop_path in
    to_thread_fors tl loop_path (*TODO*)



let%transfo convert_thread_for_nest (enclosing_loops : string list) (tg : target) : unit =
  Trace.tag_valid_by_composition ();
  let enclosing_loops = List.rev enclosing_loops in
  Target.iter (fun p -> to_thread_fors enclosing_loops p) tg
