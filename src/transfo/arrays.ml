include Arrays_basic

open Target
open Prelude

(* returns the [i] and [indices - i] if [t] is a loop over index [i]. *)
let inv_loop_with_index_in (indices : Var_set.t) (t : trm) : (var * Var_set.t) option =
  match trm_for_inv t with
  | Some ((idx, _, _, _, _, _), _, _) when Var_set.mem idx indices ->
    Some (idx, Var_set.remove idx indices)
  | _ -> None

let find_surrounding_loops (indices : Var_set.t) (p : path) : paths =
  let rec aux indices p =
    if Var_set.is_empty indices then []
    else begin
    let p_t = Path.resolve_path p (Trace.ast ()) in
    match inv_loop_with_index_in indices p_t with
    | Some (_, indices) ->
      p :: (aux indices (Path.parent p))
    | None -> aux indices (Path.parent p)
    end
  in
  aux indices p

let unroll_surrounding_loops (indices : Var_set.t) (p : path) : unit =
  let loop_ps = find_surrounding_loops indices p in
  Loop.unroll (target_of_paths loop_ps)

let unroll_index_vars_from_array_reads (tg : target) : unit =
  Target.iter (fun t p ->
    let read_t = Path.resolve_path p t in
    let error = "Arrays.inline_index_vars_from_array_reads: expected array access" in
    let ptr_t = trm_inv ~error trm_get_inv read_t in
    let (_, index) = trm_inv ~error array_access_inv ptr_t in
    let vars = ref Var_set.empty in
    let rec collect_vars (t : trm) : unit =
      match trm_var_inv t with
      | Some v -> vars := Var_set.add v !vars
      | _ -> trm_iter collect_vars t
    in
    collect_vars index;
    unroll_surrounding_loops !vars p
  ) tg

(* FIXME: should be equal to arith default? *)
let default_inline_constant_simpl tg = Arith.(simpl_surrounding_expr (fun x -> compute (gather x))) (nbAny :: tg)

(* [inline_constant] expects the target [decl] to point at a constant array literal declaration, and resolves all accesses targeted by [tg], that must be at constant indices.
For every variable in non-constant indices, this transformation will attempt unrolling the corresponding for loop.
  *)
let%transfo inline_constant ?(mark_accesses : mark option) ~(decl : target) ?(simpl : Transfo.t = default_inline_constant_simpl) (tg : target) : unit =
  Trace.tag_valid_by_composition ();
  (* TODO: unroll if necessary *)
  Marks.with_fresh_mark (fun m ->
    Marks.add m tg;
    unroll_index_vars_from_array_reads [nbMulti; cMark m];
    simpl [nbAny; cMark m];
    Arrays_basic.inline_constant ?mark_accesses ~decl [nbMulti; cMark m]
  )

(* [elim_constant] expects the target [tg] to point at a constant array literal declaration, and resolves all its accesses, that must be at constant indices. Then, eliminates the array declaration.
  *)
let%transfo elim_constant ?(mark_accesses : mark option) (tg : target) : unit =
  Target.iter (fun t p ->
    let decl_t = Path.resolve_path p t in
    let error = "Arrays.elim_constant: expected constant array literal declaration" in
    let (_, var, _, _) = trm_inv ~error trm_let_inv decl_t in
    let (_, p_seq) = Path.index_in_seq p in
    inline_constant ?mark_accesses ~decl:(target_of_path p) ((target_of_path p_seq) @ [nbAny; cArrayRead var.name]);
    Arrays_basic.elim (target_of_path p);
  ) tg
