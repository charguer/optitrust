include Arrays_basic

open Target
open Ast

let is_loop_with_index (i : var) (t : trm) : bool =
  match trm_for_inv t with
  | Some ((idx, _, _, _, _, _), _) when idx = i -> true
  | _ -> false

let find_surrounding_loop (i : var) (p : path) : path =
  let rec aux p =
    let p_t = Path.resolve_path p (Trace.ast ()) in
    if is_loop_with_index i p_t then p else aux (Path.parent p)
  in
  aux p

let unroll_surrounding_loop (i : var) (p : path) : unit =
  let loop_p = find_surrounding_loop i p in
  Loop.unroll loop_p

let unroll_index_vars_from_array_reads (tg : target) : unit =
  Target.iter (fun t p ->
    let read_t = Path.resolve_path p t in
    let error = "Arrays.inline_index_vars_from_array_reads: expected array access" in
    let ptr_t = trm_inv ~error trm_get_inv read_t in
    let (_, index) = trm_inv ~error array_access_inv ptr_t in
    let vars = ref Var_set.empty in
    let rec collect_vars (t : trm) : unit =
      match trm_var_inv t with
      | Some (_, v) -> vars := Var_set.add v !vars
      | _ -> trm_iter collect_vars t
    in
    collect_vars index;
    Var_set.iter (fun i -> unroll_surrounding_loop i p) !vars
  ) tg

(* [inline_constant] expects the target [decl] to point at a constant array literal declaration, and resolves all accesses targeted by [tg], that must be at constant indices.
For every variable in non-constant indices, this transformation will attempt unrolling the corresponding for loop.
  *)
let%transfo inline_constant ?(mark_accesses : mark option) ~(decl : target) (tg : target) : unit =
  (* TODO: unroll if necessary *)
  Marks.with_fresh_mark (fun m ->
    Marks.add m tg;
    unroll_index_vars_from_array_reads [cMark m];
    Arrays_basic.inline_constant ?mark_accesses ~decl [cMark m]
  )

(* [elim_constant] expects the target [tg] to point at a constant array literal declaration, and resolves all its accesses, that must be at constant indices. Then, eliminates the array declaration.
  *)
let%transfo elim_constant ?(mark_accesses : mark option) (tg : target) : unit =
  Target.iter (fun t p ->
    let decl_t = Path.resolve_path p t in
    let error = "Arrays.elim_constant: expected constant array literal declaration" in
    let (_, var, _, _) = trm_inv ~error trm_let_inv decl_t in
    let (_, p_seq) = Path.index_in_seq p in
    inline_constant ?mark_accesses ~decl:(target_of_path p) ((target_of_path p_seq) @ [nbAny; cArrayRead var]);
    Arrays_basic.elim (target_of_path p);
  ) tg