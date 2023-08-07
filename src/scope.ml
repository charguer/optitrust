open Ast
open Trm

let trm_let_or_let_fun_inv t =
  match trm_let_inv t with
  | Some (_, x, _, _) -> Some x
  | _ ->
  begin match trm_let_fun_inv t with
  | Some (qvar, _, _, _) -> Some (qvar_to_var qvar)
  | _ -> None
  end

(** Lists all the let-bindings inside [tl_new_scope] interfering with the instructions in [tl_after].
    A let-binding interferes if it appears as a free variable in [tl_after]. *)
let find_interference tl_new_scope tl_after : var list =
  let fv_after = trm_free_vars (trm_seq tl_after) in
  let find_toplevel_bind t =
    match trm_let_or_let_fun_inv t with
    | Some x when Var_set.mem x fv_after -> Some x
    | _ -> None
  in
  List.filter_map find_toplevel_bind (Mlist.to_list tl_new_scope)

(** Like [find_interference], but fails if there are interferences. *)
let assert_no_interference ~(after_what : string) ~(on_interference : string) tl_new_scope tl_after : unit =
  match find_interference tl_new_scope tl_after with
  | [] -> ()
  | [x] -> failwith (sprintf "local variable '%s' is used after %s, but will now be %s" x after_what on_interference)
  | xs -> failwith (sprintf "local variables %s are used after %s, but will now be %s" (Tools.list_to_string ~sep:"', '" ~bounds:["'";"'"] xs) after_what on_interference)

(** If [x] is used in [instrs], traces a justification, otherwise fails. *)
let justif_unused_in (x : var) (instrs : trm mlist) : unit =
  let fv = trm_free_vars (trm_seq instrs) in
  if Var_set.mem x fv then
    failwith (sprintf "'%s' is used" x);
  Trace.justif (sprintf "'%s' is unused" x)

(** Given a path to a variable definition, assert that it is unused using [justif_unused_in]. *)
let justif_unused (p : Path.path) : unit =
  let t = Path.get_trm_at_path p (Trace.ast ()) in
  let error = "expected a variable or function definition within a sequence" in
  let x = trm_inv ~error trm_let_or_let_fun_inv t in
  let (index, pseq) = Path.index_in_seq p in
  let instrs = trm_inv ~error trm_seq_inv (Path.get_trm_at_path pseq (Trace.ast ())) in
  let _, instrs_after = Mlist.split (index + 1) instrs in
  justif_unused_in x instrs_after