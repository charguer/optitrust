open Prelude
open Target
open Resource_formula
open Resource_contract

type unparsed_contract = (contract_clause_type * string) list

let with_desugared_res push_fn clause (x, formula) contract =
  push_fn clause (x, (desugar_formula (Ast_fromto_AstC.caddress_elim formula))) contract

let parse_fun_contract =
  parse_contract_clauses empty_fun_contract (with_desugared_res push_fun_contract_clause)

let parse_loop_contract =
  parse_contract_clauses empty_loop_contract (with_desugared_res push_loop_contract_clause)

let __pure () = Requires, ""
let __requires (r: string) = Requires, r
let __ensures (r: string) = Ensures, r
let __invariant (r: string) = Invariant, r
let __reads (r: string) = Reads, r
let __modifies (r: string) = Modifies, r
let __consumes (r: string) = Consumes, r
let __produces (r: string) = Produces, r
let __sequentially_reads (r: string) = SequentiallyReads, r
let __sequentially_modifies (r: string) = SequentiallyModifies, r

let set_fun_contract_on (contract: fun_contract) (t: trm): trm =
  let name, ret_typ, args, body = trm_inv ~error:"Resources.set_fun_contract_on: Expected function" trm_let_fun_inv t in
  trm_like ~old:t (trm_let_fun name ret_typ args ~contract:(FunSpecContract contract) body)

let%transfo set_fun_contract (contract: unparsed_contract) (tg : Target.target) : unit =
  Target.apply_at_target_paths (set_fun_contract_on (parse_fun_contract contract)) tg

let set_loop_contract_on (contract: loop_contract) (t: trm): trm =
  let range, body, _ = trm_inv ~error:"Resource.set_loop_contract_on: Expected for loop" trm_for_inv t in
  trm_like ~old:t (trm_for ~contract range body)

let%transfo set_loop_contract (contract: unparsed_contract) (tg: Target.target): unit =
  Target.apply_at_target_paths (set_loop_contract_on (parse_loop_contract contract)) tg

(*let recompute_resources (tg : Target.target) : unit =
  Target.apply_at_target_paths (Resources_core.trm_recompute_resources) tg*)

let recompute_all_resources () : unit =
  Trace.typing_step ~name:"Resource recomputation" (fun () ->
    let t = Trace.ast () in
    let t = Scope.infer_var_ids t in (* Resource computation needs var_ids to be calculated *)
    (* TODO: Configurable base environment *)
    let t = Resource_computation.(trm_recompute_resources empty_resource_set t) in
    Trace.set_ast t
  )

(* TODO: avoid recomputing all resources for validity checks. *)
let required_for_check () : unit =
  if !Flags.check_validity then recompute_all_resources ()

let justif_correct (why : string) : unit =
  if !Flags.check_validity then begin
    recompute_all_resources ();
    Trace.justif (sprintf "resources are correct: %s" why)
  end

let trm_for_contract t =
  match trm_for_inv t with
  | Some (_, _, Some c) -> Some c
  | _ -> None

let loop_minimize_on (t: trm): trm =
  let range, body, contract = trm_inv ~error:"loop_minimize_on: not a for loop" trm_for_inv t in
  let res_before =
    match t.ctx.ctx_resources_before with
    | None -> fail t.loc "loop_minimize_on: resources need to be computed before this transformation"
    | Some res_before -> res_before
  in

  let contract = match contract with
    | None -> { loop_ghosts = res_before.pure; invariant = resource_set ~linear:res_before.linear (); iter_contract = empty_fun_contract }
    | Some contract -> contract
  in

  let body_res_usage =
    match body.ctx.ctx_resources_usage with
    | None -> fail t.loc "loop_minimize_on: the body of the loop needs a resource usage annotation"
    | Some res_before -> res_before
  in

  let new_fracs = ref [] in
  let keep_used_filter (hyp, formula) =
    match Hyp_map.find hyp body_res_usage with
    | NotUsed -> None
    | UsedReadOnly ->
      begin match formula_read_only_inv formula with
      | Some _ -> Some (hyp, formula)
      | None ->
        let frac_var, frac_ghost = new_frac () in
        new_fracs := frac_ghost :: !new_fracs;
        let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
        Some (hyp, ro_formula)
      end
    | UsedFull -> Some (hyp, formula)
  in

  let new_linear_invariant = List.filter_map keep_used_filter contract.invariant.linear in
  let new_invariant = { contract.invariant with linear = new_linear_invariant } in

  let new_contract = { contract with loop_ghosts = !new_fracs @ contract.loop_ghosts; invariant = new_invariant } in
  let t = trm_like ~old:t (trm_for range ~contract:new_contract body) in
  Resource_computation.(trm_recompute_resources res_before t)

(* [loop_minimize]: minimize linear invariants of a loop contract *)
let%transfo loop_minimize (tg: target) : unit =
  recompute_all_resources (); (* TODO: Incrementalize this computation *)
  Target.apply_at_target_paths loop_minimize_on tg

let assert_hyp_read_only ~(error : string) ((x, t) : (hyp * formula)) : unit =
  match formula_read_only_inv t with
  | Some _ -> ()
  | None -> failwith (sprintf "%s: %s is used sequentially and is not read only." error (Ast_fromto_AstC.named_formula_to_string (x, t)))

let justif_parallelizable_loop_contract ~error (contract: loop_contract): unit =
  if contract.invariant.linear <> []
    then failwith (sprintf "%s: the for loop is not parallelizable, invariant non empty." error)
    else Trace.justif "The for loop is parallelizable"

(** Collects effects intererences as a map from hyps to pairs of interfering resource usages. *)
let collect_interferences (before : trm) (after : trm) : (resource_usage option * resource_usage option) Hyp_map.t =
  (* TODO: let error' = error in *)
  let error = "expected resources usage to be available" in
  let a_res = Tools.unsome ~error before.ctx.ctx_resources_usage in
  let b_res = Tools.unsome ~error after.ctx.ctx_resources_usage in
  let res_merge _ a_res_usage b_res_usage =
    match (a_res_usage, b_res_usage) with
    | (_, None)
    | (_, Some NotUsed)
    | (Some NotUsed, _) -> None
    | (None, _)
    | (Some UsedFull, _)
    | (_, Some UsedFull) -> Some (a_res_usage, b_res_usage)
    | _ -> None
  in
  Hyp_map.merge res_merge a_res b_res

(** <private> *)
let string_of_interference (interference : (resource_usage option * resource_usage option) Hyp_map.t) : string =
  sprintf "the resources do not commute: %s\n" (Tools.list_to_string (List.map (fun (x, (f1, f2)) -> sprintf "%s: %s != %s" x.name (resource_usage_opt_to_string f1) (resource_usage_opt_to_string f2)) (Hyp_map.bindings interference)))

(** Checks that effects commute, infer var ids to check pure facts scope. *)
let assert_commute (before : trm) (after : trm) : unit =
  let interference = collect_interferences before after in
  if not (Hyp_map.is_empty interference) then
    fail after.loc (string_of_interference interference)

(** <private> *)
let move_in_seq (i : int) (direction : int) (seq : trm) : trm =
  let error = "Resources.move_in_seq: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let instr = Mlist.nth instrs i in
  let current_i = ref i in
  let dest_offset, interference_with_instr =
    match direction with
    | -1 -> 0, fun next -> collect_interferences next instr
    | 1 -> 1, fun next -> collect_interferences instr next
    | _ -> failwith "Resources.move_in_seq: expected -1 or 1 direction"
  in
  let commutes_with_next () : bool =
    let next_i = !current_i + direction in
    let commutes = match Mlist.nth_opt instrs next_i with
    | Some next ->
      let interference = interference_with_instr next in
      let commutes = Hyp_map.is_empty interference in
      if not commutes then
        print_string (string_of_interference interference);
      commutes
    | None ->
      false
    in
    if commutes then current_i := next_i;
    commutes
  in
  while commutes_with_next () do () done;
  printf "move_in_seq: %i -> %i\n" i !current_i;
  if i != !current_i then
    let dest_i = !current_i + dest_offset in
    Instr_core.copy_aux dest_i i true seq
  else
    seq

(** Moves instruction at index [i] in sequence [seq] as far down as possible, as long as effects commute.
    TODO: what about var ids and pure facts scopes?
   *)
let move_down_in_seq (i : int) (seq : trm) : trm = move_in_seq i 1 seq

(** Moves instruction at index [i] in sequence [seq] as far up as possible, as long as effects commute.
    TODO: what about var ids and pure facts scopes?
   *)
let move_up_in_seq (i : int) (seq : trm) : trm = move_in_seq i (-1) seq

(** <private>
    Moves all begins downwards, starting from downmost ones. *)
let move_all_begins_downwards (seq : trm) : trm =
  let error = "Resources.move_all_begins_downwards: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let begins = ref [] in
  let find_begins i instr =
    match trm_ghost_begin_inv instr with
    | Some _ -> begins := i :: !begins;
    | None -> ()
  in
  Mlist.iteri find_begins instrs;
  let upwards_begins = !begins in
  Printf.printf "upwards_begins: %s\n" (Tools.list_to_string (List.map string_of_int upwards_begins));
  List.fold_left (fun seq beg_i ->
    move_down_in_seq beg_i seq
  ) seq upwards_begins

(** <private>
    Moves all ends upwards, starting from upwardmost ones. *)
let move_all_ends_upwards (seq : trm) : trm =
  let error = "Resources.move_all_ends_upwards: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let ends = ref [] in
  let find_ends i instr =
    match trm_ghost_end_inv instr with
    | Some _ -> ends := i :: !ends;
    | None -> ()
  in
  Mlist.iteri find_ends instrs;
  let downwards_ends = List.rev !ends in
  Printf.printf "downwards_ends: %s\n" (Tools.list_to_string (List.map string_of_int downwards_ends));
  List.fold_left (fun seq end_i ->
    move_up_in_seq end_i seq
  ) seq downwards_ends

(** <private>
    Cancels all ghost pairs that have an empty scope, starting from innermost ones. *)
let cancel_all_ghost_pairs (seq : trm) : trm =
  let error = "Resources.cancel_all_ghost_pairs: expected sequence" in
  let instrs = trm_inv ~error trm_seq_inv seq in
  let begins_stack = ref [] in (* stack of open begins vars and indices *)
  let to_delete = ref [] in (* upwards list of indices *)
  let populate_to_delete i instr =
    let i_with_delete = i - List.length !to_delete in
    match trm_ghost_begin_inv instr with
    | Some (gv, _, _) ->
      begins_stack := (gv, i, i_with_delete) :: !begins_stack;
    | None ->
      begin match trm_ghost_end_inv instr with
      | Some gv ->
        begin match !begins_stack with
        | (gv_beg, i_beg, i_beg_with_delete) :: bs_rest when gv = gv_beg ->
          begins_stack := bs_rest;
          if i_beg_with_delete + 1 == i_with_delete then
            to_delete := i :: i_beg :: !to_delete
        | _ ->
          (* unbalanced ghost pairs, or no matching ghost begin *)
          ()
        end
      | None -> ()
      end
  in
  Mlist.iteri populate_to_delete instrs;
  to_delete := List.rev !to_delete; (* downwards list of indices *)
  let instrs' = Mlist.filteri (fun i _ ->
    match !to_delete with
    | tdi :: tdr when i = tdi ->
      to_delete := tdr;
      false
    | _ ->
      true
  ) instrs in
  trm_seq ~annot:seq.annot ?loc:seq.loc instrs'

(** Minimizes the scope of ghost pairs in the given sequence. *)
let minimize_ghost_scopes_on (seq : trm) : trm =
  (* Transfo_debug.trm "before move begins" seq; *)
  let seq = move_all_begins_downwards seq in
  (* Transfo_debug.trm "after move begins" seq; *)
  let seq = move_all_ends_upwards seq in
  (* Transfo_debug.trm "after move ends" seq; *)
  let seq = cancel_all_ghost_pairs seq in
  (* Transfo_debug.trm "after cancel" seq; *)
  seq

(** Minimizes the scope of ghost pairs in the targeted sequence. *)
let%transfo minimize_ghost_scopes (tg : target) : unit =
  recompute_all_resources ();
  Target.apply_at_target_paths minimize_ghost_scopes_on tg;
  Trace.apply Scope.infer_var_ids (* FIXME: move up/down should avoid breaking scopes *)

(* [show] enables to view the result of resource computations. *)
let show (*LATER?(details:bool=true)*) ?(line:int = -1) () : unit =
  let t = Trace.ast() in
  let t = Scope.infer_var_ids t in (* Resource computation needs var_ids to be calculated *)
  let tres = Resource_computation.(trm_recompute_resources empty_resource_set t) in
  show_computed_res ~line ~ast:tres ()
