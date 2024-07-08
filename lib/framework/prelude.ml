include Optitrust_utils
include Optitrust_ast
include Ast
include Trm
include Typ
include Contextualized_error
include Mark
include Target

module ShowAt = Show.At

let trm_seq_nobrace = Nobrace.trm_seq
let trm_seq_nobrace_nomarks = Nobrace.trm_seq_nomarks

type seq_component =
  | Trm of trm
  | TrmList of trm list
  | TrmMlist of trm mlist
  | Mark of mark

let trm_seq_helper ?(annot : trm_annot option) ?(loc : location) ?(braces = true) (components: seq_component list) : trm =
  let mlist = List.fold_right (fun comp acc ->
    let res = match comp with
    | Trm t -> Mlist.push_front t acc
    | TrmList tl -> Mlist.merge (Mlist.of_list tl) acc
    | TrmMlist tml -> Mlist.merge tml acc
    | Mark "" -> acc
    | Mark m -> Mlist.insert_mark_at 0 m acc
    in
    (* DEBUG:
      Show.trm ~msg:"res" (trm_seq res); *)
    res
  ) components Mlist.empty in
  if braces then
    trm_seq ?annot ?loc mlist
  else begin
    assert (annot = None);
    assert (loc = None);
    trm_seq_nobrace mlist
  end

let skip_includes (t : trm) : trm =
  match trm_seq_inv t with
  | Some instrs ->
    let not_include = Mlist.filter (fun t -> not (trm_is_include t)) instrs in
    trm_seq not_include
  | None -> t

(* TODO: reflect on the API implications of #var-id (e.g. where this function is called) *)
let find_var_in_current_ast ?(target : target = []) (name : string) : var =
  let vars =
    if target = [] then trm_def_or_used_vars (skip_includes (Trace.ast ()))
    else List.fold_left (fun acc p ->
      Var_set.union acc (trm_def_or_used_vars (Target.resolve_path p))
    ) Var_set.empty (resolve_target target)
  in
  let candidates = Var_set.filter (fun v -> v.namespaces = [] && v.name = name) vars in
  match Var_set.cardinal candidates with
  | 0 -> failwith "could not find variable '%s' in current AST variables: %s" name (vars_to_string (Var_set.elements vars))
  | 1 -> Var_set.choose candidates
  | n -> failwith "%d variables with name '%s' found in current AST: %s" n name (vars_to_string (Var_set.elements candidates))

let foreach_target (tg : target) (f : constr -> unit) : unit =
  Target.iter (fun p -> f (cTarget (target_of_path p))) tg

(* TODO: DEPRECATE *)
let assert_transfo_error (msg : string) (f : unit -> unit) : unit =
  try f () with
  | Contextualized_error (_, Failure msg2) -> assert (msg = msg2)

(** [AstParser]: module for integrating pieces of code given as input by the user. *)
module AstParser = struct
  let var v = trm_var (name_to_var v)
  (* DEPRECATED: (find_var_in_current_ast v) *)

  let var_mut v = trm_var_get (name_to_var v)
  (* DEPRECATED: (find_var_in_current_ast v) *)

  let lit l = code (Lit l)

  let ty ty = typ_arbitrary ty

  let subst_dollar_number (inst : var list) (s : string) : string =
    failwith "#var-id"
  (* List.fold_lefti (fun i acc insti ->
    Tools.string_subst ("${"^(string_of_int i) ^ "}") insti acc
  ) s inst
*)
  let expr ?(vars : var list = []) (e : string)  =
    let e = if vars = [] then e else subst_dollar_number vars e in
    code (Expr e)

  let stmt s = code (Stmt s)

  let instr s = code (Instr s)

end
include AstParser
