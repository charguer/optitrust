include Optitrust_utils
include Optitrust_ast
include Ast
include Trm
include Typ
include Contextualized_error
include Mark
include Target
include Trm_pattern

module Trm = struct
  include Trm
  (* short aliases *)
  let var = trm_var
  let struct_access = trm_struct_access
  let array_access = trm_array_access
  let get_stringreprid = trm_get_stringreprid

  let pattern_var = trm_pattern_var
end

module ShowAt = Show.At

let trm_seq_nobrace = Nobrace.trm_seq
let trm_seq_nobrace_nomarks = Nobrace.trm_seq_nomarks

type seq_component =
  | Trm of trm
  | TrmList of trm list
  | TrmMlist of trm mlist
  | Mark of mark
  | MarkList of mark list
  | SeqComponents of seq_component list

let trm_seq_helper ?(annot : trm_annot option) ?(loc : location) ?(result: var option) ?(braces = true) (components: seq_component list) : trm =
  let rec aux cs acc = List.fold_right (fun comp acc ->
    match comp with
    | Trm t -> Mlist.push_front t acc
    | TrmList tl -> Mlist.merge (Mlist.of_list tl) acc
    | TrmMlist tml -> Mlist.merge tml acc
    | Mark "" -> acc
    | Mark m -> Mlist.insert_mark_at 0 m acc
    | MarkList ms -> Mlist.insert_marks_at 0 ms acc
    | SeqComponents cs -> aux cs acc
  ) cs acc in
  let mlist = aux components Mlist.empty in
  if braces then
    trm_seq ?annot ?loc ?result mlist
  else begin
    assert (annot = None);
    assert (loc = None);
    assert (result = None);
    trm_seq_nobrace mlist
  end

let update_span_helper (span : Dir.span) (t_seq : trm) (f : trm mlist -> seq_component list) : trm =
  let instrs, result = trm_inv ~error:"expected seq" trm_seq_inv t_seq in
  if span.start >= span.stop then begin
    t_seq
  end else begin
    let (span_instrs, instrs_after) = Mlist.split ~left_bias:false span.stop instrs in
    let (instrs_before, span_instrs) = Mlist.split ~left_bias:true span.start span_instrs in
    let new_span_components = f span_instrs in
    trm_seq_helper ~annot:t_seq.annot ?result [
      TrmMlist instrs_before;
      SeqComponents new_span_components;
      TrmMlist instrs_after;
    ]
  end

let skip_includes (t : trm) : trm =
  match trm_seq_inv t with
  | Some (instrs, None) ->
    let not_include = Mlist.filter (fun t -> not (trm_is_include t)) instrs in
    trm_seq not_include
  | _ -> failwith "skip_includes should be called on the root of the AST"

let rec find_var_filter_on (candidates : typ option varmap ref) (filter : var -> bool) (t : trm) : unit =
  let update_map v ty =
    if filter v
    then begin
      candidates := Var_map.update v (fun prev_ty_entry ->
        match Option.join prev_ty_entry with
        | None -> Some ty
        | Some ty -> Some (Some ty)
      ) !candidates
    end
  in
  Pattern.pattern_match t [
    Pattern.(trm_var !__) (fun v () -> update_map v t.typ);
    Pattern.(trm_let !__ !__ __) (fun v typ () -> update_map v (Some typ));
    Pattern.(trm_let_fun !__ __ __ __ __) (fun v () -> update_map v None);
    Pattern.(trm_for !__ __ __) (fun range () -> update_map range.index range.start.typ);
    Pattern.__ (fun () -> ())
  ];
  trm_iter (find_var_filter_on candidates filter) t

let find_var_filter ?(target : target = []) (filter : var -> bool) : var * typ option =
  let candidates = ref Var_map.empty in
  if target = []
  then find_var_filter_on candidates filter (skip_includes (Trace.ast ()))
  else List.iter (fun p ->
    find_var_filter_on candidates filter (Target.resolve_path p)
  ) (resolve_target target);
  (* let candidates = Var_set.filter filter vars in *)
  match Var_map.cardinal !candidates with
  | 0 -> failwith "could not find variable in current AST variables" (* ": %s" (vars_to_string (Var_set.elements vars)) *)
  | 1 -> Var_map.choose !candidates
  | n -> failwith "%d variables found in current AST: %s" n (vars_to_string (List.map fst (Var_map.bindings !candidates)))

let find_var (name : string) (target : target) : var * typ option =
  find_var_filter ~target (fun v -> v.namespaces = [] && v.name = name)

let trm_find_var (name : string) (target : target) : trm =
  let (v, typ) = find_var name target in
  trm_var ?typ v

let find_typ_var (name : string) (target : target) : var =
  fst (find_var_filter ~target (fun v -> v.namespaces = typ_namespace && v.name = name))

let typ_find_var (name  : string) (target : target) : typ =
  typ_var (find_typ_var name target)

(* TODO: DEPRECATE *)
let assert_transfo_error (msg : string) (f : unit -> unit) : unit =
  try f () with
  | Contextualized_error (_, Failure msg2) -> assert (msg = msg2)

(** [AstParser]: module for integrating pieces of code given as input by the user. *)
module AstParser = struct
  let var v = trm_find_var v []

  let var_mut v = trm_get (var v)

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
