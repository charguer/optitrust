open Ast
open Trm
open Trm_unify
open Typ
open Contextualized_error
open Resource_formula
open Resource_contract

type range = var * trm
(** A [Range] represents the iterator on resources, it is composed by a [var] that iterates within a [trm] range *)

type starindex =
  | Star of range * trm
  | Index of trm
      (**
   A [Star] represents a permission on an entire group.
   The [range] represents the inf,sup and iterator.
   The [index] represents the accessed cell. Look at group_repr for an example
   Example: for i in r -> .


   An [Index] represents a permission on a specific cell in an array.

   A [starindex] represents a permission on a single dimension of an array.
   You can have either:
     - Permission on the entire dimension: for i in r -> &t[MINDEX(..., i, ...)]
     - Permission on a single index of this dimension.
*)

type group_repr = starindex list
(** [group_repr] is the internal representation of a permission on an array : For each dimension you can either have a [Star] or an [Index], the term [t] represents the array base
Example : for i1 in 0..n1 -> &x[MINDEX1(n1,n2,f(i1),2)] ---> [Star(0,n1,i1,f(i1)), Index(2)],x  *)

type focus_list = (group_repr * group_repr * (var * trm)) list
(**
   [focus_list] is the internal representation of the transformation
   that allows us to go from the required resources to the resources we have.

   Each entry is a quadruple: [group_repr] * [group_repr] * [var] * [trm],
   which specifies how to transform the first group into the second
   by instantiating [var] with [trm].
*)

type ghosts = {
  ghost_begin : trm list;
  ghost_end : trm list;
}

(**
   [ghosts] is the representation of the transformation
   that allows us to go from the required resources to the resources we have using ghosts.
   Composed by two lists of trm_apps(ghost) that must be must before and after the trm that need resource focus
*)
let empty_ghost = { ghost_begin = []; ghost_end = [] }

(** [DEBUGGING]  *)
let debug = false

let print_trm_string (t : trm) : string =
  let doc = Ast_to_text.print_trm Ast_to_text.default_style t in
  Tools.document_to_string doc

let print_range ((a, b) : range) : string = Printf.sprintf "(%s, %s)" a.name (print_trm_string b)

let print_starindex (s : starindex) : string =
  match s with
  | Index i -> Printf.sprintf "Index(%s)" (print_trm_string i)
  | Star (r, i) -> Printf.sprintf "Star(%s, %s)" (print_range r) (print_trm_string i)

let print_group_repr (stars : group_repr) : string =
  let stars_str = stars |> List.map print_starindex |> String.concat "; " in
  Printf.sprintf "([%s])" stars_str

let string_of_evar_resolution = function Resolved t -> print_trm_string t | Unknown _ -> "Unknown"

let string_of_unification_ctx (ctx : 'a unification_ctx) : string =
  let bindings =
    Var_map.bindings ctx
    |> List.map (fun (v, res) ->
           Printf.sprintf " %d %s -> %s" v.id v.name (string_of_evar_resolution res)
       ) in
  "{ " ^ String.concat "; " bindings ^ " }"

(** [GROUP EXTRACTION] *)

(** [range_check]: Checks that the [range] ~(inf,sup,ite) is such that inf = 0, the sup unifies with [dim] and the iteration is one  *)
let range_check (range : trm) (dim : trm) : bool =
  match formula_range_inv range with
  | Some (start, stop, _ite) -> are_same_trm start (trm_int 0) && are_same_trm stop dim
  | _ ->
    Printf.printf "isssue with inversion \n";
    false

(** [extract_ranges]: Will extract the groups and the basic cell accessed for future processing  *)
let rec extract_ranges (formula : trm) : (range list * trm) option =
  match formula_group_inv formula with
  | Some (index, range, body) -> (
    match extract_ranges body with
    | Some (range_list, t) -> Some ([ (index, range) ] @ range_list, t)
    | _ -> None
  )
  | _ -> (
    match formula_cell_inv formula with
    | Some t -> Some ([], t)
    | _ -> (
      match formula_uninit_cell_inv formula with Some t -> Some ([], t) | _ -> None
    )
  )

(** [to_group_repr]: Build group_repr representation of the groups / and indices
Used to compute list of focus in the build_focus function
We are trying to find for each indice, a group that can be binded to it.
For every indice : find the sets of used variables, try to find the intersection of used vars and groups that
If the set S of intersection is > 1 then we abort
If there is exaclty one match then the group is binded to this index (Star)
If there is no match, then we procuce an index;
In order to be able to produce a group_repr, range must iterate over the full length of the dimension concerned  *)
let to_group_repr (range_list : (var * trm) list) (indices : trms) (dims : trms) (t_base : trm) :
    (group_repr * trm) option =
  let indices_dims = List.combine indices dims in
  let rec aux range_list group indices_dims =
    match indices_dims with
    | [] ->
      let t_base =
        List.fold_left (fun acc (var, range) -> formula_group var range acc) t_base range_list in
      Some (List.rev group, t_base)
    | (indice, dim) :: rest -> (
      let possible_group =
        List.filter (fun (var, range) -> Var_set.mem var (trm_used_vars indice)) range_list in
      match possible_group with
      | [] -> aux range_list (Index indice :: group) rest
      | [ (var, range) ] ->
        if range_check range dim then
          let current_group = List.remove (var, range) range_list in
          aux current_group (Star ((var, range), indice) :: group) rest
        else
          None
      | _ -> None
    ) in
  aux range_list [] indices_dims

(** [group_repr_inv]: Inversion fonction to go from group_repr to a formula (trm)
You need the [t_base] and [dims] to be able to rebuild the trm corresponding to the cell*)
let group_repr_inv ~(frac : trm option) (group : group_repr) (t_base : trm) (dims : trms) : formula
    =
  let list_access =
    List.map
      (fun starindex ->
        match starindex with Index trm_index -> trm_index | Star (range, trm_index) -> trm_index
      )
      group in
  let access_trm = Matrix_trm.access t_base dims list_access in
  let cell = formula_cell access_trm in
  let formula_inv =
    List.fold_right
      (fun star_index formula ->
        match star_index with
        | Star (range, trm) ->
          let var, range = range in
          formula_group var range formula
        | Index i -> formula
      )
      group cell in
  if Option.is_some frac then
    formula_read_only ~frac:(Option.get frac) formula_inv
  else
    formula_inv

(** [var_group_susbt]: Subsitute the group variables (i1,i2..) from group to the one used in group_candidate.
We need to performs some substitution to be able to compare & unify trm_indices *)
let var_group_subst (group : group_repr) (group_candidate : group_repr) : group_repr =
  let concat = List.combine group group_candidate in
  let mapping =
    List.filter_map
      (fun (s1, s2) ->
        match (s1, s2) with
        | Star ((var, range), _t1), Star ((var_candidate, range_candidate), _t2) ->
          Some (var, var_candidate)
        | _, _ -> None
      )
      concat in
  let subst_var = Var_map.of_seq (List.to_seq mapping) in
  List.map
    (fun starindex ->
      match starindex with
      | Index trm_index -> Index (trm_vars_subst subst_var trm_index)
      | Star ((var, range), trm_index) ->
        Star ((Var_map.find var subst_var, range), trm_vars_subst subst_var trm_index)
    )
    group

(** [trms_unify] : Unification of two trm list. Unification is done trm by trm *)
let rec trms_unify
    (l1 : trm list)
    (l2 : trm list)
    (evar_ctx : 'a unification_ctx)
    (validate_inst : trm -> 'a -> 'a unification_ctx -> 'a unification_ctx option) :
    'a unification_ctx option =
  let open Option.Monad in
  match (l1, l2) with
  | [], [] -> Some evar_ctx
  | t1 :: q1, t2 :: q2 ->
    let* evar_ctx = trm_unify t1 t2 evar_ctx validate_inst in
    trms_unify q1 q2 evar_ctx validate_inst
  | _, _ -> None

(** [COMPARAISON FUNCTION]*)

(** [is_focusable star index] : determines whether a given star can be focused on a specific element
    Criteria for focus: This fonction determines whether
    for i in r -> H(i) can be focused into H'
    The function returns Some(t) if H(t) unifies with H', meaning that the star on i is focused on index t *)
let is_focusable
    ((range, formula) : range * formula)
    (trm_index : trm)
    (evar_ctx : 'a unification_ctx) : (var * trm) option =
  let open Option.Monad in
  let var_star_index, range = range in
  let evar_ctx = Var_map.(empty |> add var_star_index (Unknown ())) in
  (* i is directly a var *)
  let* evar_ctx = trm_unify formula trm_index evar_ctx (fun _ _ ctx -> Some ctx) in

  match Var_map.find var_star_index evar_ctx with
  | Unknown () -> None
  | Resolved t -> Some (var_star_index, t)

(** [unify_range] : Determines if TODO *)
let unify_range (range1 : range) (range2 : range) (evar_ctx : 'a unification_ctx) validate_inst :
    'a unification_ctx option =
  let var1, range_trm1 = range1 in
  let var2, range_trm2 = range2 in
  trm_unify range_trm1 range_trm2 evar_ctx validate_inst

(** [are_same_group_repr] : Two groups are the same, if the base trm they refer to are the same and every item in the star_index list are the same *)
let are_same_group_repr (stars1 : group_repr) (stars2 : group_repr) : bool =
  List.length stars1 = List.length stars2
  && List.for_all2
       (fun s1 s2 ->
         match (s1, s2) with
         | Index i1, Index i2 -> are_same_trm i1 i2
         | Star (range1, i1), Star (range2, i2) -> are_same_trm i1 i2
         | _, _ -> false
       )
       stars1 stars2

(** [are_same_focus_list] : Compares and returns if the two focus list at hand are the same.  *)
let are_same_focus_list (result : focus_list) (expected : focus_list) : bool =
  List.length result = List.length expected
  && List.for_all2
       (fun (g1a, g1b, (v1, t1)) (g2a, g2b, (v2, t2)) ->
         are_same_group_repr g1a g2a
         && are_same_group_repr g1b g2b
         && are_same_trm t1 t2
         && v1.name = v2.name
       )
       result expected

(** [ALGO]  *)
let evar_subst (t : trm) (evar_ctx : 'a unification_ctx) : trm =
  let resolved_varmap =
    Var_map.fold
      (fun v res acc -> match res with Resolved t -> Var_map.add v t acc | Unknown _ -> acc)
      evar_ctx Var_map.empty in
  trm_subst resolved_varmap t

let update_group (var : var) (resolved_trm : trm) (group : group_repr) : group_repr =
  List.map
    (fun starindex ->
      match starindex with
      | Star (range, trm_index) -> Star (range, trm_subst_var var resolved_trm trm_index)
      | Index trm_index -> Index (trm_subst_var var resolved_trm trm_index)
    )
    group

(** [build focus list]: Tries to build a [focus_list], i.e list of pairs of [group repr] tha represents unitary focuses that allows to goes from [from_group] to [to _group] *
Each focus : H_i -> H_i+1)
This function assumes that the from_group and to_group have been normalized already by the caller.
Criteria for focus is described in the [is_focusable] function
If the result is Some [F_1;..;F_N] then F_i represents a ghost operation that consumes H_i and produces H_i+1, therefore the sequence F1;..Fn consumes H_1 = from_group and produces H_n+1 = to_group.

*)
let build_focus_list
    (stars_from : group_repr)
    (stars_to : group_repr)
    (evar_ctx : 'a unification_ctx)
    (validate_inst : trm -> 'a -> 'a unification_ctx -> 'a unification_ctx option) :
    focus_list option =
  let open Option.Monad in
  if not (List.length stars_from == List.length stars_to) then begin
    None
  end else
        let folder
            (acc : (starindex list * focus_list * int * 'a unification_ctx) option)
            (si_to : starindex) =
          match acc with
          | None -> None
          | Some (current_group, acc_focus, ind, evar_ctx) -> (
            let si_from = List.nth current_group ind in
            match (si_from, si_to) with
            | Index _, Star _ ->
              if debug then Printf.printf "Index, Star \n";
              None
            | Star (range1, index_trm1), Star (range2, index_trm2) ->
              let* evar_ctx = trm_unify index_trm2 index_trm1 evar_ctx validate_inst in
              let* evar_ctx = unify_range range1 range2 evar_ctx validate_inst in
              Some (current_group, acc_focus, ind + 1, evar_ctx)
            | Index i1, Index i2 ->
              let* evar_ctx = trm_unify i1 i2 evar_ctx validate_inst in
              Some (current_group, acc_focus, ind + 1, evar_ctx)
            | Star (range, trm_index), Index i2 ->
              if debug then
                Printf.printf "starindex starindex  %s %s \n" (print_trm_string trm_index)
                  (print_trm_string i2);
              let* var, resolved_trm = is_focusable (range, trm_index) i2 evar_ctx in
              let new_group = update_group var resolved_trm current_group in
              let new_group = List.update_nth ind (fun t -> Index i2) new_group in
              Some
                ( new_group,
                  acc_focus @ [ (current_group, new_group, (var, resolved_trm)) ],
                  ind + 1,
                  evar_ctx
                )
          ) in
        Option.map
          (fun (a, b, c, d) -> b)
          (List.fold_left folder (Some (stars_from, [], 0, evar_ctx)) stars_to)

(* let handle_read_only (frac : trm) (formula : trm) : trms * trms * trm option =
  let autofoc = formula_frac_div frac (trm_int 2) in
  let ghosts =
    [
      Resource_trm.ghost_admitted
        {
          pre = Resource_set.make ~linear:[ (new_anon_hyp (), formula_read_only ~frac formula) ] ();
          post =
            Resource_set.make
              ~linear:
                [
                  (new_anon_hyp (), formula_read_only ~frac:autofoc formula);
                  (new_anon_hyp (), formula_read_only ~frac:autofoc formula);
                ]
              ();
        };
    ] in
  let to_join = formula_frac_sub frac autofoc in
  let rev_ghosts =
    [
      Resource_trm.ghost_admitted
        {
          pre =
            Resource_set.make
              ~linear:[ (new_anon_hyp (), formula_read_only ~frac:autofoc formula) ]
              ();
          post =
            Resource_set.make
              ~linear:[ (new_anon_hyp (), formula_read_only ~frac:to_join formula) ]
              ();
        };
    ] in
  (ghosts, rev_ghosts, Some autofoc) *)

let group_repr_to_closure
    ~(frac : trm option)
    ?(rev = false)
    (focus_list : focus_list)
    (t_base : trm)
    (dims : trms) : trm list =
  List.map
    (fun (from_group, to_group, (var, index)) ->
      let from_group, to_group = if rev then (to_group, from_group) else (from_group, to_group) in
      let contract =
        {
          pre =
            Resource_set.make
              ~linear:[ (new_anon_hyp (), group_repr_inv ~frac from_group t_base dims) ]
              ();
          post =
            Resource_set.make
              ~linear:[ (new_anon_hyp (), group_repr_inv ~frac to_group t_base dims) ]
              ();
        } in
      trm_fun [] typ_auto ~contract:(FunSpecContract contract)
        (trm_seq_nomarks [ Resource_trm.admitted () ])
    )
    focus_list

let group_repr_to_ghost_pairs
    ~(frac : trm option)
    ?(rev = false)
    (focus_list : focus_list)
    (t_base : trm)
    (dims : trms) : trm list * trm list =
  let contracts_fn = group_repr_to_closure ~frac focus_list t_base dims in
  let contracts_bw = group_repr_to_closure ~frac ~rev:true focus_list t_base dims in
  let ghosts_begin, ghosts_end =
    List.split
      (List.map
         (fun (contract_fn, contract_bw) ->
           let _g_var, g_begin, g_end = Resource_trm.ghost_custom_pair contract_fn contract_bw in
           (g_begin, g_end)
         )
         (List.combine contracts_fn contracts_bw)
      ) in
  let ghosts_end = List.rev ghosts_end in
  (ghosts_begin, ghosts_end)

let handle_reorder
    ~(frac : trm option)
    (ghosts : trm list)
    (rev_ghosts : trm list)
    (group : group_repr)
    (group_candidate : group_repr)
    (formula : trm)
    (formula_candidate : formula)
    (t_base_candidate : trm)
    (t_base : trm)
    (dims_candidate : trms)
    (dims : trms) =
  let formula, formula_candidate =
    match frac with
    | Some frac -> (formula_read_only ~frac formula, formula_read_only ~frac formula_candidate)
    | None -> (formula, formula_candidate) in
  let reordered_candidate = group_repr_inv ~frac group_candidate t_base_candidate dims_candidate in
  let reordered_target = group_repr_inv ~frac group t_base dims in

  let ghosts, rev_ghosts =
    if not (are_same_trm reordered_candidate formula_candidate) then
      let contract =
        {
          pre = Resource_set.make ~linear:[ (new_anon_hyp (), formula_candidate) ] ();
          post = Resource_set.make ~linear:[ (new_anon_hyp (), reordered_candidate) ] ();
        } in
      let rev_contract = { pre = contract.post; post = contract.pre } in
      ( Resource_trm.ghost_admitted contract :: ghosts,
        Resource_trm.ghost_admitted rev_contract :: rev_ghosts
      )
    else
      (ghosts, rev_ghosts) in
  let ghosts, rev_ghosts =
    if not (are_same_trm reordered_target formula) then
      let contract =
        {
          pre = Resource_set.make ~linear:[ (new_anon_hyp (), reordered_target) ] ();
          post = Resource_set.make ~linear:[ (new_anon_hyp (), formula) ] ();
        } in
      let rev_contract = { pre = contract.post; post = contract.pre } in
      ( ghosts @ [ Resource_trm.ghost_admitted contract ],
        Resource_trm.ghost_admitted rev_contract :: rev_ghosts
      )
    else
      (ghosts, rev_ghosts) in
  (ghosts, rev_ghosts)

let handle_frac (frac:trm option) : trm option =
  match frac with
  |Some(f) -> Some(formula_frac_div f (trm_int 2))
  | _ -> None
(**
  [autofocus_unify] : tries to see if formula and formula_to_unify can be unified if we add some ghosts instructions to satisfy the resource needed
  [formula] : might describe an access to a cell or a group
  [formula_candidate] : must be a group (if it's a Cell it has been unified on first round of [subtract_resource_item]
    formula and formula will be parsed,reorder and renamed in order to search for possible focus
  This function first checks that the resources are on an array and on the same array.
  Then it unifies the dimensions
  Then it tries to see if a list of ghost_pair can be
     established around the instruction, that allows to focus the [formula_candidate] into the [formula].
  Returns the updated evar_ctx and the ghosts if the operation succeeded *)

let autofocus_unify
    ?(frac = None)
    (formula : trm)
    (formula_candidate : trm)
    (evar_ctx : 'a unification_ctx)
    (validate_inst : trm -> 'a -> 'a unification_ctx -> 'a unification_ctx option) :
    (ghosts * 'a unification_ctx) option =
  let open Option.Monad in
  (* Subsitution needed for get / set operation, we substitute everything  *)
  let formula = evar_subst formula evar_ctx in
  let formula = formula_init formula in
  let formula_candidate = evar_subst formula_candidate evar_ctx in

  (* Group extraction for group and group candidate *)
  let* ranges_candidate, t_candidate = extract_ranges formula_candidate in
  let* ranges, t = extract_ranges formula in
  (* normalization and unification checks *)
  let t, evar_ctx = normalize_trm t evar_ctx validate_inst in
  let t_candidate, evar_ctx = normalize_trm t_candidate evar_ctx validate_inst in
  let* t_base_candidate, dims_candidate, indices_candidates = Matrix_trm.access_inv t_candidate in
  let* t_base, dims, indices = Matrix_trm.access_inv t in
  let* evar_ctx = trm_unify t_base_candidate t_base evar_ctx validate_inst in
  let* evar_ctx = trms_unify dims_candidate dims evar_ctx validate_inst in
  let t_base, evar_ctx = unfold_if_resolved_evar t_base evar_ctx in (* Unfolding needed for ghost creation in candidate context *)

  (* Conversion to group_repr *)
  let* group, t_base = to_group_repr ranges indices dims t_base in
  let* group_candidate, t_base_candidate =
    to_group_repr ranges_candidate indices_candidates dims_candidate t_base_candidate in
  let group = var_group_subst group group_candidate in
  let* focus_list = build_focus_list group_candidate group evar_ctx validate_inst in
  (* group_repr -> formula *)
  let frac = handle_frac frac in

  let ghosts_focus, rev_ghosts_focus =
    group_repr_to_ghost_pairs ~frac focus_list t_base_candidate dims_candidate in
  (* Adding ghosts for reordering whenever it is needed *)
  let ghosts, rev_ghosts =
    handle_reorder ~frac ghosts_focus rev_ghosts_focus group group_candidate formula formula_candidate
      t_base_candidate t_base dims_candidate dims in
  Some ({ ghost_begin = ghosts; ghost_end = rev_ghosts }, evar_ctx)

(** [GHOST_CREATION] *)

let ghost_begin ghosts = ghosts.ghost_begin
let ghost_end ghosts = ghosts.ghost_end

(** [seq_from_ghosts_list] : Returns the trm_seq corresponding to the full sequence needde to type the resource that needed focus  *)
let seq_from_ghosts_list (t : trm) (gl : ghosts list) : trm =
  let ghosts_before = List.concat (List.map (fun ghosts -> ghosts.ghost_begin) gl) in
  let ghosts_after = List.concat (List.map (fun ghosts -> ghosts.ghost_end) gl) in
  (* let tmp = trm_let (new_var "tmp")  *)
  let tmp = new_var "autofocus_tmp" in
  if Option.is_some t.typ then begin
    let result = trm_let (tmp, Option.get t.typ) t in
    trm_seq ~result:tmp (Mlist.of_list (ghosts_before @ [ result ] @ ghosts_after))
  end else begin
        trm_seq (Mlist.of_list (ghosts_before @ [ t ] @ ghosts_after))
      end

(** [elaborate] : check if any trm in the ast as Some elaboration to do and replace the trm accordingly. For now it's replaced with ghost before and after the trm coming from the autofocus algorithm  *)
let elaborate (t :trm)  =
Printf.printf "Elaboration \n";
trm_bottom_up  ~keep_ctx:true (fun t -> match t.ctx.elaborate with
  | Some { pre_ghost; post_ghost} -> (
    t.ctx.elaborate <- None;
    let new_seq = seq_from_ghosts_list t [{ghost_begin = pre_ghost; ghost_end = post_ghost}] in
    Mark.trm_add_mark "autofoc_seq" new_seq)
  | _ -> t ) t
