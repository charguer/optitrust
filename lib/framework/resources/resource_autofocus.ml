open Ast
open Trm
open Trm_unify
open Typ
open Contextualized_error
open Resource_formula
open Resource_contract

type range = var * trm

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
type starindex =
  | Star of range * trm
  | Index of trm

(** [group_repr] is the internal representation of a permission on an array : For each dimension you can either have a [Star] or an [Index], the term [t] represents the array base
Example : for i1 in 0..n1 -> &x[MINDEX1(n1,n2,f(i1),2)] ---> [Star(0,n1,i1,f(i1)), Index(2)],x  *)

type group_repr = starindex list

type focus_list = (group_repr * group_repr * (var * trm)) list
(**
   [focus_list] is the internal representation of the transformation
   that allows us to go from the required resources to the resources we have.

   Each entry is a quadruple: [group_repr] * [group_repr] * [var] * [trm],
   which specifies how to transform the first group into the second
   by instantiating [var] with [trm].
*)

type ghosts = (formula * formula * (var * trm)) list
(**
   [ghosts] is the representation of the transformation
   that allows us to go from the required resources to the resources we have using ghosts.

   Each entry is a quadruple: [formula] * [formula] * [var] * [trm],
   which specifies how to transform the first resources into the second one, instantiating var with trm.
   Theses quadruple will be need to build the ghosts around the instructions to focus the resources.
*)

(** [DEBUGGING]  *)
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

(** [GROUP EXTRACTION] *)

(** [extract_group]: Will extract the groups and the basic cell accessed for future processing  *)
let rec extract_group (formula : trm) : (range list * trm * bool) option =
  match formula_group_inv formula with
  | Some (index, range, body) -> (
    match extract_group body with
    | Some (range_list, t, uninit) -> Some ([ (index, range) ] @ range_list, t, uninit)
    | _ -> None
  )
  | _ -> (
    match formula_cell_inv formula with
    | Some t -> Some ([], t, false)
    | _ -> (
      match formula_uninit_cell_inv formula with Some t -> Some ([], t, false) | _ -> None
    )
  )

(** [to_group_repr]: Build group_repr representation of the groups / and indices
Used to compute list of focus in the build_focus function *)

(* We are trying to find for each indice, a group that can be binded to it.
For every indice : find the sets of used variables, try to find the intersection of used vars and groups that
If the set S of intersection is > 1 then we abort
If there is exaclty one match then the group is binded to this index (Star)
If there is no match, then we procuce an index  *)
let to_group_repr (group : (var * trm) list) (indices : trms) (t_base : trm) :
    (group_repr * trm) option =
  let rec aux current_group star_index_list = function
    | [] ->
      let t_base =
        List.fold_left (fun acc (var, range) -> formula_group var range acc) t_base current_group
      in
      Some (List.rev star_index_list, t_base)
    | indice :: rest -> (
      let possible_group =
        List.filter (fun (var, range) -> Var_set.mem var (trm_used_vars indice)) current_group in
      match possible_group with
      | [] -> aux current_group (Index indice :: star_index_list) rest
      | [ (var, range) ] ->
        let current_group = List.remove (var, range) current_group in

        aux current_group (Star ((var, range), indice) :: star_index_list) rest
      | _ -> None
    ) in
  aux group [] indices

(* TODO CHANGE *)

(** [group_repr_inv]: Inversion fonction to go from group_repr to a formula (trm) , we can't keep the term because it doesnot make any sens*)
let group_repr_inv ~(uninit : bool) (group : group_repr) (t_base : trm) : formula =
  let star_index_list = group in
  let cell = if uninit then formula_uninit_cell t_base else formula_cell t_base in
  let formula_inv =
    List.fold_left
      (fun formula star_index ->
        match star_index with
        | Star (range, trm) ->
          let var, range = range in
          formula_group var range formula
        | Index i -> formula
      )
      cell star_index_list in
  formula_inv

let var_index (i : int) = toplevel_var (sprintf "__AUTOFOCUS_INDEX_%d" i)

let rename (group : group_repr) : group_repr * var varmap =
  let star_index_list = group in
  let pairs =
    List.filter_map
      (fun (i, starindex) ->
        match starindex with Index _ -> None | Star ((var, range), _) -> Some (var, var_index i)
      )
      (List.mapi (fun i s -> (i, s)) star_index_list) in
  let subst_varmap = Var_map.of_seq (List.to_seq pairs) in
  let inverted_varmap = Var_map.of_seq (List.to_seq (List.map (fun (a, b) -> (b, a)) pairs)) in

  let star_index_list_renamed =
    List.map
      (fun starindex ->
        match starindex with
        | Index index -> Index (trm_vars_subst subst_varmap index)
        | Star (range, index) ->
          let var, range_trm = range in
          Star ((Var_map.find var subst_varmap, range_trm), trm_vars_subst subst_varmap index)
      )
      star_index_list in
  (star_index_list_renamed, inverted_varmap)

let var_group_unify (group : group_repr) (group_candidate : group_repr) : group_repr =
  Printf.printf "in var_group\n";

  let concat = List.combine group group_candidate in
  Printf.printf "end var_group\n";
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
  let _ =
    Var_map.for_all
      (fun key value ->
        Printf.printf "%s %s \n" key.name value.name;
        true
      )
      subst_var in

  let group =
    List.map
      (fun starindex ->
        match starindex with
        | Index trm_index -> Index (trm_vars_subst subst_var trm_index)
        | Star ((var, range), trm_index) ->
          Star ((Var_map.find var subst_var, range), trm_vars_subst subst_var trm_index)
      )
      group in

  Printf.printf "end var_group\n";

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

(** [unfold_list_if_resolved_evar] : Unfold trm in trms if the evar has been resolved *)
let rec unfold_list_if_resolved_evar (trms : trm list) (evar_ctx : 'a unification_ctx) :
    trm list * 'a unification_ctx =
  match trms with
  | [] -> ([], evar_ctx)
  | t :: l ->
    let t, evar_ctx = unfold_if_resolved_evar t evar_ctx in
    let trms, evar_ctx = unfold_list_if_resolved_evar l evar_ctx in
    (t :: trms, evar_ctx)

(** [ALGO]*)

(** [is_focusable star index] : determines whether a given star can be focused on a specific element
    Criteria for focus: This fonction determines whether
    for i in r -> H(i) can be focused into H'
    The function returns Some(t) if H(t) unifies with H', meaning that the star on i is focused on index t *)
let is_focusable ((range, formula) : range * formula) (trm_index : trm) : (var * trm) option =
  let open Option.Monad in
  let var_star_index, range = range in
  let evar_ctx = Var_map.(empty |> add var_star_index (Unknown ())) in
  (* i is directly a var *)
  let* evar_ctx = trm_unify formula trm_index evar_ctx (fun _ _ ctx -> Some ctx) in
  match Var_map.find var_star_index evar_ctx with
  | Unknown () -> None
  | Resolved t -> Some (var_star_index, t)

(** [are_same_range] : Determines if TODO *)
let are_same_range (range1 : range) (range2 : range) : bool = true

(** [are_same_group_repr] : Two groups are the same, if the base trm they refer to are the same and every item in the star_index list are the same *)
let are_same_group_repr (stars1 : group_repr) (stars2 : group_repr) : bool =
  List.length stars1 = List.length stars2
  && List.for_all2
       (fun s1 s2 ->
         match (s1, s2) with
         | Index i1, Index i2 -> are_same_trm i1 i2
         | Star (range1, i1), Star (range2, i2) ->
           are_same_range range1 range2 && are_same_trm i1 i2
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

(** [build focus list]: Tries to build a [focus_list], i.e list of pairs of [group repr] tha represents unitary focuses that allows to goes from [from_group] to [to _group] *
Each focus : H_i -> H_i+1)
This function assumes that the from_group and to_group have been normalized already by the caller.
Criteria for focus is described in the [is_focusable] function
If the result is Some [F_1;..;F_N] then F_i represents a ghost operation that consumes H_i and produces H_i+1, therefore the sequence F1;..Fn consumes H_1 = from_group and produces H_n+1 = to_group.

*)
let build_focus_list (stars_from : group_repr) (stars_to : group_repr) : focus_list option =
  let open Option.Monad in
  if not (List.length stars_from == List.length stars_to) then begin
    None
  end else
        let folder
            (acc : (starindex list * focus_list * int) option)
            (si_from : starindex)
            (si_to : starindex) =
          match acc with
          | None -> None
          | Some (current_group, acc_focus, ind) -> (
            match (si_from, si_to) with
            | Index _, Star _ ->
              Printf.printf "Index, Star \n";
              None
            | Star (range1, index_trm1), Star (range2, index_trm2) ->
              if are_same_range range1 range2 && are_same_trm index_trm1 index_trm2 then
                Some (current_group, acc_focus, ind + 1)
              else begin
                Printf.printf "Index 1 %s \n" (print_trm_string index_trm1);
                Printf.printf "Index 2 %s \n" (print_trm_string index_trm2);
                None
              end
            | Index i1, Index i2 ->
              if are_same_trm i1 i2 then Some (current_group, acc_focus, ind + 1) else None
            | Star (range, index), Index i2 ->
              Printf.printf "Index 1 %s \n" (print_trm_string index);
              Printf.printf "Index 2 %s \n" (print_trm_string i2);

              Printf.printf "Issue inside is_focusable \n";
              let* var, resolved_trm = is_focusable (range, index) i2 in
              Printf.printf "is focusable is ok\n";
              let new_group = List.update_nth ind (fun t -> Index i2) current_group in
              Some
                (new_group, acc_focus @ [ (current_group, new_group, (var, resolved_trm)) ], ind + 1)
          ) in
        Option.map
          (fun (a, b, c) -> b)
          (List.fold_left2 folder (Some (stars_from, [], 0)) stars_from stars_to)

(**
  [autofocus_unify] : tries to see if formula and formula_to_unify can be unified if we add some ghosts instructions to satisfy the resource needed
  [formula] : might describe an access to a cell or a group
  [formula_candidate] : must be a group (if it's a Cell it has been unified on first round of [subtract_resource_item]
    formula and formula will be parsed,reorder and renamed in order to search for possible focus
  This function first checks that the resources are on an array and on the same array.
  Then it unifies the dimensions
  Then it tries to see if a list of ghost_pair can be established around the instruction, that allows to focus the [formula_candidate] into the [formula].
  Returns the updated evar_ctx and the ghosts if the operation succeeded *)

let autofocus_unify
    (formula : trm)
    (formula_candidate : trm)
    (evar_ctx : 'a unification_ctx)
    (validate_inst : trm -> 'a -> 'a unification_ctx -> 'a unification_ctx option) :
    (ghosts * 'a unification_ctx) option =
  let open Option.Monad in
  (* Extract groups from the formulas, returns None if it doesn't fit the autofocus scope *)
  Printf.printf "Enter autofoc unify \n";
  let* group_candidate, t_candidate, uninit_candidate = extract_group formula_candidate in
  let* group, t, uninit = extract_group formula in
  if uninit_candidate && not uninit then
    None
  else (* Inverse the term group are pointing to, returns None if it's not a Matrix access *)
    let* t_base_candidate, dims_candidate, indices_candidates = Matrix_trm.access_inv t_candidate in
    let* t_base, dims, indices = Matrix_trm.access_inv t in
    (* let* evar_ctx = trm_unify t t_candidate evar_ctx validate_inst in *)
    let* evar_ctx = trm_unify t_base_candidate t_base evar_ctx validate_inst in
    let* evar_ctx = trms_unify dims_candidate dims evar_ctx validate_inst in
    let t_base, evar_ctx = unfold_if_resolved_evar t_base evar_ctx in
    let dims, evar_ctx = unfold_list_if_resolved_evar dims evar_ctx in
    Printf.printf "Ended unification phase \n";

    let t = Matrix_trm.access t_base dims indices in
    let* group, t_base = to_group_repr group indices t_base in
    let* group_candidate, t_base_candidate =
      to_group_repr group_candidate indices_candidates t_base_candidate in
    Printf.printf "group repr for group_candidate : %s\n" (print_group_repr group_candidate);
    let group = var_group_unify group group_candidate in
    Printf.printf "group repr for group : %s\n" (print_group_repr group);

    let* focus_list = build_focus_list group_candidate group in
    Printf.printf "Builded focus list\n";
    Printf.printf "%d \n" (List.length focus_list);
    let g_from, g_to, (_, _) = List.nth focus_list 0 in
    Printf.printf "FOCUS LIST %s \n " (print_group_repr g_from);
    Printf.printf "FOCUS LIST %s \n " (print_group_repr g_to);
    let ghosts =
      List.map
        (fun (from_group, to_group, (var, index)) ->
          ( group_repr_inv ~uninit from_group t_base_candidate,
            group_repr_inv ~uninit to_group t_base_candidate,
            (var, index)
          )
        )
        focus_list in
    Some (ghosts, evar_ctx)

(** [GHOST_CREATION] *)

(** [ghosts_formula_begin]: Transforms ghosts into actual calls (trms) to ghost function.
    Returns the list of [trm] representing these ghost calls, used to type-check the expression that required focus. *)
let ghosts_formula_begin (ghosts : ghosts) : trm list =
  List.map
    (fun (formula_from, formula_to, (_, _)) ->
      Resource_trm.ghost_admitted_rewrite formula_from formula_to (trm_var (toplevel_var "focus"))
    )
    ghosts

(* TODO :  *)

(** [ghosts_formula_end]: Transforms ghost placeholders into actual calls to the [ghosts] function.
    Returns the list of [trm] representing these ghost calls, used to restore the resources released during focus. *)
let ghosts_formula_end (ghosts : ghosts) =
  List.map
    (fun (formula_from, formula_to, (_, _)) ->
      Resource_trm.ghost_admitted_rewrite formula_to formula_from (trm_var (toplevel_var "focus"))
    )
    (List.rev ghosts)

(** [seq_from_ghosts_list] : Returns the trm_seq corresponding to the full sequence needde to type the resource that needed focus  *)
let seq_from_ghosts_list (t : trm) (gl : ghosts list) : trm =
  let ghosts_before = List.concat (List.map (fun ghosts -> ghosts_formula_begin ghosts) gl) in
  let ghosts_after = List.concat (List.map (fun ghosts -> ghosts_formula_end ghosts) gl) in
  trm_seq (Mlist.of_list (ghosts_before @ [ t ] @ ghosts_after))
