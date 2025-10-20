open Ast
open Trm
open Trm_unify
open Typ
open Contextualized_error
open Resource_formula
open Resource_contract

type range = var * trm
type index = trm

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
  | Star of range * index
  | Index of index

(** [group_repr] is the internal representation of a permission on an array : For each dimension you can either have a [Star] or an [Index], the term [t] represents the array base
Example : for i1 in 0..n1 -> &x[MINDEX1(n1,n2,f(i1),2)] ---> [Star(0,n1,i1,f(i1)), Index(2)],x  *)

type group_repr = starindex list * trm

(**
   [focus_list] is the internal representation of the transformation
   that allows us to go from the required resources to the resources we have.

   Each entry is a quadruple: [group_repr] * [group_repr] * [var] * [trm],
   which specifies how to transform the first group into the second
   by instantiating [var] with [trm].
*)

type focus_list = (group_repr * group_repr * (var * trm)) list

(** [extract_group]: Will extract the groups and the basic cell accessed for future processing  *)
let rec extract_group (formula : trm) : (range list * trm) option =
  match formula_group_inv formula with
  | Some (index, range, body) -> (
    match extract_group body with
    | Some (range_list, t) -> Some ([ (index, range) ] @ range_list, t)
    | _ -> None
  )
  | _ -> (
    match formula_cell_inv formula with Some t -> Some ([], t) | _ -> None
  )

(** [to_group_repr]: Build group_repr representation of the groups / and indices
Used to compute list of focus in the build_focus function *)

(* We are trying to find for each indice, a group that can be binded to it.
For every indice : find the sets of used variables, try to find the intersection of used vars and groups that
If the set S of intersection is > 1 then we abort
If thereis exaclty one match then the group is binded to this index (Star)
If there is no match, then we procuce an Index  *)
let to_group_repr (groups : (var * trm) list) (indices : trms) (t_base : trm) : group_repr option =
  let rec aux current_groups star_index_list = function
    | [] ->
      let t_base =
        List.fold_left (fun acc (var, range) -> formula_group var range acc) t_base current_groups
      in
      Some (List.rev star_index_list, t_base)
    | indice :: rest -> (
      let possible_groups =
        List.filter (fun (var, range) -> Var_set.mem var (trm_used_vars indice)) current_groups
      in
      match possible_groups with
      | [] -> aux current_groups (Index indice :: star_index_list) rest
      | [ (var, range) ] ->
        let current_groups = List.remove (var, range) current_groups in
        aux current_groups (Star ((var, range), indice) :: star_index_list) rest
      | _ -> None
    ) in
  aux groups [] indices

let rec trms_unify l1 l2 evar_ctx validate_inst =
  let open Option.Monad in
  match (l1, l2) with
  | [], [] -> Some evar_ctx
  | t1 :: q1, t2 :: q2 ->
    let* evar_ctx = trm_unify t1 t2 evar_ctx validate_inst in
    trms_unify q1 q2 evar_ctx validate_inst
  | _, _ -> None

(** [ALGO]*)

(* index -> trm_index *)

(** [is_focusable star index] : determines whether a given star can be focused on a specific element
    Criteria for focus: This fonction determines whether
    for i in r -> H(i) can be focused into H'
    The function returns Some(t) if H(t) unifies with H', meaning that the star on i is focused on index t *)
let is_focusable (range, formula) index : (var * trm) option =
  let open Option.Monad in
  let var_star_index, range = range in
  let evar_ctx = Var_map.(empty |> add var_star_index (Unknown ())) in
  (* i is directly a var *)
  let* evar_ctx = trm_unify formula index evar_ctx (fun _ _ ctx -> Some ctx) in
  match Var_map.find var_star_index evar_ctx with
  | Unknown () -> None
  | Resolved t -> Some (var_star_index, t)

let are_same_range range1 range2 : bool = true

let are_same_group_repr ((stars1, t1) : group_repr) ((stars2, t2) : group_repr) : bool =
  are_same_trm t1 t2
  && List.length stars1 = List.length stars2
  && List.for_all2
       (fun s1 s2 ->
         match (s1, s2) with
         | Index i1, Index i2 -> are_same_trm i1 i2
         | Star (range1, i1), Star (range2, i2) ->
           are_same_range range1 range2 && are_same_trm i1 i2
         | _, _ -> false
       )
       stars1 stars2

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

let base_access t =
  let base, access = trm_inv trm_array_access_inv t in
  base

(* To comment  *)

(** [build focus list]: Tries to build a [focus_list], i.e list of pairs of [group repr] tha represents unitary focuses that allows to goes from [from_group] to [to _group] *
Each focus : H_i -> H_i+1)
This function assumes that the from_group and to_group have been normalized already by the caller.
Criteria for focus is described in the [is_focusable] function
If the result is Some [F_1;..;F_N] then F_i represents a ghost operation that consumes H_i and produces H_i+1, therefore the sequence F1;..Fn consumes H_1 = from_group and produces H_n+1 = to_group.

*)
let build_focus_list (from_group : group_repr) (to_group : group_repr) : focus_list option =
  let open Option.Monad in
  let stars_from, t1 = from_group in
  let stars_to, t2 = to_group in
  if not (List.length stars_from == List.length stars_to && are_same_trm t1 t2) then
    raise
      (Invalid_argument
         "Build focus: lists must have the same size and base_trm must be identical in 'from' and \
          'to'"
      )
  else
    let folder
        (acc : (starindex list * focus_list * int) option)
        (si_from : starindex)
        (si_to : starindex) =
      match acc with
      | None -> None
      | Some (current_group, acc_focus, ind) -> (
        match (si_from, si_to) with
        | Index _, Star _ -> None
        | Star (range1, index1), Star (range2, index2) ->
          if are_same_range range1 range2 && are_same_trm index1 index2 then
            Some (current_group, acc_focus, ind + 1)
          else
            None
        | Index i1, Index i2 ->
          if are_same_trm i1 i2 then Some (current_group, acc_focus, ind + 1) else None
        | Star (range, index), Index i2 ->
          let* var, resolved_trm = is_focusable (range, index) i2 in
          let new_group = List.update_nth ind (fun t -> Index i2) current_group in
          Some
            ( new_group,
              acc_focus @ [ ((current_group, t1), (new_group, t2), (var, resolved_trm)) ],
              ind + 1
            )
      ) in
    Option.map
      (fun (a, b, c) -> b)
      (List.fold_left2 folder (Some (stars_from, [], 0)) stars_from stars_to)

(**
  [autofocus_unify] : tries to see if formula and formula_to_unify can be unified if we add some ghosts instructions to satisfy the resource needed
  [formula] : might describe an access to a cell or a group
  [formula_candidate] : must be a group (if it's a Cell it has been unified on first round of [subtract_resource_item]
    formula and formula will be parsed,reorder and renamed in order to search for possible focus *)

let autofocus_unify
    (formula : trm)
    (formula_candidate : trm)
    (evar_ctx : 'a unification_ctx)
    (validate_inst : trm -> 'a -> 'a unification_ctx -> 'a unification_ctx option) :
    'a unification_ctx option =
  let open Option.Monad in
  (* Extract groups from the formulas, returns None if it doesn't fit the autofocus scope *)
  let* groups_candidate, t_candidate = extract_group formula_candidate in
  let* groups, t = extract_group formula in
  (* Inverse the term groups are pointing to, returns None if it's not a Matrix access *)
  let* t_base_candidate, dims_candidate, indices_candidates = Matrix_trm.access_inv t_candidate in
  let* t_base, dims, indices = Matrix_trm.access_inv t in
  let* evar_ctx = trm_unify t_base_candidate t_base evar_ctx validate_inst in
  let* evar_ctx = trms_unify dims_candidate dims evar_ctx validate_inst in
  let* groups = to_group_repr groups indices t_base in
  let* groups_candidate = to_group_repr groups_candidate indices_candidates t_base_candidate in
  let* focus_list = build_focus_list groups groups_candidate in


  Some evar_ctx
