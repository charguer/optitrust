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

type focus_list = (group_repr * group_repr * (var * trm)) list
(**
   [focus_list] is the internal representation of the transformation
   that allows us to go from the required resources to the resources we have.

   Each entry is a quadruple: [group_repr] * [group_repr] * [var] * [trm],
   which specifies how to transform the first group into the second
   by instantiating [var] with [trm].
*)

type ghost_list = (formula * formula * (var * trm)) list
(**
   [group_list] is the representation of the transformation
   that allows us to go from the required resources to the resources we have using ghosts.

   Each entry is a quadruple: [formula] * [formula] * [var] * [trm],
   which specifies how to transform the first resources into the second one, instantiating var with trm.
   Theses quadruple will be need to build the ghosts around the instructions to focus the resources.
*)

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

let print_trm_string (t : trm) : string =
  let doc = Ast_to_text.print_trm Ast_to_text.default_style t in
  Tools.document_to_string doc

let print_range ((a, b) : range) : string = Printf.sprintf "(%s, %s)" a.name (print_trm_string b)

let print_starindex (s : starindex) : string =
  match s with
  | Index i -> Printf.sprintf "Index(%s)" (print_trm_string i)
  | Star (r, i) -> Printf.sprintf "Star(%s, %s)" (print_range r) (print_trm_string i)

let print_group_repr ((stars, t) : group_repr) : string =
  let stars_str = stars |> List.map print_starindex |> String.concat "; " in
  Printf.sprintf "([%s], %s)" stars_str (print_trm_string t)

(** [to_group_repr]: Build group_repr representation of the groups / and indices
Used to compute list of focus in the build_focus function *)

(* We are trying to find for each indice, a group that can be binded to it.
For every indice : find the sets of used variables, try to find the intersection of used vars and groups that
If the set S of intersection is > 1 then we abort
If thereis exaclty one match then the group is binded to this index (Star)
If there is no match, then we procuce an Index  *)
let to_group_repr (group : (var * trm) list) (indices : trms) (t_base : trm) : group_repr option =
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

let group_repr_inv (group : group_repr) : formula =
  let star_index_list, t = group in
  let cell = formula_cell t in
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

let rec trms_unify l1 l2 evar_ctx validate_inst =
  let open Option.Monad in
  match (l1, l2) with
  | [], [] -> Some evar_ctx
  | t1 :: q1, t2 :: q2 ->
    let* evar_ctx = trm_unify t1 t2 evar_ctx validate_inst in
    trms_unify q1 q2 evar_ctx validate_inst
  | _, _ -> None

let rec unfold_list_if_resolved_evar (trms : trm list) evar_ctx =
  match trms with
  | [] -> ([], evar_ctx)
  | t :: l ->
    let t, evar_ctx = unfold_if_resolved_evar t evar_ctx in
    let trms, evar_ctx = unfold_list_if_resolved_evar l evar_ctx in
    (t :: trms, evar_ctx)

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

(** [build focus list]: Tries to build a [focus_list], i.e list of pairs of [group repr] tha represents unitary focuses that allows to goes from [from_group] to [to _group] *
Each focus : H_i -> H_i+1)
This function assumes that the from_group and to_group have been normalized already by the caller.
Criteria for focus is described in the [is_focusable] function
If the result is Some [F_1;..;F_N] then F_i represents a ghost operation that consumes H_i and produces H_i+1, therefore the sequence F1;..Fn consumes H_1 = from_group and produces H_n+1 = to_group.

*)
let build_focus_list (from_group : group_repr) (to_group : group_repr) : focus_list option =
  let open Option.Monad in
  Printf.printf "Entered build_focus \n";
  let stars_from, t1 = from_group in
  let stars_to, t2 = to_group in
  if not (List.length stars_from == List.length stars_to) then begin
    Printf.printf "Hre? \n";
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
              Printf.printf " index star \n";
              None
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
        Printf.printf "we managed to get there\n";

        Option.map
          (fun (a, b, c) -> b)
          (List.fold_left2 folder (Some (stars_from, [], 0)) stars_from stars_to)

let ghosts (gl : ghost_list) =
  List.map
    (fun (formula_from, formula_to, (_, _)) ->
      Resource_trm.ghost_admitted_rewrite formula_from formula_to (trm_var (toplevel_var "focus"))
    )
    gl

let ghosts_inv (gl : ghost_list) =
  List.map
    (fun (formula_from, formula_to, (_, _)) ->
      Resource_trm.ghost_admitted_rewrite formula_to formula_from (trm_var (toplevel_var "focus"))
    )
    gl

let seq_from_ghost_list (t : trm) (gl : ghost_list) =
  trm_seq (Mlist.of_list (ghosts gl @ [ t ] @ ghosts_inv gl))

(**
  [autofocus_unify] : tries to see if formula and formula_to_unify can be unified if we add some ghosts instructions to satisfy the resource needed
  [formula] : might describe an access to a cell or a group
  [formula_candidate] : must be a group (if it's a Cell it has been unified on first round of [subtract_resource_item]
    formula and formula will be parsed,reorder and renamed in order to search for possible focus
  This function first checks that the resources are on an array and on the same array.
  Then it unifies the dimensions
  Then it tries to see if a list of ghost_pair can be established around the instruction, that allows to focus the [formula_candidate] into the [formula].
  Returns the updated evar_ctx and the ghost_list if the operation succeeded *)

let autofocus_unify
    (formula : trm)
    (formula_candidate : trm)
    (evar_ctx : 'a unification_ctx)
    (validate_inst : trm -> 'a -> 'a unification_ctx -> 'a unification_ctx option) :
    (ghost_list * 'a unification_ctx) option =
  let open Option.Monad in
  Printf.printf "entered autofocus_unify \n";
  (* Extract groups from the formulas, returns None if it doesn't fit the autofocus scope *)
  let* group_candidate, t_candidate = extract_group formula_candidate in
  let* group, t = extract_group formula in

  (* Inverse the term group are pointing to, returns None if it's not a Matrix access *)
  let* t_base_candidate, dims_candidate, indices_candidates = Matrix_trm.access_inv t_candidate in
  let* t_base, dims, indices = Matrix_trm.access_inv t in
  (* let* evar_ctx = trm_unify t t_candidate evar_ctx validate_inst in *)
  let* evar_ctx = trm_unify t_base_candidate t_base evar_ctx validate_inst in
  let* evar_ctx = trms_unify dims_candidate dims evar_ctx validate_inst in
  let t_base, evar_ctx = unfold_if_resolved_evar t_base evar_ctx in
  let dims, evar_ctx = unfold_list_if_resolved_evar dims evar_ctx in
  let t = Matrix_trm.access t_base dims indices in
  Printf.printf "%s \n" (print_trm_string t);
  Printf.printf "candidat: %s \n" (print_trm_string t_candidate);
  let* group = to_group_repr group indices t in
  let* group_candidate = to_group_repr group_candidate indices_candidates t_candidate in
  let* focus_list = build_focus_list group_candidate group in
  let ghost_list =
    List.map
      (fun (from_group, to_group, (var, index)) ->
        (group_repr_inv from_group, group_repr_inv to_group, (var, index))
      )
      focus_list in
  Some (ghost_list, evar_ctx)
