open Ast
open Trm
open Typ
open Mark

(**
  A [formula] is a [trm] that corresponds to a logical formula. Evaluating inside a formula cannot have side effects and always terminates.
  Formulas may refer to pure variables, bound by a [resource_set] or by the program (program variables are constant due to our AST encoding).
*)

(** An optionally named resource item produced by parser.
    TODO: remove. *)
type contract_resource_item = var option * formula

let new_hyp = new_var

(** Number used to generate variable names for resources. *)
let next_hyp_id = Tools.fresh_generator ()

(** Returns a variable with a generated name.

  TODO: separate pure ($) and linear (#). *)
let new_anon_hyp (): hyp =
  let hid = next_hyp_id () in
  new_hyp (sprintf "#%d" hid)

(* TODO: should be new_var_like and maybe useful elsewhere? *)
let new_hyp_like (h: hyp): hyp =
  new_hyp ~qualifier:h.qualifier h.name

(** _HasModel(p, Cell) <=> p ~> Cell *)
let var_has_model = toplevel_var "_HasModel"
let trm_has_model = trm_var var_has_model

(** Primitive function that constructs a read only resource. *)
let var_read_only = toplevel_var "_RO"
let trm_read_only = trm_var var_read_only

(** Primitive function that constructs an uninit resource. *)
let var_uninit = toplevel_var "_Uninit"
let trm_uninit = trm_var var_uninit

(** Primitive type of fractions. *)
let var_frac = toplevel_var "_Fraction"
let trm_frac = trm_var var_frac

(** Creates a new fraction variable.
    It ranges on fraction values from \]0; 1\]. *)
let new_frac (): var * resource_item =
  let frac_hyp = new_anon_hyp () in
  (frac_hyp, (frac_hyp, trm_frac))

(** The fraction representing having it all. *)
let full_frac = trm_int 1

(** All formulas should have this annotation. *)
let formula_annot = {trm_annot_default with trm_annot_cstyle = [ResourceFormula]}

(** Tries to embed a program term within formulas.
    Pure and total terms can be successfully embedded, according to built-in whitelist. *)
let rec formula_of_trm (t: trm): formula option =
  (* LATER: Extensible list of applications that can be translated into formula.
     OR A term can be embedded in a formula if it is pure (no linear resources).
     Does [t] change or is this just checking whether trm_is_formula?
     I.e. are the trm and formula languages intersecting or separate?
     *)
  let open Xoption.OptionMonad in
  match t.desc with
  | Trm_val _ | Trm_var _ -> Some t
  | Trm_apps (fn, args, _) ->
    let* f_args = try Some (List.map (fun arg -> Option.get (formula_of_trm arg)) args) with Invalid_argument _ -> None in
    begin match trm_prim_inv fn with
      | Some Prim_binop Binop_add
      | Some Prim_binop Binop_sub
      | Some Prim_binop Binop_mul
      | Some Prim_binop Binop_div (* TODO: think hard about 'div' totality *)
      | Some Prim_binop Binop_mod
      | Some Prim_binop Binop_array_access
          -> Some (trm_apps fn f_args)
      | Some _ -> None
      | None ->
        begin match trm_var_inv fn with
          | Some fv when Matrix_trm.mindex_var_inv fv <> None
              -> Some (trm_apps fn f_args)
          | _ -> None
        end
    end
  | _ -> None

(* -------- SMART FORMULA CONSTRUCTORS, INVERTERS and COMBINATORS -------- *)

let formula_fun =
  trm_fun ~annot:formula_annot

let formula_model (x: trm) (model: formula): formula =
  trm_apps ~annot:formula_annot trm_has_model [x; model]

let formula_model_inv (t: formula): (trm * formula) option =
  match trm_apps_inv t with
  | Some (fn, [tx; tf]) ->
    begin match trm_var_inv fn with
    | Some fnv when var_eq var_has_model fnv -> Some (tx, tf)
    | _ -> None
    end
  | _ -> None

let formula_read_only ~(frac: formula) (res: formula) =
  trm_apps ~annot:formula_annot trm_read_only [frac; res]

let formula_uninit (inner_formula: formula): formula =
  trm_apps ~annot:formula_annot trm_uninit [inner_formula]

let var_cell = toplevel_var "Cell"
let trm_cell = trm_var var_cell

let formula_cell (x: var): formula =
  formula_model (trm_var x) trm_cell

let var_range = toplevel_var "range"
let trm_range = trm_var var_range
let formula_range (start: trm) (stop: trm) (step: trm) =
  trm_apps ~annot:formula_annot trm_range [start; stop; step]

let var_group = toplevel_var "Group"
let trm_group = trm_var var_group

let formula_matrix (m: trm) (dims: trm list) : formula =
  let indices = List.mapi (fun i _ -> new_var (sprintf "i%d" (i+1))) dims in
  let inner_trm = formula_model (Matrix_trm.access m dims (List.map trm_var indices)) trm_cell in
  List.fold_right2 (fun idx dim formula ->
    trm_apps ~annot:formula_annot trm_group [formula_range (trm_int 0) dim (trm_int 1); formula_fun [idx, typ_int ()] None formula])
    indices dims inner_trm

let var_in_range = toplevel_var "in_range"
let trm_in_range = trm_var var_in_range
let formula_in_range (i: trm) (range: trm) =
  trm_apps trm_in_range [i; range]

let var_is_subrange = toplevel_var "is_subrange"
let trm_is_subrange = trm_var var_is_subrange
let formula_is_subrange (range1: trm) (range2: trm) =
  trm_apps trm_is_subrange [range1; range2]

module Pattern = struct
  include Pattern

  let formula_model f_var f_model =
    trm_apps_specific_var var_has_model (f_var ^:: f_model ^:: nil)

  let formula_read_only f_frac f_formula =
    trm_apps2 (trm_var (var_eq var_read_only)) f_frac f_formula

  let formula_uninit f_formula =
    trm_apps1 (trm_var (var_eq var_uninit)) f_formula

  let formula_group f_range f_group_body =
    trm_apps_specific_var var_group (f_range ^:: f_group_body ^:: nil)

  let formula_range (f_begin: 'a -> trm -> 'b) (f_end: 'b -> trm -> 'c) (f_step: 'c -> trm -> 'd) =
    trm_apps_specific_var var_range (f_begin ^:: f_end ^:: f_step ^:: nil)
end

type read_only_formula = { frac: formula; formula: formula }
let formula_read_only_inv (formula : formula): read_only_formula option =
  Pattern.pattern_match_opt formula [
    Pattern.(formula_read_only !__ !__) (fun frac formula -> { frac; formula })
  ]

(** Applies a function below a read only wrapper if there is one,
    otherwise simply applies the function. *)
let formula_map_under_read_only (f_map: formula -> formula) (formula: formula) =
  match formula_read_only_inv formula with
  | Some { frac; formula } ->
    formula_read_only ~frac (f_map formula)
  | None -> f_map formula

let formula_uninit_inv (formula: formula): formula option =
  Pattern.pattern_match_opt formula [
    Pattern.(formula_uninit !__) (fun f -> f);
  ]


(** Applies a function below an uninit wrapper if there is one,
    otherwise simply applies the function. *)
let formula_map_under_uninit (f_map: formula -> formula) (formula: formula) =
  match formula_uninit_inv formula with
  | Some formula -> formula_uninit (f_map formula)
  | None -> f_map formula

(** Applies a function below a resource mode wrapper if there is one,
    otherwise simply applies the function.

  Current resource mode wrappers include read only and uninit, they float to the top of formulas.
 *)
let formula_map_under_mode (f_map: formula -> formula): formula -> formula =
  formula_map_under_read_only (formula_map_under_uninit f_map)

let formula_loop_range (range: loop_range): formula =
  if range.direction <> DirUp then failwith "formula_loop_range only supports DirUp";
  formula_range range.start range.stop (loop_step_to_trm range.step)

let formula_group_range (range: loop_range) =
  formula_map_under_mode (fun fi ->
    let range_var = new_var ~qualifier:range.index.qualifier range.index.name in
    let fi = trm_subst_var range.index (trm_var range_var) fi in
    trm_apps ~annot:formula_annot trm_group [formula_loop_range range; formula_fun [range_var, typ_int ()] None fi]
  )

let formula_matrix_inv (f: formula): (trm * trm list) option =
  let open Xoption.OptionMonad in
  let rec nested_group_inv (f: formula): (formula * var list * trm list) =
    Pattern.pattern_match f [
      Pattern.(formula_group (formula_range (trm_int (eq 0)) !__ (trm_int (eq 1))) (trm_fun (pair !__ __ ^:: nil) !__))
        (fun dim idx inner_formula ->
          let inner_formula, indices, dims = nested_group_inv inner_formula in
          (inner_formula, idx::indices, dim::dims)
        );
      Pattern.__ (f, [], [])
    ]
  in
  let inner_formula, indices, dims = nested_group_inv f in

  let* location, cell = formula_model_inv inner_formula in
  let* cell_candidate = trm_var_inv cell in
  let* () = if var_eq cell_candidate var_cell then Some () else None in
  let* matrix, mindex_dims, mindex_indices = Matrix_trm.access_inv location in
  let* () = if List.length mindex_dims = List.length dims then Some () else None in
  let* () = if List.for_all2 are_same_trm mindex_dims dims then Some () else None in
  if List.for_all2 (fun mindex_idx idx ->
      match trm_var_inv mindex_idx with
      | Some idx_arg when var_eq idx idx_arg -> true
      | _ -> false
    ) mindex_indices indices
  then Some (matrix, dims)
  else None

let var_fun_type = toplevel_var "_Fun"

let formula_fun_type (targ: trm) (tres: trm) =
  trm_apps (trm_var var_fun_type) [targ; tres]

let formula_assert_eq = toplevel_var "__assert_eq"
let formula_assert_neq = toplevel_var "__assert_neq"
let formula_assert_lt = toplevel_var "__assert_lt"
let formula_assert_gt = toplevel_var "__assert_gt"
let formula_assert_leq = toplevel_var "__assert_leq"
let formula_assert_geq = toplevel_var "__assert_geq"

let formula_cmp (cmp: var) (a: formula) (b: formula): formula =
  trm_apps ~annot:formula_annot (trm_var cmp) [a; b]

let formula_eq = formula_cmp formula_assert_eq
let formula_neq = formula_cmp formula_assert_neq
let formula_lt = formula_cmp formula_assert_lt
let formula_gt = formula_cmp formula_assert_gt
let formula_leq = formula_cmp formula_assert_leq
let formula_geq = formula_cmp formula_assert_geq

let var_arith_checked = toplevel_var "arith_checked"
let formula_arith_checked = trm_var var_arith_checked

(** [filter_common_resources res1 res2] finds all the resources that are common in [res1] and [res2], and returns [common, res1', res2'] where:
  - [comm] are the common resources,
  - [res1'] are the resources from [res1] that are not in [res2],
  - [res2'] are the resources from [res2] that are not in [res1].

  Resources are matched if they have the same formula, the resource names in [comm] are the names from [res1].

  If [~filter] is provided, keep separated resources for which [filter] returns false.
*)
let filter_common_resources ?(filter = fun _ -> true) (res1: resource_item list) (res2: resource_item list): resource_item list * resource_item list * resource_item list =
  let res2 = ref res2 in
  let rec try_remove_same_formula formula l =
    match l with
    | [] -> None
    | (_,f)::l when are_same_trm f formula -> Some l
    | res::l -> Option.map (fun l -> res::l) (try_remove_same_formula formula l)
  in
  let common, res1 = List.partition
    (fun (_, formula) ->
      if filter formula then
        match try_remove_same_formula formula !res2 with
        | None -> false
        | Some new_res2 -> res2 := new_res2; true
      else false)
    res1
  in
  common, res1, !res2
