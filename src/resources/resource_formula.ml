open Ast
open Trm
open Typ
open Mark

type contract_resource = var option * formula

let new_hyp = new_var

let next_hyp_id = Tools.fresh_generator ()

let new_anon_hyp (): hyp =
  let hid = next_hyp_id () in
  new_hyp (sprintf "#%d" hid)

let new_hyp_like (h: hyp option): hyp =
  match h with
  | Some h -> new_hyp ~qualifier:h.qualifier h.name
  | None -> new_anon_hyp ()

let new_res_item ((name, formula): contract_resource): resource_item =
  (new_hyp_like name, formula)

let var_has_model = toplevel_var "_HasModel"
let trm_has_model = trm_var var_has_model
let var_read_only = toplevel_var "_RO"
let trm_read_only = trm_var var_read_only
let var_frac = toplevel_var "_Fraction"
let trm_frac = trm_var var_frac
let full_frac = trm_int 1

let formula_annot = {trm_annot_default with trm_annot_cstyle = [ResourceFormula]}

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

let new_frac (): var * resource_item =
  let frac_hyp = new_anon_hyp () in
  (frac_hyp, (frac_hyp, trm_frac))

type read_only_formula = { frac: formula; formula: formula }
let formula_read_only_inv (t: formula): read_only_formula option =
  match trm_apps_inv t with
  | Some (fn, [frac; formula]) ->
    begin match trm_var_inv fn with
    | Some fnv when var_eq var_read_only fnv -> Some { frac ; formula }
    | _ -> None
    end
  | _ -> None

let formula_read_only_map (f_map: formula -> formula) (formula: formula) =
  match formula_read_only_inv formula with
  | Some { frac; formula } ->
    formula_read_only ~frac (f_map formula)
  | None -> f_map formula

let var_cell = toplevel_var "Cell"
let trm_cell = trm_var var_cell
let var_group = toplevel_var "Group"
let trm_group = trm_var var_group
let var_range = toplevel_var "range"
let trm_range = trm_var var_range

let formula_cell (x: var): formula =
  formula_model (trm_var x) trm_cell

let formula_matrix (m: trm) (dims: trm list) : formula =
  let indices = List.mapi (fun i _ -> new_var (sprintf "i%d" (i+1))) dims in
  let mindex_n = trm_var (mindex_var (List.length dims)) in
  let inner_trm = formula_model (trm_array_access m (trm_apps mindex_n (dims @ List.map trm_var indices))) trm_cell in
  List.fold_right2 (fun idx dim formula ->
    trm_apps ~annot:formula_annot trm_group [trm_apps trm_range [trm_int 0; dim; trm_int 1]; formula_fun [idx, typ_int ()] None formula])
    indices dims inner_trm

let formula_group_range ((idx, tfrom, dir, tto, step, _): loop_range) =
  formula_read_only_map (fun fi ->
    if dir <> DirUp then failwith "formula_group_range only supports DirUp";
    let range_var = new_var ~qualifier:idx.qualifier idx.name in
    let fi = trm_subst_var idx (trm_var range_var) fi in
    trm_apps ~annot:formula_annot trm_group [trm_apps trm_range [tfrom; tto; loop_step_to_trm step]; formula_fun [range_var, typ_int ()] None fi]
  )

module Pattern = struct
  include Pattern

  let formula_model f_var f_model =
    trm_apps_specific_var var_has_model (f_var ^:: f_model ^:: nil)

  let formula_group f_range f_group_body =
    trm_apps_specific_var var_group (f_range ^:: f_group_body ^:: nil)

  let formula_range (f_begin: 'a -> trm -> 'b) (f_end: 'b -> trm -> 'c) (f_step: 'c -> trm -> 'd) =
    trm_apps_specific_var var_range (f_begin ^:: f_end ^:: f_step ^:: nil)
end

let formula_matrix_inv (f: formula): (trm * trm list) option =
  let open Tools.OptionMonad in
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
  let* matrix, mindex = array_access_inv location in
  let* mindex_dims, mindex_indices = mindex_inv mindex in
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

let var_checked = toplevel_var "checked"
let formula_checked = trm_var var_checked
