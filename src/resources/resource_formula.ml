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

let var_has_model = trm_toplevel_free_var "_HasModel"
let var_read_only = trm_toplevel_free_var "_RO"
let var_frac = trm_toplevel_free_var "_Fraction"
let full_frac = trm_int 1

let formula_annot = {trm_annot_default with trm_annot_cstyle = [ResourceFormula]}

let formula_model (x: trm) (model: formula): formula =
  trm_apps ~annot:formula_annot var_has_model [x; model]

let formula_model_inv (t: formula): (trm * formula) option =
  match trm_apps_inv t with
  | Some (fn, [tx; tf]) ->
    begin match trm_var_inv fn with
    | Some fnv when var_has_name fnv "_HasModel" -> Some (tx, tf)
    | _ -> None
    end
  | _ -> None

let formula_read_only ~(frac: formula) (res: formula) =
  trm_apps ~annot:formula_annot var_read_only [frac; res]

let new_frac (): var * resource_item =
  let frac_hyp = new_anon_hyp () in
  (frac_hyp, (frac_hyp, var_frac))

type read_only_formula = { frac: formula; formula: formula }
let formula_read_only_inv (t: formula): read_only_formula option =
  match trm_apps_inv t with
  | Some (fn, [frac; formula]) ->
    begin match trm_var_inv fn with
    | Some fnv when var_has_name fnv "_RO" -> Some { frac ; formula }
    | _ -> None
    end
  | _ -> None

let formula_read_only_map (f_map: formula -> formula) (formula: formula) =
  match formula_read_only_inv formula with
  | Some { frac; formula } ->
    formula_read_only ~frac (f_map formula)
  | None -> f_map formula

let var_cell = toplevel_free_var "Cell"
let trm_cell = trm_var var_cell
let var_group = toplevel_free_var "Group"
let trm_group = trm_var var_group
let var_range = toplevel_free_var "range"
let trm_range = trm_var var_range

let formula_cell (x: var): formula =
  formula_model (trm_var x) trm_cell

let formula_matrix (m: trm) (dims: trm list) : formula =
  let indices = List.mapi (fun i _ -> new_var (sprintf "i%d" (i+1))) dims in
  let mindex_n = trm_var (name_to_var (sprintf "MINDEX%d" (List.length dims))) in
  let inner_trm = formula_model (trm_array_access m (trm_apps mindex_n (dims @ List.map trm_var indices))) trm_cell in
  List.fold_right2 (fun idx dim formula ->
    trm_apps ~annot:formula_annot trm_group [trm_apps trm_range [trm_int 0; dim; trm_int 1]; trm_fun ~annot:formula_annot [idx, typ_int ()] None formula])
    indices dims inner_trm

let formula_group_range ((idx, tfrom, dir, tto, step, _): loop_range) =
  formula_read_only_map (fun fi ->
    if dir <> DirUp then failwith "formula_group_range only supports DirUp";
    let range_var = new_var ~qualifier:idx.qualifier idx.name in
    let fi = trm_subst_var idx (trm_var range_var) fi in
    trm_apps ~annot:formula_annot trm_group [trm_apps trm_range [tfrom; tto; loop_step_to_trm step]; trm_fun ~annot:formula_annot [range_var, typ_int ()] None fi]
  )

module Pattern = struct
  include Pattern

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
  let* mindex_f, mindex_args = trm_apps_inv mindex in
  let* mindex_f = trm_var_inv mindex_f in
  let* () = if mindex_f.name = sprintf "MINDEX%d" ((List.length mindex_args) / 2) then Some () else None in
  let rec check_idx_args args indices =
    match args, indices with
    | arg :: args, idx :: indices ->
      begin match trm_var_inv arg with
      | Some idx_arg when var_eq idx idx_arg -> check_idx_args args indices
      | _ -> None
      end
    | [], [] -> Some (matrix, dims)
    | _ -> None
  in
  let rec check_dim_args (args: trm list) (dims: trm list) =
    match args, dims with
    | arg :: args, dim :: dims when are_same_trm arg dim ->
      check_dim_args args dims
    | _, [] -> check_idx_args args indices
    | _ -> None
  in
  check_dim_args mindex_args dims

let var_fun_type = toplevel_free_var "_Fun"

let formula_fun_type (targ: trm) (tres: trm) =
  trm_apps (trm_var var_fun_type) [targ; tres]
