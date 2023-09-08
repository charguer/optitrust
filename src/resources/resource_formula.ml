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

(* LATER: Decide if we want to generalize this way of deconstructing patterns *)
module Pattern = struct
  exception PatternFailed

  let __ (v: 'a) (k:'a -> 'b): 'b = k v
  let any (v: 'a) (k: 'b): 'b = k
  let trm_apps (fn: trm -> 'a -> 'b) (args: trm list -> 'b -> 'c) (t: trm) (k: 'a): 'c =
    match trm_apps_inv t with
    | Some (f, a) ->
      let k = fn f k in
      let k = args a k in
      k
    | None -> raise PatternFailed

  let trm_var (var: var -> 'a -> 'b) (t: trm) (k: 'a): 'b =
    match trm_var_inv t with
    | Some v -> var v k
    | None -> raise PatternFailed

  let specific_var (v: var) (pat_var: var) (k: 'a): 'a =
    if var_eq v pat_var then k else raise PatternFailed

  (* Probably useless? *)
  let trm_apps_specific_var (v: var) = trm_apps (trm_var (specific_var v))

  let nil (l: _ list) (k: 'a) : 'a =
    match l with
    | [] -> k
    | _ -> raise PatternFailed

  let (^::) (fh: 'a -> 'b -> 'c) (ft: 'a list -> 'c -> 'd) (l: 'a list) (k: 'b): 'd =
    match l with
    | h :: t ->
      let k = fh h k in
      let k = ft t k in
      k
    | _ -> raise PatternFailed

  let formula_group f_range f_group_body =
    trm_apps_specific_var var_group (f_range ^:: f_group_body ^:: nil)

  let formula_range f_begin f_end f_step =
    trm_apps_specific_var var_range (f_begin ^:: f_end ^:: f_step ^:: nil)

  let trm_int_eq (x: int) (t: trm) (k: 'a): 'a =
    match trm_int_inv t with
    | Some y when x = y -> k
    | _ -> raise PatternFailed

  let trm_fun f_args f_typ f_body f_contract t k =
    match t.desc with
    | Trm_fun (args, typ, body, contract) ->
      let k = f_args args k in
      let k = f_typ typ k in
      let k = f_body body k in
      let k = f_contract contract k in
      k
    | _ -> raise PatternFailed

  let pair f1 f2 (x1, x2) k =
    let k = f1 x1 k in
    let k = f2 x2 k in
    k
end

let trm_apps_specific_var_inv (f: var) (t: trm): trm list option =
  try
    Pattern.(trm_apps_specific_var f __) t (fun args -> Some args)
  with Pattern.PatternFailed -> None

let formula_matrix_inv (f: formula): (trm * trm list) option =
  let open Tools.OptionMonad in
  let rec nested_group_inv (f: formula): (formula * var list * trm list) =
    try
      Pattern.(formula_group (formula_range (trm_int_eq 0) __ (trm_int_eq 1)) (trm_fun (pair __ any ^:: nil) any __ any)) f (fun dim idx inner_formula ->
        let inner_formula, indices, dims = nested_group_inv inner_formula in
        (inner_formula, idx::indices, dim::dims)
      )
    with Pattern.PatternFailed -> (f, [], [])
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
