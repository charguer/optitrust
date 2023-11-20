open Ast
open Trm
open Typ
open Mark

let next_hyp_id = Tools.fresh_generator ()

let new_hyp ?(qualifier = []) (name : string) : hyp =
  let id = next_var_int () in
  { qualifier; name; id }

let new_anon_hyp (): hyp =
  let hid = next_hyp_id () in
  new_hyp (sprintf "#%d" hid)

let new_hyp_like (h: hyp option): hyp =
  match h with
  | Some h -> new_hyp ~qualifier:h.qualifier h.name
  | None -> new_anon_hyp ()

type contract_clause_type =
  | Requires
  | Ensures
  | Invariant
  | Reads
  | Modifies
  | Consumes
  | Produces
  | SequentiallyReads
  | SequentiallyModifies

type contract_resource = var option * formula

let var_has_model = trm_toplevel_free_var "_HasModel"
let var_read_only = trm_toplevel_free_var "_RO"
let var_frac = trm_toplevel_free_var "_Fraction"
let full_frac = trm_int 1

let formula_model (x: trm) (model: formula): formula =
  trm_apps var_has_model [x; model]

let formula_model_inv (t: formula): (trm * formula) option =
  match trm_apps_inv t with
  | Some (fn, [tx; tf]) ->
    begin match trm_var_inv fn with
    | Some fnv when var_has_name fnv "_HasModel" -> Some (tx, tf)
    | _ -> None
    end
  | _ -> None

let formula_read_only ~(frac: formula) (res: formula) =
  trm_apps var_read_only [frac; res]

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

let var_cell = trm_var (toplevel_free_var "Cell")

let formula_cell (x: var): formula =
  formula_model (trm_var x) var_cell

let formula_matrix (x: var) (dims: trms) : formula =
  let matrixN = trm_toplevel_free_var (sprintf "Matrix%d" (List.length dims)) in
  formula_model (trm_var x) (trm_apps matrixN dims)

type contract_clause = contract_clause_type * contract_resource

let resource_set ?(pure = []) ?(linear = []) ?(fun_contracts = Var_map.empty) () =
  { pure; linear; fun_contracts }

let empty_resource_set = resource_set ()

let empty_fun_contract =
  { pre = empty_resource_set; post = empty_resource_set }

let empty_loop_contract =
  { loop_ghosts = []; invariant = empty_resource_set; iter_contract = empty_fun_contract }


let new_res_item ((name, formula): contract_resource): resource_item =
  (new_hyp_like name, formula)

let push_pure_res (res: contract_resource) (res_set: resource_set) =
  { res_set with pure = new_res_item res :: res_set.pure }

let push_linear_res (res: contract_resource) (res_set: resource_set) =
  { res_set with linear = new_res_item res :: res_set.linear }

let push_read_only_fun_contract_res ((name, formula): contract_resource) (contract: fun_contract): fun_contract =
  let frac_var, frac_ghost = new_frac () in
  let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
  let pre = push_linear_res (name, ro_formula) { contract.pre with pure = frac_ghost :: contract.pre.pure } in
  let post = push_linear_res (name, ro_formula) contract.post in
  { pre; post }

(* LATER: Preserve user syntax using annotations *)
let push_fun_contract_clause (clause: contract_clause_type)
    (res: contract_resource) (contract: fun_contract) =
  match clause with
  | Requires -> { contract with pre = push_pure_res res contract.pre }
  | Consumes -> { contract with pre = push_linear_res res contract.pre }
  | Ensures -> { contract with post = push_pure_res res contract.post }
  | Produces -> { contract with post = push_linear_res res contract.post }
  | Reads -> push_read_only_fun_contract_res res contract
  | Modifies -> { pre = push_linear_res res contract.pre ; post = push_linear_res res contract.post }
  | Invariant -> { pre = push_pure_res res contract.pre ; post = push_pure_res res contract.post }
  | SequentiallyReads -> failwith "SequentiallyReads only makes sense for loop contracts"
  | SequentiallyModifies -> failwith "SequentiallyModifies only makes sense for loop contracts"

let push_loop_contract_clause (clause: contract_clause_type)
    (res: contract_resource) (contract: loop_contract) =
  match clause with
  | Invariant -> { contract with invariant = push_pure_res res contract.invariant }
  | SequentiallyReads ->
    let name, formula = res in
    let frac_var, frac_ghost = new_frac () in
    let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
    { contract with loop_ghosts = frac_ghost :: contract.loop_ghosts; invariant = push_linear_res (name, ro_formula) contract.invariant }
  | SequentiallyModifies ->
    { contract with invariant = push_linear_res res contract.invariant }
  | _ -> { contract with iter_contract = push_fun_contract_clause clause res contract.iter_contract }

(* CHECK: #var-id *)
let var_group = trm_toplevel_free_var "Group"
let var_range = trm_toplevel_free_var "range"

let formula_group_range ((idx, tfrom, dir, tto, step, _): loop_range) (fi: formula) =
  if dir <> DirUp then failwith "formula_group_range only supports DirUp";
  let range_var = new_var ~qualifier:idx.qualifier idx.name in
  let fi = trm_subst_var idx (trm_var range_var) fi in
  trm_apps var_group [trm_apps var_range [tfrom; tto; loop_step_to_trm step]; trm_fun [range_var, typ_int ()] None fi]

let res_group_range (range: loop_range) (res: resource_set): resource_set =
  { pure = List.map (fun (x, fi) -> (x, formula_group_range range fi)) res.pure;
    linear = List.map (fun (x, fi) -> (x, formula_group_range range fi)) res.linear;
    fun_contracts = res.fun_contracts; }

let res_union (res1: resource_set) (res2: resource_set): resource_set =
  { pure = res1.pure @ res2.pure; linear = res1.linear @ res2.linear;
    fun_contracts = Var_map.union (fun _ _ c -> Some c) res1.fun_contracts res2.fun_contracts }
