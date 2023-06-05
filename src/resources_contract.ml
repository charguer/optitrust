open Ast

type contract_clause_type =
  | Requires
  | Ensures
  | Invariant
  | Reads
  | Modifies
  | Consumes
  | Produces
  | IndependantlyModifies

type contract_resource = hyp option * formula

let var_has_model = trm_var "_HasModel"
let var_read_only = trm_var "_RO"

let trm_var_model (x: var) (model: formula): formula =
  trm_apps var_has_model [trm_var x; model]

let trm_var_model_inv (t: formula): (var * formula) option =
  match trm_apps_inv t with
  | Some (fn, [tx; tf]) ->
    begin match trm_var_inv fn, trm_var_inv tx with
    | Some "_HasModel", Some x -> Some (x, tf)
    | _ -> None
    end
  | _ -> None

let trm_read_only (t: formula) =
  trm_apps var_read_only [t]

let trm_read_only_inv (t: formula): formula option =
  match trm_apps_inv t with
  | Some (fn, [t]) ->
    begin match trm_var_inv fn with
    | Some "_RO" -> Some t
    | _ -> None
    end
  | _ -> None

let formula_cell (x: var): formula =
   trm_var_model x (trm_var "Cell")

type contract_clause = contract_clause_type * contract_resource

let resource_set ?(pure = []) ?(linear = []) ?(fun_contracts = Var_map.empty) () =
  { pure; linear; fun_contracts }

let empty_resource_set = resource_set ()

let empty_fun_contract =
  { pre = empty_resource_set; post = empty_resource_set }

let empty_loop_contract =
  { invariant = empty_resource_set; iter_contract = empty_fun_contract }




let push_pure_res (res: contract_resource) (res_set: resource_set) =
  { res_set with pure = res :: res_set.pure }

let push_linear_res (res: contract_resource) (res_set: resource_set) =
  (* LATER: add existential qualifier for the models *)
  { res_set with linear = res :: res_set.linear }

let push_read_only_res ((name, formula): contract_resource) (res_set: resource_set) =
  push_pure_res (name, trm_read_only formula) res_set

(* Preserving user syntax: two possibilities
   - Use another structured type for storage in AST
   - Use annotations *)

let push_fun_contract_clause (clause: contract_clause_type)
    (res: contract_resource) (contract: fun_contract) =
  match clause with
  | Requires -> { contract with pre = push_pure_res res contract.pre }
  | Consumes -> { contract with pre = push_linear_res res contract.pre }
  | Ensures -> { contract with post = push_pure_res res contract.post }
  | Produces -> { contract with post = push_linear_res res contract.post }
  | Reads -> { contract with pre = push_read_only_res res contract.pre }
  | Modifies -> { pre = push_linear_res res contract.pre ; post = push_linear_res res contract.post }
  | Invariant -> { pre = push_pure_res res contract.pre ; post = push_pure_res res contract.post }
  | IndependantlyModifies -> failwith "IndependantlyModifies only makes sense for loop contracts"

let push_loop_contract_clause (clause: contract_clause_type)
    (res: contract_resource) (contract: loop_contract) =
  match clause with
  | Invariant -> { contract with invariant = push_pure_res res contract.invariant }
  | IndependantlyModifies ->
    { contract with invariant = push_linear_res res contract.invariant }
  | _ -> { contract with iter_contract = push_fun_contract_clause clause res contract.iter_contract }
