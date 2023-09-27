open Ast
open Trm
open Typ
open Mark
open Resource_formula

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

type contract_clause = contract_clause_type * contract_resource

let resource_set ?(pure = []) ?(linear = []) ?(fun_contracts = Var_map.empty) () =
  { pure; linear; fun_contracts }

let empty_resource_set = resource_set ()

let empty_fun_contract =
  { pre = empty_resource_set; post = empty_resource_set }

let empty_loop_contract =
  { loop_ghosts = []; invariant = empty_resource_set; iter_contract = empty_fun_contract }


let rec desugar_formula (formula: formula): formula =
  (* Warning: this function is called on formulas with unresolved variable ids, therefore, we need to invert it by name *)
  Pattern.pattern_match formula [
    Pattern.(trm_apps2 (trm_var (check (fun v -> v.name = "_HasModel"))) !__ (trm_apps (trm_var !__) !__ __)) (fun var f args ->
        if f.name <> sprintf "Matrix%d" (List.length args) then raise Pattern.Next;
        formula_matrix var args
      );
    Pattern.(!__) (fun formula -> trm_map desugar_formula formula)
  ]

let rec encode_formula (formula: formula): formula =
  match formula_matrix_inv formula with
  | Some (m, dims) -> formula_model m (trm_apps (trm_var (name_to_var (sprintf "Matrix%d" (List.length dims)))) dims)
  | None -> trm_map encode_formula formula

let desugar_res ((name, formula): contract_resource): contract_resource =
  (name, desugar_formula formula)

let push_pure_res (res: contract_resource) (res_set: resource_set) =
  { res_set with pure = new_res_item (desugar_res res) :: res_set.pure }

let push_linear_res (res: contract_resource) (res_set: resource_set) =
  { res_set with linear = new_res_item (desugar_res res) :: res_set.linear }

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
  | Reads ->
    let name, formula = res in
    let frac_var, frac_ghost = new_frac () in
    let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
    { contract with loop_ghosts = frac_ghost :: contract.loop_ghosts; iter_contract = push_fun_contract_clause Modifies (name, ro_formula) contract.iter_contract }
  | SequentiallyReads ->
    let name, formula = res in
    let frac_var, frac_ghost = new_frac () in
    let ro_formula = formula_read_only ~frac:(trm_var frac_var) formula in
    { contract with loop_ghosts = frac_ghost :: contract.loop_ghosts; invariant = push_linear_res (name, ro_formula) contract.invariant }
  | SequentiallyModifies ->
    { contract with invariant = push_linear_res res contract.invariant }
  | _ -> { contract with iter_contract = push_fun_contract_clause clause res contract.iter_contract }

let parse_contract_clauses (empty_contract: 'c) (push_contract_clause: contract_clause_type -> contract_resource -> 'c -> 'c) (clauses: (contract_clause_type * string) list) : 'c =
  List.fold_right (fun (clause, desc) contract  ->
      try
        let res_list = Resource_cparser.resource_list (Resource_clexer.lex_resources) (Lexing.from_string desc) in
        List.fold_right (fun res contract -> push_contract_clause clause res contract) res_list contract
      with Resource_cparser.Error ->
        failwith ("Failed to parse resource: " ^ desc)
    ) clauses empty_contract

let parse_fun_contract = parse_contract_clauses empty_fun_contract push_fun_contract_clause
let parse_loop_contract = parse_contract_clauses empty_loop_contract push_loop_contract_clause

let __pure () = Requires, ""
let __requires (r: string) = Requires, r
let __ensures (r: string) = Ensures, r
let __invariant (r: string) = Invariant, r
let __reads (r: string) = Reads, r
let __modifies (r: string) = Modifies, r
let __consumes (r: string) = Consumes, r
let __produces (r: string) = Produces, r
let __sequentially_reads (r: string) = SequentiallyReads, r
let __sequentially_modifies (r: string) = SequentiallyModifies, r

let res_group_range (range: loop_range) (res: resource_set): resource_set =
  { pure = List.map (fun (x, fi) -> (x, formula_group_range range fi)) res.pure;
    linear = List.map (fun (x, fi) -> (x, formula_group_range range fi)) res.linear;
    fun_contracts = res.fun_contracts; }

let res_union (res1: resource_set) (res2: resource_set): resource_set =
  { pure = res1.pure @ res2.pure; linear = res1.linear @ res2.linear;
    fun_contracts = Var_map.union (fun _ _ c -> Some c) res1.fun_contracts res2.fun_contracts }
