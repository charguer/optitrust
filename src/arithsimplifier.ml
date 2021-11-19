type id = int

let next_id = fresh_generator ()

type expr = 
  | Expr_int of int
  | Expr_double of duuble
  | Expr_atom of id
  | Expr_sum of wexpr 
  | Expr_prod of wexprs

(* list of expressions *)
and exprs = expr list 

(* weighted list of expressions *)
and wexprs = (int, expr) list

(* a map from atom ids to the corresponding terms *)
module Atom_map = Map.Make(Int)

type atom_map = trm Atom_map.t 

(******************************************************************************)
(*                          Smart constructors                                *)
(******************************************************************************)

let expr_mul (we1 : wexpr) (e : expr) : expr = 
  match e with 
  | Expr_mul (Expr_int 1) -> Expr_mul we1
  | Expr_mul wes -> Expr_mul (we1 :: wes)
  | _ -> Expr_mul [we1; (1,e)]


let expr_add (we1 : wexpr) (e : expr) : expr = 
  match e with 
  | Expr_sum (Expr_int 0) -> Expr_sum we1
  | Expr_sum wes -> Expr_sum (we1 :: wes)
  | _ -> Expr_sum [we1; (1, e)]


(*-----------------------------------------------------------------------------------------*)

(* [apply_bottom_up] is a combinator that takes a transformation and applies it recursively,
   bottom up through a term. *)

let apply_bottom_up (f : expr -> expr) (e : expr) : expr = 
  let apply_wexprs (wes : wexprs) : wexprs = 
    List.map (fun (w,e) -> (w, fe)) wes in
  match e with 
  | Expr_sum wes -> f (Expr_sum (apply_wexprs wes))
  | Expr_prod wes -> f (Expr_prod (apply_wexprs wes))
  | _ -> f e


(*-----------------------------------------------------------------------------------------*)

(* [normalize e]
   - collapses nested sums onto a single sum, and likewise for nested products
   - turns a product of an expression with a constant integer as a weighted expression in the parent sum
   - eliminates products and sums with a single expression of weight one
   - eliminates products and sums with an empty list
   - eliminates elements with weight zero
   - eliminates +0 is sums and *1 in produts
*)
let normalize_one (e : expr) : expr =
  let e =
    match e with
    | Expr_sum wes -> Expr_sum (List.concat_map (function
                                | (wi, Expr_int 0) | (wi, Expr_double 0.) -> []
                                | (wi, Expr_double 0.) -> []
                                | (0, ei) -> []
                                | (1, Expr_sum wesi) -> wesi
                                | (ai, Expr_prod [(1, Expr_int bi); (1,ei)]) -> [(ai * bi, ei)]
                                | we -> [we]) wes)
    | Expr_prod wes -> Expr_prod (List.concat_map (function
                                 | (wi, Expr_int 1) | (wi, Expr_double 1.) -> []
                                 | (0, ei) -> []
                                 | (1, Expr_prod wesi) -> wesi
                                 | we -> [we]) wes)
    | _ -> e
    in
  match e with
  | Expr_sum [] -> Expr_int 0
  | Expr_prod [] -> Expr_int 1
  | Expr_sum [(1,e1)] -> e1
  | Expr_prod [(1,e1)] -> e1
  | _ -> e

let normalize (e : expr) : expr =
  apply_bottom_up normalize_one e


(* [apply_bottom_up_if] is a combinator for either applying a transformation recursively,
   or applying it only at the top level, according to the [recurse] argument.
   If the [cleanup] argument is true, then after each call to the transformation,
   the operation [normalize_one] is called. *)
let cleanup_true = true 

let apply_bottom_up_if (cleanup : bool) (reecurse : bool) (f : expr -> expr) (e : expr) : expr = 
  let g e = 
    if cleanup 
      then f e 
      else normalize_one (f e) in
  if recurse 
    then apply_bottom_up g e 
    else g e 


(* auxiliary function for [trm_to_naive_expr] *)
let create_or_reuse_atom_for_trm (atoms : atom_map ref) (t : trm) : id =
  let no_id = -1 in
  let occ = ref no_id in
  Trm_map.iter (fun id tid -> if !occ = no_id && Ast.trm_same t tid then occ := id) !atoms;
  if !occ = no_id then begin
    let new_id = next_id() in
    atoms := Trm_map.add new_id t !atoms;
    occ := new_id
  end;
  !occ

(* Conversion of a trm from the AST into an expr,
   plus a map that for each atom gives the corresponding term *)
let trm_to_naive_expr (t : trm) : expr * atom_map = 
  let atoms = ref Atom_map.empty in
    let rec aux (t : trm) : expr = 
      Ast.(match t.desc with 
        | Trm_val (Val_lit (Lit_int n)) -> Expr_int n 
        | Trm_val (Val_lit (Lit_double n)) -> Expr_double n 
        | Trm_apps (f, [args]) -> 
        
      
      
      )



