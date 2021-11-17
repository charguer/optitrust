
(*--------contents below in module ArithSimplifier -----------*)

(* A grammar for arithmetic expressions *)

type id = int

let next_id = fresh_generator ()

type expr =
  | Expr_int of int
  | Expr_double of float
  | Expr_atom of id
  | Expr_sum of wexprs
  | Expr_prod of wexprs

(* list of expressions *)
and exprs = expr list

(* weighted list of expressions *)
and wexprs = (int, expr) list

(* [atom_map] is a map from atom ids to correspond terms *)
module Atom_map = Map.Make(Int)
type atom_map = trm Atom_map.t


(*-----------------------------------------------------------------------------------------*)
(* smart constructors *)

let expr_mul (we1 : wexpr) (e : expr) : expr =
  match e with
  | Expr_mul (Expr_int 1) -> Expr_mul we1
  | Expr_mul wes -> Expr_mul (we1::wes)
  | _ -> Expr_mul [we1; (1,e)]


let expr_add (we1 : wexpr) (e : expr) : expr =
  match e with
  | Expr_sum (Expr_int 0) -> Expr_sum we1
  | Expr_sum wes -> Expr_sum (we1::wes)
  | _ -> Expr_sum [we1; (1,e)]


(*-----------------------------------------------------------------------------------------*)

(* [apply_bottom_up] is a combinator that takes a transformation and applies it recursively,
   bottom up through a term. *)

let apply_bottom_up (f : expr -> expr) (e : expr) : expr =
  let apply_wexprs (wes : wexprs) : wexprs =
    List.map (fun (w,e) -> (w, f e)) wes in
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

let apply_bottom_up_if (cleanup : bool) (recurse : bool) (f : expr -> expr) (e : expr) : expr =
  let g e =
    if cleanup
      then f e
      else normalize_one (f e) in
  if recurse
    then apply_bottom_up g e
    else g e

(*-----------------------------------------------------------------------------------------*)

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

let trm_to_naive_expr (t:trm) : expr * atom_map =
  let atoms = ref Atom_map.empty in
  let rec aux (t:trm) : expr =
  (* TODO:
    Ast.(match t with
    | .. Literal_int n -> Expr_int n
    | .. Literal_double d -> Expr_double d
    | .. Trm_app (Prim_neg [t1] ->
        Expr_sum [(-1, aux t1)]
    | .. Trm_app ((Prim_add | Prim_sum) as op) [t1; t2] ->
        let w = match op with Prim_add -> 1 | Prim_sum -> -1 | _ -> assert false in
        Expr_sum [(1, aux t1); (w, aux t2)]
    | .. Trm_app ((Prim_mul | Prim_div) as op) [t1; t2] ->
        let w = match op with Prim_mul -> 1 | Prim_div -> -1 | _ -> assert false in
        Expr_prod [(1, aux t1); (w, aux t2)]
    | _ ->
        Expr_atom (create_or_reuse_atom_for_trm atoms t)
    ) in *) (* dummy implementation: *) Expr_int 0
    in
  aux t, !atoms

let trm_to_expr (t : trm) : expr =
  let expr, atoms = trm_to_naive_expr t in
  (normalize expr), atoms

(* TODO: expr_to_trm, the reverse conversion *)

(* TODO: define an operation, following the pattern of trm_to_doc / trm_to_string

  val expr_to_string (atoms:atom_map) (e:expr) : string

    - print int and double as their values
    - print atom id  as  "<id>[t]"    where t is the term bound to id in atoms
    - print expr_sum as    a1 * x1 + a2 * x2 + ...   but omitting the "ai *" part when ai = 1
        and omitting the "* xi" part when it xi = 1
      if the sum carries an empty list, it is printed as 0
    - print expr_prod as   x1 ^ a1 + x2 ^ a2 + ...   but omitting the "^ ai" part when ai = 1
      if the product carries an empty list, it is printed as 1
*)

(*-----------------------------------------------------------------------------------------*)

(* [gather] regroups similar expressions that appear inside a same product or sum.
   For example, [2 * e1 + (-1)*e1] simplifies to [e1]
   and [e1 * e2 * e1^(-1)] simplifies to [e2]. *)

let gather_one (e : expr) : expr =
  let gather_wexprs (wes : wexprs) : wexprs =
    let rec insert (acc : wexprs) ((w,e) : wexpr) : wexprs =
      match acc with
      | [] -> [(w,e)]
      | (wi,ei)::acc2 -> if e = ei then (wi+w,ei)::acc2 else (wi,ei)::(insert acc2 (w,e))
      in
    List.fold_left insert [] wes
    in
  match e with
  | Expr_sum wes -> Expr_sum (gather_wexprs wes)
  | Expr_prod wes -> Expr_prod (gather_wexprs wes)
  | _ -> e

let gather (recurse : bool) (e : expr) : expr =
  apply_bottom_up_if cleanup_true recurse gather_one e

(* [expand_one e] expands sums that appear inside a product.
    For example, [e1 * (e2 + e3)] becomes [e1 * e2 + e1 * e3].
    The function is the identity if no expansion can be performed. *)

let expand_one (e : expr) : expr =
  let aux ((w,e) : wexpr) (acc : exprs) : exprs =
    match (w,e) with
    | 1, (Expr_sum wes) -> List.concat_map (fun (wk,ek) -> List.map (fun ei -> expr_mul (Expr_int wk) (expr_mul (1,ek) ei)) acc) wes
    | _ -> List.map (fun ei -> expr_mul (w,e) ei) acc
    in
  match e with
  | Expr_prod wes ->
      let exprs_in_sum = List.fold_right aux wes [Expr_int 1] in
      let wes_in_sum = List.map (fun e -> (1,e)) exprs_in_sum in
      normalize_one (Expr_sum wes_in_sum)
  | _ -> e

(* [expand] calls [expand_one] recursively, calling [gather] and [normalize]
   operations after each step. *)
let expand (recurse : bool) (e : expr) : expr =
  let tr (ei : expr) : expr =
    normalize_one (gather_one (expand_one ei)) in
  apply_bottom_up_if cleanup_true recurse tr e


(*-----------------------------------------------------------------------------------------*)

let trm_transfo (f : expr -> expr) (t : trm) : trm =
  let expr, atoms = trm_to_expr t in
  expr_to_trm atoms (f expr)

(* in file Arith, we can define  expand ?(recurse:bool=true) as
      ArithSimplifier.(apply_arith_transfo (expand recurse))
*)




