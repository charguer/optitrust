open Ast
open PPrint

(* ***********************************************************************************
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [apply_aux op arg t]: apply binary_operation op on [t] with the second arguement of the operation being [arg]
    params:
      [op]: the binary operation going to be applied
      [arg]: the second argument after [t] in the performed operation
      [t]: the first argument in the performed operation
    return:
      the ast of the binary operation
*)
let apply_aux (op : binary_op) (arg : trm) (t : trm) : trm =
  trm_apps (trm_binop op) [t; arg]

let apply (op : binary_op) (arg : trm) : Target.Transfo.local =
  Target.apply_on_path (apply_aux op arg)



(******************************************************************************)
(*                          Simplifier                                        *)
(******************************************************************************)

type id = int

let next_id = Tools.fresh_generator ()
type expr =
  | Expr_int of int
  | Expr_double of float
  | Expr_atom of id
  | Expr_sum of wexprs
  | Expr_prod of wexprs

(* list of expressions *)
and exprs = expr list

(* weighted list of expressions *)

and wexprs = wexpr list

and wexpr = (int * expr)

(* a map from atom ids to the corresponding terms *)
module Atom_map = Map.Make(Int)

type atom_map = trm Atom_map.t

(* [print_atom_map k v] pretty print the keys and values of an atom_map  *)
let print_atom_map (k : int) (v : trm) : unit =
  Tools.printf "Atom_id : %d assigned to node %s _n" k (Ast_to_c.ast_to_string v)


(******************************************************************************)
(*                          Smart constructors                                *)
(******************************************************************************)

(* [expr_mul we1 e]  *)
let expr_mul (we1 : wexprs) (e : expr) : expr =
  match e with
  | Expr_prod [_,Expr_int 1] -> Expr_prod we1
  | Expr_prod wes -> Expr_prod (we1 @ wes)
  | _ -> Expr_prod  ((1,e) :: we1)

(* [expr_add we1 e]  *)
let expr_add (we1 : wexprs) (e : expr) : expr =
  match e with
  | Expr_sum [_,Expr_int 0] -> Expr_sum we1
  | Expr_sum wes -> Expr_sum (we1 @ wes)
  | _ -> Expr_sum ((1, e) :: we1)


(*-----------------------------------------------------------------------------------------*)

(* [apply_bottom_up] is a combinator that takes a transformation and applies it recursively,
   bottom up through a term.
*)
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
                                | (_wi, Expr_int 0) | (_wi, Expr_double 0.) -> []
                                | (0, _ei) -> []
                                | (1, Expr_sum wesi) -> wesi
                                | (ai, Expr_prod [(1, Expr_int bi); (1,ei)]) -> [(ai * bi, ei)]
                                | we -> [we]) wes)

    | Expr_prod [(1,Expr_int n);(1,e)]
    | Expr_prod [(1,e);(1,Expr_int n)] -> Expr_sum [(n,e)]
    | Expr_prod wes -> Expr_prod (List.concat_map (function
                                 | (_wi, Expr_int 1) | (_wi, Expr_double 1.) -> []
                                 | (0, _ei) -> []
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


(* auxiliary function for [trm_to_naive_expr] *)
let create_or_reuse_atom_for_trm (atoms : atom_map ref) (t : trm) : id =
  let no_id = -1 in
  let occ = ref no_id in
  Atom_map.iter (fun id tid -> if !occ = no_id && Internal.same_trm t tid then occ := id) !atoms;
  if !occ = no_id then begin
    let new_id = next_id() in
    atoms := Atom_map.add new_id t !atoms;
    occ := new_id
  end;
  !occ

(* Conversion of a trm from the AST into an expr,
   plus a map that for each atom gives the corresponding term *)
let trm_to_naive_expr (t : trm) : expr * atom_map =
  let atoms = ref Atom_map.empty in
  let rec aux (t : trm) : expr =
    let not_expression = Expr_atom (create_or_reuse_atom_for_trm atoms t) in
    match t.desc with
      | Trm_val (Val_lit (Lit_int n)) -> Expr_int n
      | Trm_val (Val_lit (Lit_double n)) -> Expr_double n
      | Trm_apps (f, [t1; t2]) ->
        begin match trm_prim_inv f with
        | Some (Prim_binop b) ->
          begin match b with
          | Binop_add | Binop_sub ->
              let w = match  b with | Binop_add -> 1 | Binop_sub -> -1 | _ -> assert false in
              Expr_sum [(1, aux t1); (w, aux t2)]
          | Binop_mul | Binop_div ->
              let w = match  b with | Binop_mul -> 1 | Binop_div -> -1 | _ -> assert false in
              Expr_prod [(1, aux t1); (w, aux t2)]
          | _ -> not_expression
          end
        | _ -> not_expression
        end
      | Trm_apps (f, [t1]) ->
        begin match trm_prim_inv f with
        | Some (Prim_unop Unop_neg) ->
          Expr_sum [(-1, aux t1)]
        | _ ->
          not_expression
        end
      | _ -> not_expression
    in
    let res = aux t in
    res, !atoms


(* [is_one e] check is e = 1 *)
let is_one (e : expr) : bool =
  match e with
  | Expr_int 1 | Expr_double 1.0 -> true
  | _ -> false

let parens_if_neg (n:int) (d:document) : document =
  if n < 0 then parens d else d

(* [expr_to_string atoms e] convert an expression to a string,
    NOTE: Only for debugging
*)
let expr_to_string (atoms : atom_map) (e : expr) : string =
  let power_to_doc (base : document) (power : int) : document =
     (* if power = 0 then string "1"
     else if power < 0 then string "1./" ^^ parens (power_to_doc base (-power))
     else Tools.list_to_doc ~sep:(star) ~bounds:[empty;empty] (List.init power (fun _ -> base)) *)
     base ^^ string "^" ^^ string (string_of_int power)
     in
  let rec aux (e : expr) : document =
    match e with
    | Expr_int n -> string (string_of_int n)
    | Expr_double n -> string (string_of_float n)
    | Expr_sum we ->
      begin match we with
      | [] -> Printf.printf "expr: Expr_sum [] should never appear";
        (string (string_of_int 0))
      | _ ->
        let we_l = List.map (fun (w, e) ->
          if is_one e then
            string (string_of_int w)
          else begin
            let s = aux e in
            if w = 1
              then s
              else ((parens_if_neg w (string (string_of_int w))) ^^ star ^^ s)
          end
        ) we in
        Tools.list_to_doc ~sep:plus ~bounds:[lparen; rparen] we_l
      end
    | Expr_prod we ->
      begin match we with
      | [] -> Printf.printf "expr: Expr_prod [] should never appear";
        string (string_of_int 1)
      | _ ->
        let we_l = List.map (fun (w, e) ->
        power_to_doc (aux e) w
      ) we in
      Tools.list_to_doc ~sep:star ~bounds:[lparen; rparen] we_l
      end
    | Expr_atom id ->
      begin match Atom_map.find_opt id atoms with
      | Some t1 -> (Ast_to_c.trm_to_doc t1)
      | _  -> fail None "expr_to_string: couldn't convert an atom expr to a trm"
      end
  in
  Tools.document_to_string (aux e)


(* [trm_to_expr t] convert trm [t] to an expression*)
let trm_to_expr (t : trm) : expr * atom_map =
  let expr, atoms = trm_to_naive_expr t in
  Tools.printf "Expr after conversion: %s\n" (expr_to_string atoms expr);
  let res = normalize expr in
  Tools.printf "Expr after normalization: %s\n" (expr_to_string atoms res);
  res, atoms

(* [expr_to_trm e ] convert expr [e] to trm  *)
let expr_to_trm (atoms : atom_map) (e : expr) : trm =
  let rec aux (e : expr) : trm =
    match e with
    | Expr_int n -> trm_int n
    | Expr_double n -> trm_double n
    | Expr_sum we ->
      let we_l = List.map (fun (w, e) -> if w = 1 then aux e else trm_apps (trm_binop Binop_mul) [trm_int w; aux e]) we in
      Tools.fold_lefti (fun i acc x ->
        if i = 0 then x else trm_apps (trm_binop Binop_add) [x; acc]
      ) (trm_unit ()) we_l
    | Expr_prod  we ->
      (* LATER: Since there isn't any power operator in C the last line is the same was the one in the previous case *)
      let we_l = List.map (fun (w, e) -> if w = 1 then aux e else trm_apps (trm_binop Binop_mul) [trm_int w; aux e]) we in
      Tools.fold_lefti (fun i acc x ->
        if i = 0 then x else trm_apps (trm_binop Binop_mul) [x; acc]
      ) (trm_unit ()) we_l
    | Expr_atom id ->
        begin match Atom_map.find_opt id atoms with
        | Some t1 -> t1
        | _ -> fail None "expr_to_trm: couldn't convert an atom expr to a trm"
        end
    in
  aux e

(* [gather_one e] regroups similar expression that appear inside a same product or sum
      For example, [2 * e1 + (-1)*e1] simplifies to [e1]
      and [e1 * e2 * e1^(-1)] simplifies to [e2].
 *)
(* LATER: Use a map instead of a list *)
let gather_one (e : expr) : expr =
  let rec insert (acc : wexprs) ((w,e) : wexpr) : wexprs =
      match acc with
      | [] -> [(w,e)]
      | (wi,ei)::acc2 -> if e = ei then (wi+w,ei)::acc2 else (wi,ei)::(insert acc2 (w,e))
    in
  let gather_wexprs (wes : wexprs) : wexprs =
     List.fold_left insert [] wes
    in
  match e with
  | Expr_sum wes -> Expr_sum (gather_wexprs wes)
  | Expr_prod wes -> Expr_prod (gather_wexprs wes)
  | _ -> e

  (* [gather recurese_bool e] apply gather one in a full expression if recurse is set to true *)
let gather (recurse : bool) (e : expr) : expr =
  let tr (ei : expr) : expr =
    normalize_one (gather_one ei) in
  apply_bottom_up_if cleanup_true recurse tr e

(* [expand_one e] expends sums that appear inside product.
    For example, [e1 * (e2 + e3)] becomes [e1 * e2 + e1 * e3]
    The function is identity if no expansion can be performed


  | 1, (Expr_sum wes) ->
    wes=(e2+e3)   acc=(w1*a1+a2+a3)  ->  (e2*w1*a1 +e2*a2 + e2*a3   +  e3*a2 + e3*a2 + e3*a2)
*)

(*
let expand_one (e : expr) : expr =
  let aux ((w,e) : wexpr) (acc : exprs) : exprs =
    match (w,e) with
    | 1, (Expr_sum wes) ->
        List.concat_map (fun (wk,ek) ->
           List.map (fun ei ->
              expr_mul [(1,Expr_int wk)] (expr_mul [(1,ek)] ei))
            ) acc
        ) wes
    | _ -> List.map (fun ei -> expr_mul [(w,e)] ei) acc
    in
  match e with
  | Expr_prod wes ->
      let exprs_in_sum = List.fold_right aux wes [Expr_int 1] in
      let wes_in_sum = List.map (fun e -> (1,e)) exprs_in_sum in
      normalize_one (Expr_sum wes_in_sum)
  | _ -> e
*)

let expand_one (e : expr) : expr =
  let aux ((w,e) : wexpr) (acc : exprs) : exprs =
    match (w,e) with
    | 1, (Expr_sum wes) ->
      List.concat_map (fun (_wk, ek) ->
        List.map (fun ei -> expr_mul [(w,e)] (expr_mul [(1,ek)] ei)) acc
      ) wes
    | _ ->
      List.map (fun ei -> expr_mul [(w,e)] ei) acc
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


let trm_transfo  (f : expr -> expr) (t : trm) : trm =
  let expr, atoms = trm_to_expr t in
  expr_to_trm atoms (f expr)

(* [simplify_aux f t] convert node [t] to an expression then apply the simplifier [f], then convert it back to a trm
      params:
        [f]: simplifier function
        [t]: the node on which the simplifications should be performed
      return:
        update t with the simplified expressions
*)
let simplify_aux  (f : expr -> expr) (t : trm) : trm =
  let expr, atoms = trm_to_expr t in
  expr_to_trm atoms (f expr)

let simplify (f : expr -> expr) : Target.Transfo.local =
  Target.apply_on_path (simplify_aux f)





