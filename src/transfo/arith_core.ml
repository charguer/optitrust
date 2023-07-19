
open Syntax
open PPrint
open Target

(* debug flags *)
let debug = false
let debug_rec = false

(* mark used for marking trms that should be skipped by the simplifier *)
let mark_nosimpl = "__arith_core_nosimpl"

(* [has_mark_nosimplf t]: check if [t] should be skipped by the simplifier or not.*)
let has_mark_nosimpl (t : trm) : bool =
  trm_has_mark mark_nosimpl t
  (* LATER Ast.trm_has_mark *)

(* arithmetic operation type *)
type arith_op =
  | Arith_shift
  | Arith_scale

(* [transform_aux aop inv pre_cast post_cast u t]: shifts or scale the right hand
    side of a set operation with term [u]
    [aop] - a flag to decide if the arithmetic operation should be Arith_scale
       or Arith_shift
    [inv] - a flag for the sign(plus or minus) of shifting
    [u] - shift size
    [pre_cast] - casting of type [pre_cast] performed on the right hand side of the
      set operation before shifting
    [post_cast] - casting of type [post_cast] performed after shifting
    [t] - the ast of the set operation *)
let transform_aux (aop : arith_op) (inv : bool) (u : trm) (pre_cast : typ option)
  (post_cast : typ option)(t : trm) : trm =
  let binop_op = match aop with
    | Arith_shift -> if inv then Binop_sub else Binop_add
    | Arith_scale -> if inv then Binop_div else Binop_mul
    in
  let trm_apps_binop t1 t2 = trm_apps (trm_binop binop_op) [t1; t2] in
  match t.desc with
  | Trm_apps(f, [lhs; rhs]) when is_set_operation t ->
    begin match pre_cast, post_cast with
     | None, None -> trm_replace (Trm_apps (f, [lhs; trm_apps_binop rhs u])) t

     | None, Some ty -> trm_replace (Trm_apps (f, [lhs;trm_cast ty (trm_apps_binop rhs u)])) t

     | Some ty, None -> trm_replace (Trm_apps (f, [lhs;
                    trm_apps_binop (trm_cast ty rhs) u])) t
     | _ -> fail t.loc "Arith_core.transform_aux: can't apply both pre-casting
                        and post-casting"
    end
  | Trm_apps (_, [arg]) when is_get_operation t ->
    begin match pre_cast, post_cast with
     | None , None -> trm_apps_binop t u
     | None, Some ty -> trm_cast ty (trm_apps_binop t u)
     | Some ty, None -> trm_apps_binop (trm_cast ty t)  u
     | _ -> fail t.loc "Arith_core.transfom_aux: can't apply both pre-casting
                        and post-casting"
    end
  | _ -> fail t.loc "Arith_core.transform_aux: expected a get or a set operation"

(* [transform aop inv pre_cast post_cast u t p: applies [transform_aux] at the trm with path [p]. *)
let transform (aop : arith_op)(inv : bool) (u : trm) (pre_cast : typ option)
  (post_cast : typ option) : Transfo.local =
  apply_on_path (transform_aux aop inv u pre_cast post_cast)


(* [apply_aux op arg t]: applies binary_operation [op] on [t] with the second  argument of the operation being [arg],
    [op] -the binary operation to apply.
    [arg] - the second operand of [op].
    [t] - the first argument in the performed operation. *)
let apply_aux (op : binary_op) (arg : trm) (t : trm) : trm =
  trm_apps (trm_binop op) [t; arg]

(* [apply op arg t p]: applies [transform_aux] at the trm [t] with path [p]. *)
let apply (op : binary_op) (arg : trm) : Transfo.local =
  apply_on_path (apply_aux op arg)

(******************************************************************************)
(*                          Types                                        *)
(******************************************************************************)

(* id type *)
type id = int

(* generate a new id *)
let next_id = Tools.fresh_generator ()

type loc = location (* alias *)

(* [expr]: expression type, it may be a literal expression, an atom expression or an arithmetic expression *)
type expr = {
  expr_desc : expr_desc;
  expr_typ : typ option; (* LATER: type should not be optional *)
  expr_loc : loc; }

(*  Grammar of expressions.
  Atoms : uninterpreted expressions
  Int, Double : base types
  Expr_sum  : w1 * e1 + ... + wN * eN
  Expr_prod : e1 ^ w1 + ... + eN ^ wN
  Supported arithmetic binary operators on int :
  | Binop_div           (* a / b *) -> div_floor : a/b  on int
  | Binop_mod           (* a % b *)
  | Binop_shiftl        (* a >> k*)
  | Binop_shiftr        (* a << k *)
  | Binop_xor           (* a ^ b *)
  | Binop_bitwise_and   (* a & b *)
  | Binop_bitwise_or    (* a | b *)
  --LATER: add fmod, not clear whether it should be a function or a binop in ast.ml;
*)
and expr_desc =
  | Expr_int of int
  | Expr_double of float
  | Expr_atom of id
  | Expr_sum of wexprs
  | Expr_prod of wexprs
  | Expr_binop of binary_op * expr * expr

(* TODO ARTHUR: add types on nodes AND
   disable simplification of integer division
   TODO: add tests for this *)

(* [exprs]: list of expressions *)
and exprs = expr list

(* [wexprs]: weighted list of expressions *)
and wexprs = wexpr list

(* [wexpr]: weighted expression *)
and wexpr = (int * expr)

(* [Atom_map]: a map from atom ids to the corresponding terms *)
module Atom_map = Map.Make(Int)

(* [atom_map]: atom map for storing atoms *)
type atom_map = trm Atom_map.t

(* [no_atoms]: empty atom map *)
let no_atoms = Atom_map.empty

(******************************************************************************)
(*                          Smart constructors                                *)
(******************************************************************************)

let is_integer_typ (typ : typ option) : bool =
  match typ with
  | Some t ->
    begin match t.typ_desc with
    | Typ_int -> true
    | Typ_float | Typ_double -> false
    | _ -> failwith (Printf.sprintf "unsupported type: %s" (AstC_to_c.typ_to_string t))
    end
  | _ ->
    printf "WARNING: trm_to_naive_expr: missing type information, assuming floating point\n";
    (* if true then failwith "DEBUGME"; *)
    false (* LATER: fix this assumption *)

let unsupported_binop (op : binary_op) =
  let s = Tools.document_to_string (Ast_to_text.print_binop op) in
  fail None ("Arith_core: unsupported binop: " ^ s)

let expr_make ?(loc : loc) ~(typ : typ option) (desc : expr_desc) : expr =
  { expr_desc = desc;
    expr_typ = typ;
    expr_loc = loc; }

let expr_make_like (e : expr) (desc : expr_desc) : expr =
  { e with expr_desc = desc }

(* [expr_int n] produces the integer value [n] *)
let expr_int ?(loc : loc) (n : int) : expr =
  expr_make ?loc ~typ:(Some (typ_int ())) (Expr_int n)

(* [expr_float f] produces the float value [n] *)
let expr_float ?(loc : loc) ?(typ : typ option = (Some (typ_float ()))) (f : float) : expr =
  expr_make ?loc ~typ (Expr_double f)

(* [expr_double f] produces the double value [n] *)
let expr_double ?(loc : loc) (f : float) : expr =
  expr_float ?loc ~typ:(Some (typ_double ())) f

(* [expr_one typ] produces either [expr_int 1] or [expr_double 1.0] depending on the type *)
let expr_one (typ : typ option) : expr =
  match typ with
  | Some t ->
    begin match t.typ_desc with
    | Typ_int -> expr_int 1
    | Typ_float -> expr_float 1.0
    | Typ_double -> expr_double 1.0
    | _ -> failwith (Printf.sprintf "unsupported type: %s" (AstC_to_c.typ_to_string t))
    end
  | None -> fail None "expr_one: requires a known type"

(* [expr_atom id] produces a variable [id], denoting an arbitrary subterm form ast.ml *)
let expr_atom ?(loc : loc) ~(typ : typ option) (id : id) : expr =
  expr_make ?loc ~typ (Expr_atom id)

(* [expr_sum [(w1,e1);(w2,e2)] produces [w1*e1 + w2*e2] *)
let expr_sum ?(loc : loc) ~(typ : typ option) (wes : wexprs) : expr =
  expr_make ?loc ~typ (Expr_sum wes)

(* [expr_sum_nonweighted es] produces [e1 + e2 + ... + en] *)
let expr_sum_nonweighted ?(loc : loc) ~(typ : typ option) (es : exprs) : expr =
   expr_sum ?loc ~typ (List.map (fun e -> (1,e)) es)

(* [expr_neg e1] produces [-e1] *)
let expr_neg ?(loc : loc) ~(typ : typ option) (e1 : expr) : expr =
  expr_sum ?loc ~typ [(-1,e1)]

(* [expr_add e1 e2] produces [e1 + e2] *)
let expr_add ?(loc : loc) ~(typ : typ option) (e1 : expr) (e2 : expr) : expr =
  expr_sum ?loc ~typ [(1,e1); (1,e2)]

(* [expr_sub e1 e2] produces [e1 - e2] *)
let expr_sub ?(loc : loc) ~(typ : typ option) (e1 : expr) (e2 : expr) : expr =
  expr_sum ?loc ~typ [(1,e1); (-1,e2)]

(* [expr_prod [(w1,e1);(w2,e2)] produces [e1^w1 * e2^w2] *)
let expr_prod ?(loc : loc) ~(typ : typ option) (wes : wexprs) : expr =
  expr_make ?loc ~typ (Expr_prod wes)

(* [expr_prod_nonweighted es] produces [e1 * e2 * ... * en] *)
let expr_prod_nonweighted ?(loc : loc) ~(typ : typ option) (es : exprs) : expr =
   expr_prod ?loc ~typ (List.map (fun e -> (1,e)) es)

(* [expr_pow e1 w] produces [e1 ^ w] *)
let expr_pow ?(loc : loc) ~(typ : typ option) (e1 : expr) (w : int) : expr =
  expr_prod ?loc ~typ [(w,e1)]

(* [expr_mul e1 e2] produces [e1 * e2] *)
let expr_mul ?(loc : loc) ~(typ : typ option) (e1 : expr) (e2 : expr) : expr =
  expr_prod ?loc ~typ [(1,e1); (1,e2)]

(* [expr_div e1 e2] produces [e1 / e2];
   if arguments are integers, then [e1] is assumed to be divisible by [e2]  *)
let expr_div ?(loc : loc) ~(typ : typ option) (e1 : expr) (e2 : expr) : expr =
  expr_prod ?loc ~typ [(1,e1); (-1,e2)]

(* [expr_binop op e1 e2] produces the operation [op e1 e2] *)
let expr_binop ?(loc : loc) ~(typ : typ option) (op : binary_op) (e1 : expr) (e2 : expr) : expr =
  expr_make ?loc ~typ (Expr_binop (op, e1, e2))

(* [expr_div_floor e1 e2] produces the integer division [e1 / e2], rounded below *)
let expr_div_floor ?(loc : loc) (e1 : expr) (e2 : expr) : expr =
  expr_binop ?loc ~typ:(Some (typ_int ())) Binop_div e1 e2

(* LATER: might add constructors for other binary_ops *)


(******************************************************************************)
(*                          Normalize                                 *)
(******************************************************************************)

(* [normalize_one e]:
   - collapses nested sums onto a single sum, and likewise for nested products
   - turns a product of an expression with a constant integer as a weighted
     expression in the parent sum
   - eliminates products and sums with a single expression of weight one
   - eliminates products and sums with an empty list
   - eliminates elements with weight zero
   - eliminates +0 is sums and *1 in produts
   - simplifies interger-division by 1
   - simplifies modulo operations applied to zero
   - simplifies binary shifting operations by zero
   *)
let normalize_one (e : expr) : expr =
  let e =
    let mk desc = expr_make_like e desc in
    match e.expr_desc with
    | Expr_sum wes ->
        mk (Expr_sum (List.concat_map (function
          | (_ai, { expr_desc = Expr_int 0; _} ) -> []
          | (_ai, { expr_desc = Expr_double 0.; _ }) -> []
          | (1, { expr_desc = Expr_int n; _ }) -> [(n, expr_int 1)]
          | (-1, { expr_desc = Expr_int n; _ }) -> [(-n, expr_int 1)]
          | (0, _ei) -> []
          | (1, { expr_desc = Expr_sum wesi; _ }) -> wesi
          | (-1, { expr_desc = Expr_sum wesi; _ }) -> List.map (fun (ai, ei) -> (-ai,ei)) wesi
          | (ai, { expr_desc = Expr_prod [(1, { expr_desc = Expr_int bi; _}); (1,ei)]; _ }) (* Optional? *)
          | (ai, { expr_desc = Expr_prod [(1,ei); (1, { expr_desc = Expr_int bi; _})]; _ }) (* Optional? *)
          | (ai, { expr_desc = Expr_prod [(bi, { expr_desc = Expr_int 1; _}); (1,ei)]; _ })
          | (ai, { expr_desc = Expr_prod [(1,ei); (bi, { expr_desc = Expr_int 1; _})]; _ }) -> [(ai * bi, ei)]
          | we -> [we]) wes))
    | Expr_prod wes ->
      let is_val = ref None in  (* TODO: replace side effect with dedicated recursion for early abort *)
      let p2 = Expr_prod (List.concat_map (function
      | (_ai, ({ expr_desc = Expr_int 0; _ } as ezero)) ->
        is_val := Some ezero;
        []
      | (_ai, ({ expr_desc = Expr_double 0.; _} as ezero)) ->
        is_val := Some ezero;
        []
      | (_ai, { expr_desc = Expr_int 1; _ }) -> []
      | (_ai, { expr_desc = Expr_double 1.; _}) -> []
      | (0, _ei) -> []
      | (1, { expr_desc = Expr_prod wesi; _}) -> wesi
      | (-1, { expr_desc = Expr_prod wesi; _}) -> List.map (fun (w,ei) -> (-w, ei)) wesi
      | (ai, { expr_desc = Expr_sum [(bi, { expr_desc = Expr_int 1; _})]; expr_loc = loc; _}) -> [(ai, expr_int ?loc bi)]
      | (ai, { expr_desc = Expr_sum [(bi, ei)]; expr_loc = loc; _}) when is_integer_typ e.expr_typ ->
        [(ai, expr_int ?loc bi); (ai, ei)]
      | we -> [we]) wes)
      in
      begin match !is_val with
      | None -> mk p2
      | Some v -> v
      end
    (* [e1 / 1 = 1] *)
    | Expr_binop (Binop_div, e1, { expr_desc = Expr_int 1; _}) -> e1
    | Expr_binop (Binop_div, e1, { expr_desc = Expr_prod []; _}) -> e1
    (* [0 mod e2 = 0] *)
    | Expr_binop (Binop_mod, ({ expr_desc = Expr_int 0; _} as ezero), e2) -> ezero
    (* [e1 << 0 = e1] and [e1 >> 0 = e1] *)
    | Expr_binop ((Binop_shiftr | Binop_shiftl), e1, { expr_desc = Expr_int 0; _}) -> e1
    | _ -> e
    in
  match e.expr_desc with
  | Expr_sum [] -> expr_int 0
  | Expr_prod [] -> expr_int 1
  | Expr_sum [(1,e1)] -> e1
  | Expr_prod [(1,e1)] -> e1
  | _ -> e

(* [normalize e]: applies [normalize_one] in a bottom up fashion.
   Its definition appears further below. *)


(******************************************************************************)
(*                          From expr to string                                 *)
(******************************************************************************)

(* [is_one e]: checks if e == 1 *)
let is_one (e : expr) : bool =
  match e.expr_desc with
  | Expr_int 1 | Expr_double 1.0 -> true
  | _ -> false

(* [parens_if_neg n d]: if [n] is negative then it add parentheses around [d] *)
let parens_if_neg (n:int) (d:document) : document =
  if n < 0 then parens d else d

(* [expr_to_string atoms e]: convert an expression to a string, in AST form *)
let expr_to_string (atoms : atom_map) (e : expr) : string =
  let rec aux (e : expr) : document =
    let auxw ((w,e) : wexpr) : document =
      parens ((string (string_of_int w)) ^^ comma ^^ aux e) in
    let auxwes (we : wexprs) : document =
      Tools.list_to_doc ~bounds:[lbrace; rbrace] (List.map auxw we) in
    match e.expr_desc with
    | Expr_int n -> string (string_of_int n)
    | Expr_double n -> string (string_of_float n)
    | Expr_sum wes -> string "Sum" ^^ (auxwes wes)
    | Expr_prod wes -> string "Prod" ^^ (auxwes wes)
    | Expr_binop (op, e1, e2) ->
        let sop = match op with
          | Binop_div -> "Div"
          | Binop_exact_div -> "ExactDiv"
          | Binop_mod -> "Mod"
          | Binop_shiftl -> "ShiftL"
          | Binop_shiftr -> "ShiftR"
          | Binop_xor -> "Xor"
          | Binop_bitwise_and -> "BitwiseAnd"
          | Binop_bitwise_or -> "BitwiseOr"
          | _ -> unsupported_binop op
          in
        string sop ^^ string "(" ^^ aux e1 ^^ string "," ^^ aux e2 ^^ string ")"
    | Expr_atom id ->
        begin match Atom_map.find_opt id atoms with
        | Some t1 ->
            begin match t1.desc with
            | Trm_var (_, x) -> string x.qvar_var
            | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _},
               [{desc = Trm_var (_, x); _}]) -> string x.qvar_var
            | _ -> braces (AstC_to_c.trm_to_doc t1)
            end
        | _  ->
          (* To identify atoms, we use letters 'a', 'b', ... then
             'aa', 'ab', etc, ... then use 'a676', 'a677', etc... *)
          if id > 26*26 then braces (string ("a" ^ (string_of_int id))) else
            let string_of_small_int i =
              let c = Char.chr ((Char.code 'a') + i) in
              String.make 1 c in
            let s1 = (if id >= 26 then string_of_small_int (id / 26) else "") in
            let s2 = string_of_small_int (id mod 26) in
            braces (string "!" ^^ string (s1 ^ s2))
        end
  in
  Tools.document_to_string (aux e)

(* [expr_to_math_string atoms e]: converts an expression to a string, using mathematical notations *)
let expr_to_math_string (atoms : atom_map) (e : expr) : string =
  let power_to_doc (base : document) (power : int) : document =
     base ^^ string "^" ^^ string (string_of_int power)
     in
  let rec aux (e : expr) : document =
    match e.expr_desc with
    | Expr_int n -> string (string_of_int n)
    | Expr_double n -> string (string_of_float n)
    | Expr_sum we ->
      begin match we with
      | [] -> Printf.printf "Arith_core.expr: Expr_sum [] should never appear";
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
      | [] -> Printf.printf "Arith_core.expr: Expr_prod [] should never appear";
        string (string_of_int 1)
      | _ ->
        let we_l = List.map (fun (w, e) ->
        power_to_doc (aux e) w
      ) we in
      Tools.list_to_doc ~sep:star ~bounds:[lparen; rparen] we_l
      end
     | Expr_binop (op, e1, e2) ->
        let sop = match op with
          | Binop_div -> "/"
          | Binop_shiftr -> ">>"
          | Binop_shiftl -> "<<"
          | Binop_xor -> "^" (* LATER: use other symbol to avoid confusion *)
          | Binop_bitwise_and -> "&" (* LATER: use other symbol to avoid confusion *)
          | Binop_bitwise_or -> "|" (* LATER: use other symbol to avoid confusion *)
          | _ -> unsupported_binop op
          in
        parens (aux e1) ^^ string sop ^^ parens (aux e2)
     | Expr_atom id ->
      begin match Atom_map.find_opt id atoms with
      | Some t1 -> (AstC_to_c.trm_to_doc t1)
      | _  -> fail None "Arith_core.expr_to_math_string: couldn't convert
                        an atom expr to a trm"
      end
  in
  Tools.document_to_string (aux e)


(******************************************************************************)
(*                          Traversals                                        *)
(******************************************************************************)

(* [identity] transformation *)
let identity (e : expr) : expr =
  e

(* [apply_bottom_up]: is a combinator that takes a transformation and applies it recursively,
   bottom up through a term. *)
let rec apply_bottom_up (f : expr -> expr) (e : expr) : expr =
  let apply_wexprs (wes : wexprs) : wexprs =
    List.map (fun (w,e) -> (w, apply_bottom_up f e)) wes in
  let mk desc = expr_make_like e desc in
  match e.expr_desc with
  | Expr_sum wes -> f (mk (Expr_sum (apply_wexprs wes)))
  | Expr_prod wes -> f (mk (Expr_prod (apply_wexprs wes)))
  | Expr_binop (op, e1, e2) -> f (mk (Expr_binop (op, f e1, f e2)))
  | _ -> f e (* LATER: don't use a catch-all branch, as it is error prone in case we add constructors *)

(* [normalize e]: applies [normalize_one] in a bottom up fashion *)
let normalize (e : expr) : expr =
  apply_bottom_up normalize_one e

(* [cleanup_true]: perform cleanup after transformation; else [cleanup_false] *)
let cleanup_true = true
let cleanup_false = false

(* [recurse_true]: apply transformation in depth; else [recurse_false] *)
let recurse_true = true
let recurse_false = false

(* [apply_bottom_up_if]: is a combinator for either applying a transformation recursively
   or applying it only at the top level, according to the [recurse] argument.
   If the [cleanup] argument is true, then after each call to the transformation,
   the operation [normalize_one] is called. *)
let apply_bottom_up_if (recurse : bool) (cleanup : bool) (f : expr -> expr)
  (e : expr) : expr =
  let f_with_cleanup e =
    let e1 = (if cleanup then normalize_one else identity) e in
    let e2 = f e1 in
    let e3 = (if cleanup then normalize_one else identity) e2 in
    if debug_rec
      then Printf.printf "Step:\n\t%s\n\t%s\n\t%s\n" (expr_to_string no_atoms e)
        (expr_to_string no_atoms e2) (expr_to_string no_atoms e3);
    e3 in
  if recurse
    then apply_bottom_up f_with_cleanup e
    else f_with_cleanup e

(* [apply_bottom_up_debug e]: function used only for debugging purposes *)
let apply_bottom_up_debug (e : expr) : expr =
  let f ei =
    if debug then Printf.printf "Bottom-up %s\n" (expr_to_string no_atoms ei);
    ei in
  apply_bottom_up f e

(* [apply_bottom_up_if_debug]: function used only for debugging purposes *)
let apply_bottom_up_if_debug (recurse : bool) (cleanup : bool) (e : expr) : expr =
  let f ei =
    let ej = (if cleanup then normalize_one else identity) ei in
    if debug
      then Printf.printf "Bottom-up-if:\n\t%s\n\t%s\n"
            (expr_to_string no_atoms ei) (expr_to_string no_atoms ej); ej
    in
  apply_bottom_up_if recurse cleanup f e


(******************************************************************************)
(*                          From trm to expr                                 *)
(******************************************************************************)

(* [create_or_reuse_atom_for_trm atoms t]: auxiliary function for [trm_to_naive_expr]*)
let create_or_reuse_atom_for_trm (atoms : atom_map ref) (t : trm) : id =
  let no_id = -1 in
  let occ = ref no_id in
  Atom_map.iter (fun id tid ->
    if !occ = no_id && Internal.same_trm t tid then occ := id) !atoms;
    if !occ = no_id
      then begin
        let new_id = next_id() in
        atoms := Atom_map.add new_id t !atoms;
        occ := new_id
      end;
  !occ

(* [trm_to_naive_expr]: conversion of a trm from the AST into an expr, plus a map that for each atom gives
    the corresponding term *)
let trm_to_naive_expr (t : trm) : expr * atom_map =
  let atoms = ref Atom_map.empty in
  let rec aux (t : trm) : expr =
    let loc = t.loc in
    let typ = t.typ in
    let force_atom() = expr_atom ?loc ~typ (create_or_reuse_atom_for_trm atoms t) in
    if has_mark_nosimpl t then force_atom() else
    match t.desc with
     (* Recognize constants *)
     | Trm_val (Val_lit (Lit_int n)) -> expr_int ?loc n
     | Trm_val (Val_lit (Lit_double n)) -> expr_float ?loc ~typ n
     (* Recognize unary operators *)
     | Trm_apps (f, [t1]) ->
       begin match trm_prim_inv f with
        | Some (Prim_unop Unop_minus) -> expr_neg ?loc ~typ (aux t1)
        | _ -> force_atom()
       end
     (* Recognize binary operators *)
     | Trm_apps (f, [t1; t2]) ->
       let is_integer_op () = (* indicate if operation is on int or double *)
          match is_integer_typ t1.typ, is_integer_typ t2.typ with
          | true, true -> true
          | false, false -> false
          | _ ->
            printf "WARNING: arith types differ: %s and %s\n" (Tools.option_to_string AstC_to_c.typ_to_string t1.typ) (Tools.option_to_string AstC_to_c.typ_to_string t2.typ);
            false
            (* LATER: failwith "should not happen" *)
         in
       begin match trm_prim_inv f with
        | Some (Prim_binop op) ->
          begin match op with
          | Binop_add -> expr_add ?loc ~typ (aux t1) (aux t2)
          | Binop_sub -> expr_sub ?loc ~typ (aux t1) (aux t2)
          | Binop_mul -> expr_mul ?loc ~typ (aux t1) (aux t2)
          | Binop_exact_div ->
              if not (is_integer_op())
                then fail t.loc "trm_to_naive_expr: Binop_exact_div expected to be an integer operation";
              expr_div ?loc ~typ (aux t1) (aux t2)
          | Binop_div ->
              if is_integer_op()
                then expr_div_floor ?loc (aux t1) (aux t2)
                else expr_div ?loc ~typ (aux t1) (aux t2)
          | Binop_mod | Binop_shiftl | Binop_shiftr | Binop_xor | Binop_bitwise_and | Binop_bitwise_or ->
              expr_binop op ?loc ~typ (aux t1) (aux t2)
          | _ -> force_atom()
          end
        | _ -> force_atom()
       end
     | _ -> force_atom()
     in
    let res = aux t in
    res, !atoms


(******************************************************************************)
(*                          From expr to trm                                 *)
(******************************************************************************)

(* [trm_to_expr t]: convert trm [t] to an expression*)
let trm_to_expr (t : trm) : expr * atom_map =
  let expr, atoms = trm_to_naive_expr t in
  if debug && false
    then Printf.printf "Expr before conversion: %s\n" (Ast_to_text.ast_to_string t);
  if debug
    then Printf.printf "Expr after conversion: %s\n" (expr_to_string atoms expr);
  let res = normalize expr in
  if debug
    then Printf.printf "Expr after normalization: %s\n" (expr_to_string atoms res);
  res, atoms

(* [expr_to_trm atoms e]: converts expr [e] to trm  *)
let expr_to_trm (atoms : atom_map) (e : expr) : trm =
  let rec aux (e : expr) : trm =
    let loc = e.expr_loc in
    let typ = e.expr_typ in
    match e.expr_desc with
    | Expr_int n -> trm_int ?loc n
    | Expr_double n -> trm_float ?typ ?loc n
    | Expr_sum we ->
      if we = [] then failwith "expr_to_trm: assumes a normalized term";
      Xlist.fold_lefti (fun i acc (w, e) ->
        let (w, e) = match (w,e) with
          | (n,{expr_desc = Expr_int 1; _}) ->
            if n >= 0 then (1, expr_int n) else (-1, expr_int (-n))
          | _ -> (w,e) in
        let e_trm = aux e in
        let abs_w = abs w in
        let abs_t =
          if abs_w = 1 then e_trm else trm_mul ?loc (trm_int abs_w) e_trm
          in
        if i = 0 then begin
          if w >= 0 then abs_t else trm_minus ?loc abs_t
        end else begin
          if w >= 0 then trm_add ?loc acc abs_t else trm_sub ?loc acc abs_t
        end
      ) (trm_unit ()) we
    | Expr_prod wes ->
      (* We have [e1^w1 * e2^w2 * e3^w3].
         We display [e1^w1 * e2^(-w2) * e3^w3 * e4^(-w4)] as [(e1^w1 * e3^w3) / (e2^w2 * e4^w3)],
         Note: the form [e1^0] is supposed to have been eliminated during normalization.
         We display [e1^1] as just [e1].
         We display [e1^3] as [e1 * e1 * e1] -- LATER: use a pow() function in the optitrust library.
         that is, we use at most one division symbol *)

      (* combinators *)
      let trm_one () =
        if is_integer_typ e.expr_typ then trm_int 1 else trm_float ?typ:e.expr_typ 1.0 in
      let trm_div (t1:trm) (t2:trm) : trm =
        let binop_div = if is_integer_typ e.expr_typ then Binop_exact_div else Binop_div in
        trm_apps ?loc (trm_binop binop_div) [t1; t2] in
      let rec trm_mul_nonempty (ts : trm list) : trm =
        match ts with
        | [] -> assert false
        | [t1] -> t1
        | t1 :: t2 :: ts' -> trm_mul_nonempty ((trm_apps ?loc (trm_binop Binop_mul) [t1; t2])::ts')
        in
      let rec trm_power (t:trm) (n:int) : trm =
        if n = 0
          then failwith "Arith_core.expr_to_trm: assumes a normalized term, so (e,0) should not appear in the argument of [Expr_prod]"
          else if n < 0 then assert false (* DEPRECATED  trm_apps ?loc (trm_binop binop_div) [trm_double 1.0; power t (-n)] *)
          else if n = 1 then t
          else trm_apps ?loc (trm_binop Binop_mul) [t; trm_power t (n-1)]
        in

      (* separate positive and negative exponents *)
      if wes = [] then failwith "expr_to_trm: assumes a normalized term";
      let wts = List.map (fun (w,e) -> (w, aux e)) wes in
      let wts_pos = List.filter (fun (w,_t) -> w >= 0) wts in
      let wts_neg = List.filter (fun (w,_t) -> w < 0) wts in
      let ts_pos = List.map (fun (w,t) -> trm_power t w) wts_pos in
      let ts_neg = List.map (fun (w,t) -> trm_power t (-w)) wts_neg in

      begin match ts_pos, ts_neg with
      | [], [] -> failwith "Arith_core.expr_to_trm: assumes a normalized term, so the argument [we] of [Expr_prod] should be nonempty"
      | _, [] -> trm_mul_nonempty ts_pos
      | [], _ -> trm_div (trm_one()) (trm_mul_nonempty ts_neg)
      | _, _ -> trm_div (trm_mul_nonempty ts_pos) (trm_mul_nonempty ts_neg)
          (* LATER: this case is not expected to happen with exact_div *)
      end

    | Expr_binop (op, e1, e2) ->
        trm_apps ?loc (trm_binop op) [aux e1; aux e2]
    | Expr_atom id ->
        begin match Atom_map.find_opt id atoms with
        | Some t1 -> t1
        | _ -> fail None "Arith_core.expr_to_trm: couldn't convert an atom expr to a trm"
        end
    in
  aux e


(******************************************************************************)
(*                          Simpl, gather                                 *)
(******************************************************************************)

let rec same_expr (a : expr) (b : expr) : bool =
  let rec same_desc (a : expr_desc) (b : expr_desc) : bool =
    match (a, b) with
    | (Expr_int a, Expr_int b) -> a = b
    | (Expr_double a, Expr_double b) -> a = b
    | (Expr_atom a, Expr_atom b) -> a = b
    | (Expr_sum a, Expr_sum b) | (Expr_prod a, Expr_prod b) ->
       List.for_all2 same_wexprs a b
    | (Expr_binop (op1, a1, a2), Expr_binop (op2, b1, b2)) ->
       (op1 = op2) && (same_expr a1 b1) && (same_expr a2 b2)
    | _ -> false
  and same_wexprs ((a_id, a_e) : wexpr) ((b_id, b_e) : wexpr) : bool =
    (a_id = b_id) && (same_expr a_e b_e)
  in
  same_desc a.expr_desc b.expr_desc

(* [cancel_div_floor_prod wes n] simplifies
   [Expr_div (Expr_prod wes) e] into [Expr_prod wes'].
   It returns [Some wes'], or [None] if no simplification is possible *)

let rec cancel_div_floor_prod (wes : wexprs) (e : expr) : wexprs option =
  match wes with
  | [] -> None
  | (wi,ei)::wes' when (same_expr ei e) && wi > 0->
      if wi = 1
        then Some wes'
        else Some ((wi-1,ei)::wes')
  | (wi,ei)::wes' ->
      begin match cancel_div_floor_prod wes' e with (* LATER: use a monadic notation? *)
      | None -> None
      | Some res -> Some ((wi,ei)::res)
      end

(* [gather_one e]: regroups similar expression that appear inside a same product
    or sum. For example, [2 * e1 + (-1)*e1] simplifies to [e1] and
    [e1 * e2 * e1^(-1)] simplifies to [e2].
    Also changes [(a / b) / c] into [a / (b * c)], and simplifies
    [(a1 * c * a2) / (b1 * c * b2)] into  [(a1 * a2) / (b1 * b2)]
    where [c] is a common item to the numerator and divisor (order-insensitive).
    This includes simplifications of [a / a] to [1] and of [(a*b)/a] to [b]. *)
let rec gather_one (e : expr) : expr =

  let rec insert (acc : wexprs) ((w,e) : wexpr) : wexprs =
      match acc with
      | [] -> [(w,e)]
      | (wi,ei)::acc2 -> if same_expr e ei
          then (wi+w,ei)::acc2
          else (wi,ei)::(insert acc2 (w,e))
    in
  let gather_wexprs (wes : wexprs) : wexprs =
    List.fold_left insert [] wes
    in
  let loc = e.expr_loc in
  let mk desc = expr_make_like e desc in
  match e.expr_desc with
  | Expr_sum wes -> mk (Expr_sum (gather_wexprs wes))
  | Expr_prod wes -> mk (Expr_prod (gather_wexprs wes))
  (* simplify  [(a / b) / c] into [a / (b * c)] *)
  | Expr_binop (Binop_div, { expr_desc = Expr_binop (Binop_div, e1, e2); _ }, e3) ->
      let e23 = normalize_one (mk (Expr_prod [(1, e2); (1, e3)])) in
      gather_one (mk (Expr_binop (Binop_div, e1, e23))) (* attempt further simplifications *)
  (* simplify [a/a] to [1]. *)
  | Expr_binop (Binop_div, e1, e2) when e1 = e2 -> expr_int ?loc 1
  (* simplify [(a*b)/(c*a)] to [b/c] and [(a^k*b)/(c*a)] to [(a^(k-1)*b)/c]. *)
  | Expr_binop (Binop_div, ({ expr_desc = Expr_prod wes1; _ } as e1),
                           ({ expr_desc = Expr_prod wes2; _ } as e2)) ->
     let rec aux wes1 wes2 : wexprs * wexprs =
       let add_to_snd k (a,b) = (a,k::b) in
       match wes2 with
       | [] -> wes1, []
       | ((wi,ei) as wei)::wes2rest when wi > 0 ->
            begin match cancel_div_floor_prod wes1 ei with
            | None -> add_to_snd wei (aux wes1 wes2rest) (* no occurence of e in numerator *)
            | Some wes1' ->
                if wi = 1
                  then aux wes1' wes2rest  (* remove from list of divisors *)
                  else aux wes1' (((wi-1),ei)::wes2rest) (* decrement exponent *)
                  (* LATER: optimize by calling simplify_numerator only once, passing the power [wn]
                    to that function *)
            end
       | wei::wes2rest -> (* not a cancellable item *)
           add_to_snd wei (aux wes1 wes2rest)
       in
     let wes1',wes2' = aux wes1 wes2 in
     let e1' = expr_make_like e1 (Expr_prod wes1') in
     let e2' = expr_make_like e2 (Expr_prod wes2') in
     normalize_one (mk (Expr_binop (Binop_div, e1', e2')))
  (* simplify [(a*b)/a] to [b] and [(a^k*b)/a] to [a^(k-1)*b]. *)
  | Expr_binop (Binop_div, { expr_desc = Expr_prod wes; _ }, e) -> (* when [e] is not an [Expr_prod] *)
      begin match cancel_div_floor_prod wes e with
      | None -> e
      | Some wes' -> normalize_one (mk (Expr_prod wes'))
      end
  | _ -> e

(* [gather_common recurse_bool e]: apply [gather_one] in a full expression
    if recurse is set to true *)
let gather_common (recurse : bool) (e : expr) : expr =
  apply_bottom_up_if recurse cleanup_true gather_one e

(* [gather] and [gather_rec] can be passed as arguments to [Arith.simpl] *)
let gather = gather_common false
let gather_rec = gather_common true


(******************************************************************************)
(*                          Expand                                 *)
(******************************************************************************)

(* [expand_one e]: expands a sum that appears inside a product.
    For example, [e1 * (e2 + e3)] becomes [e1 * e2 + e1 * e3].
    It can also expand e.g. [(e1 + e2) * (e3 + e4) * (e5 + e6)].
    The function performs nothing if no expansion can be performed.
    At the very end, it applies [normalize] to the result.*)
let expand_one (e : expr) : expr =
  (* [aux (w,e) [e1; e2]]
     - if [w=1] and [e] is a sum [w1*a1 + w2*a2], then produce
       [w1*a1*e1 + w1*a1*e2 + w2*a2*e1 + w2*a2*e2]
     - otherwise it computes [e^w*e1 + e^w*e2]. *)
  let aux (typ : typ option) ((w,e) : wexpr) (acc : exprs) : exprs =
    (* LATER: check that e.typ does not conflict with typ *)
    match (w, e.expr_desc) with
    | 1, (Expr_sum wes) ->
        List.concat_map (fun (wk,ak) ->
          List.map (fun ei ->
            expr_prod_nonweighted ~typ [(expr_int wk); ak; ei]) acc
        ) wes
    | _ ->
      (* LATER: see if we can use expr_mul instead, and perform simplications later *)
      (* [expr_distrib_we ei] produces [e^w * ei], with on-the-fly simplifications *)
      let expr_distrib_we (ei : expr) : expr =
        let typ = ei.expr_typ in
        match ei.expr_desc with
        | Expr_prod [(_w, { expr_desc = Expr_int 1; _})] -> expr_prod ~typ [(w,e)]
        | Expr_prod wes -> expr_prod ~typ ((w,e) :: wes)
        | _ -> expr_prod ~typ [(w,e); (1,ei)]
        in
      List.map (fun ei -> expr_distrib_we ei) acc
    in
  let res =
    let typ = e.expr_typ in
    match e.expr_desc with
    | Expr_prod wes ->
        let exprs_in_sum = List.fold_right (aux typ) wes [expr_one typ] in
        expr_sum_nonweighted ~typ exprs_in_sum
    | _ -> e
    in
  normalize res

(* [expand_common recurse e]: calls [expand_one] recursively, calling the [gather] operations
    after each step. *)
let expand_common (recurse : bool) (e : expr) : expr =
  let tr (ei : expr) : expr =
    gather_rec (expand_one ei) in
  apply_bottom_up_if recurse cleanup_true tr e

(* [expand] and [expand_rec] can be passed as arguments to [Arith.simpl] *)
let expand = expand_common false
let expand_rec = expand_common true (* Warning: might be quadratic? *)

(******************************************************************************)
(*                          Compute                                 *)
(******************************************************************************)

(* [wexpr_is_numeric (w,e)] returns true if [e] is a constant [Expr_int]
   or [Expr_double]. *)

let wexpr_is_numeric ((w,e):wexpr) : bool =
  match e.expr_desc with
  | Expr_int _ | Expr_double _ -> true
  | _ -> false

(* [wwexpr_is_int (w,e)] returns true if [e] is a constant [Expr_int]. *)
let wexpr_is_int ((w,e):wexpr) : bool =
  match e.expr_desc with
  | Expr_int _ -> true
  | _ -> false

(* [compute_power_int n w] computes [n^w] *)

let rec compute_power_int (n:int) (w:int) : int =
  assert (w >= 0);
  if w = 0 then 1 else n * compute_power_int n (w-1)

(* [compute_power_double f w] computes [f^w] *)

let rec compute_power_double (f:float) (w:int) : float =
  if w < 0 then begin
    let inv = compute_power_double f (-w) in
    if inv = 0. then fail None "compute_power_double: division by zero";
    1.0 /. inv
  end else begin
    if w = 0 then 1.0 else f *. compute_power_double f (w-1)
  end

(* [compute_wexpr_sum wes] assumes all items in [wes] to satisfy [wexpr_is_numeric],
   and it returns a single item [(w,e)] describing the numerical result.
   It is either of the form [(n, Expr_int 1)] in case the result is the integer [n],
   or of the forme [(1, Expr_double f)] in case the result is the float value [f].
   When the sum involves only integers, the result is an integer;
   if, however, the sum involves at least one double, it is a double. *)

let update_typ (mem_t : typ option ref) (new_t : typ option) : unit =
  match new_t with
    | None -> ()
    | Some nt ->
      begin match !mem_t with
      | None -> mem_t := Some nt
      | Some mt ->
        if (mt <> nt) then
          printf "WARNING: arith types differ: %s and %s\n" (AstC_to_c.typ_to_string mt) (AstC_to_c.typ_to_string nt)
      end

let compute_wexpr_sum ~(typ : typ option) ?(loc) (wes:wexprs) : wexpr =
  if List.for_all wexpr_is_int wes then begin
    let n = List.fold_left (fun acc (w,e) ->
      acc + match e.expr_desc with
      | Expr_int n -> (w * n)
      | _ -> assert false
    ) 0 wes in
    (n, expr_int 1)
  end else begin
    let typ = ref typ in
    let f = List.fold_left (fun acc (w,e) ->
      update_typ typ e.expr_typ;
      acc +. match e.expr_desc with
      | Expr_int n -> float_of_int (w * n)
      | Expr_double f -> (float_of_int w) *. f
      | _ -> assert false
    ) 0. wes in
    (1, expr_float ~typ:!typ f)
  end

(* [compute_wexpr_prod wes] is similar to [compute_wexpr_sum], but for products.
   It returns a single item [(w,e)] describing the numerical result.
   It is either of the form [(1, Expr_int n)] in case the result is the integer [n],
   or of the forme [(1, Expr_double f)] in case the result is the float value [f]. *)

let compute_wexpr_prod ~(typ : typ option) ?(loc) (wes:wexprs) : wexpr =
  if List.for_all wexpr_is_int wes then begin
    let wes_pos = List.filter (fun (w,_e) -> w >= 0) wes in
    let wes_neg = List.filter (fun (w,_e) -> w < 0) wes in
    let wes_neg = List.map (fun (w,e) -> (-w,e)) wes_neg in
    let wes_prod (wes_select:wexprs) : int =
      List.fold_left (fun acc (w,e) ->
        acc * match e.expr_desc with
        | Expr_int n -> compute_power_int n w
        | _ -> assert false
      ) 1 wes_select in
    let num = wes_prod wes_pos in
    let denum = wes_prod wes_neg in
    if denum = 0 then fail loc (Printf.sprintf "compute_wexpr_prod: exact integer division by zero: %d / %d" num denum);
    if num mod denum <> 0 then fail loc (Printf.sprintf "compute_wexpr_prod: exact integer division is not exact: %d / %d" num denum);
    let n = num / denum in
    (1, expr_int n)
  end else begin
    let typ = ref typ in
    let f = List.fold_left (fun acc (w,e) ->
      update_typ typ e.expr_typ;
      acc *. match e.expr_desc with
      | Expr_int n -> compute_power_double (float_of_int n) w
      | Expr_double f -> compute_power_double f w
      | _ -> assert false
    ) 1. wes in
    (1, expr_float ~typ:!typ f)
  end

(* [compute_one e]: performs simplification of operations between known constants.
    For example, [4 + a + 3] becomes [a + 7].
    For example, [4.3 + a + 3] becomes [a + 7.3].
    For example, [10 / 2.5 - 3] becomes [1.0].
    The operation [normalize] is called at the end. *)

let compute_one (e : expr) : expr =
  let typ = e.expr_typ in
  let loc = e.expr_loc in
  let mk desc = expr_make_like e desc in
  let res = match e.expr_desc with
  | Expr_sum wes ->
      let wes_num, wes_rest = List.partition wexpr_is_numeric wes in
      if wes_num = [] then e else
      let we_num = compute_wexpr_sum ~typ ?loc wes_num in
      mk (Expr_sum (we_num :: wes_rest))
  | Expr_prod wes ->
      let wes_num, wes_rest = List.partition wexpr_is_numeric wes in
      if wes_num = [] then e else
      let we_num = compute_wexpr_prod ~typ ?loc wes_num in
      mk (Expr_prod (we_num :: wes_rest))
  (* binary operations on integers *)
  | Expr_binop (op, { expr_desc = Expr_int n1; _}, { expr_desc = Expr_int n2; _}) ->
     begin match op with
     | Binop_exact_div ->
        if n2 = 0 then fail loc (Printf.sprintf "compute_one: integer division by zero: exact_div(%d, %d)" n1 n2);
        if (n1 mod n2) <> 0 then fail loc (Printf.sprintf "compute_one: division is not exact: exact_div(%d, %d)" n1 n2);
        expr_int (n1 / n2) (* integer division with rounding *)
     | Binop_div ->
        if n2 = 0 then fail loc (Printf.sprintf "compute_one: integer division by zero: %d / %d" n1 n2);
        expr_int (n1 / n2) (* integer division with rounding *)
     | Binop_mod ->
        if n2 = 0 then fail loc (Printf.sprintf "compute_one: modulo by zero: %d / %d" n1 n2);
        expr_int (n1 mod n2)
     | Binop_shiftl ->
        expr_int (n1 lsl n2)
     | Binop_shiftr ->
        expr_int (n1 lsr n2)
     | Binop_xor ->
        expr_int (n1 lxor n2)
     | Binop_bitwise_and ->
        expr_int (n1 land n2)
     | Binop_bitwise_or ->
        expr_int (n1 lor n2)
     | _ -> e
    end
  | _ -> e (* LATER: don't use a catch-all branch, as it is error prone in case we add constructors *)
  in
  normalize res


(* [compute] can be passed as arguments to [Arith.simpl] *)
let compute (e:expr) : expr =
  apply_bottom_up_if recurse_true cleanup_true compute_one e


(******************************************************************************)
(*                          Transformation on terms                           *)
(******************************************************************************)


(* [map_on_arith_nodes tr t]: applies arithmetic simplification [tr] in depth of [t]*)
let rec map_on_arith_nodes (tr : trm -> trm) (t : trm) : trm =
  if has_mark_nosimpl t
    then t
    else if is_prim_arith_call t then tr t
    else
      trm_map (map_on_arith_nodes tr) t

(* DEBUG let c = ref 0 *)
let simplify_at_node (f_atom : trm -> trm) (f : expr -> expr) (t : trm) : trm =
  try (
  let expr, atoms = trm_to_expr t in

  let atoms2 =
    (* If we could not extract any structure, then we don't process recursively the atoms
       using [f_atom], else we would trigger an infinite loop when [indepth=true]. *)
    match expr.expr_desc with
    | Expr_atom _id -> atoms
    | _ -> Atom_map.map f_atom atoms
    in
  let expr2 = f expr in
  if debug then Printf.printf "Expr after transformation: %s\n" (expr_to_string atoms expr2);
  (* let expr3 = normalize expr2 in
  if debug then Printf.printf "Expr after normalization: %s\n" (expr_to_string atoms expr3); *)
  expr_to_trm atoms2 expr2
  )
  with e -> Printf.printf "Arith.simplify_at_node: error on processing at loc %s\n" (loc_to_string t.loc); raise e

(* [simplify_aux indepth f t]: converts node [t] to an expression, then applies the
     simplifier [f], then it converts it back to a trm
    params:
      [f]: simplifier function
      [t]: the node on which the simplifications should be performed
    return:
      update t with the simplified expressions
  LATER: should [simplify_aux false f t] fail if [t] is not an application of prim_arith? *)
let rec simplify_aux (indepth : bool) (f : expr -> expr) (t : trm) : trm =
  if not indepth
    then begin
       let f_atom_identity = (fun ti -> ti) in
       simplify_at_node f_atom_identity f t
      end
    else begin
     let f_atom_simplify = simplify_aux indepth f in
     map_on_arith_nodes (simplify_at_node f_atom_simplify f) t end

(* [simplify indepth f t p]: applies [simplify_aux] at the trm with path [p] *)
let simplify (indepth : bool) (f : expr -> expr) : Transfo.local =
  apply_on_path (simplify_aux indepth f)



