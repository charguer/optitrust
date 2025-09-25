open PPrint
open Ast
open Trm
open Typ
open Contextualized_error

(* debug flags *)
let debug = false
let debug_rec = false

(* [debug_without_inlined_atoms] controls the behavior of [expr_to_string] *)
let debug_without_inlined_atoms = ref false

(* mark used for marking trms that should be skipped by the simplifier *)
let mark_nosimpl = "__arith_core_nosimpl"

(** [has_mark_nosimplf t]: check if [t] should be skipped by the simplifier or not.*)
let has_mark_nosimpl (t : trm) : bool =
  Mark.trm_has_mark mark_nosimpl t
  (* LATER Ast.trm_has_mark *)

(******************************************************************************)
(*                          Types                                        *)
(******************************************************************************)

(* id type *)
type id = int

(* generate a new id *)
let next_id : unit -> id = Tools.fresh_generator ()

type loc = location (* alias *)

(** [expr]: expression type, it may be a literal expression, an atom expression or an arithmetic expression *)
type expr = {
  expr_desc : expr_desc;
  expr_typ : typ_builtin option; (* only Expr_atom may have an expr_typ not set. TODO: Remove the option when typing reliably give a type to all atoms *)
  expr_loc : loc; }

(*  Grammar of expressions.
  Atoms : uninterpreted expressions, with an indication of whether it is deletable
    and redundant. This "purity" information is stored both in the atom map and in
    the atom occurrences. This redundance is on purpose, to be able to display the
    contents of the atom map with the purity information, and to be able to obtain
    purity information about the atoms without an expensive lookup in the atom map.
  Int, Float : base types
  Expr_sum  : w1 * e1 + ... + wN * eN
  Expr_prod : e1 ^ w1 * ... * eN ^ wN
  Supported arithmetic binary operators on int :
  | Binop_div           (* a / b *) -> div_floor : a/b  on int
  | Binop_mod           (* a % b *)
  | Binop_shiftl        (* a >> k*)
  | Binop_shiftr        (* a << k *)
  | Binop_xor           (* a ^ b *)
  | Binop_bitwise_and   (* a & b *)
  | Binop_bitwise_or    (* a | b *)
  --LATER: add fmod, not clear whether it should be a function or a binop in ast.ml;
  Expr_cast is probably missing as well
*)
and expr_desc =
  | Expr_int of int
  | Expr_float of float
  | Expr_atom of id * purity
  | Expr_sum of wexprs
  | Expr_prod of wexprs
  | Expr_binop of binary_op * expr * expr

(** [purity]:
    redundant means that [e+e] is equivalent to [2*e].
    deletable means that [0*e] can be replaced by [0]. *)
and purity = {
  redundant : bool;
  deletable : bool; }

(** [exprs]: list of expressions *)
and exprs = expr list

(** [wexprs]: weighted list of expressions *)
and wexprs = wexpr list

(** [wexpr]: weighted expression *)
and wexpr = (int * expr)

(** [Atom_map]: a map from atom ids to the corresponding terms *)
module Atom_map = Map.Make(Int)

(** [atom_map]: atom map for storing atoms together with their purity information *)
type atom_map = (trm * purity) Atom_map.t

(** [no_atoms]: empty atom map *)
let no_atoms = Atom_map.empty

(** [arith_transfo] is a transformation on atoms and expressions *)
type arith_transfo = atom_map * expr -> atom_map * expr

(* LATER: [to_list] seems to be available in recent versions of ocaml stdlib Map. *)
let atom_map_to_list (atom_map : atom_map) : (id * (trm * purity)) list =
  Atom_map.fold (fun id trm_purity acc -> (id, trm_purity) :: acc) atom_map []


(******************************************************************************)
(*                          Smart constructors                                *)
(******************************************************************************)

let is_integer_typ (typ : typ_builtin) : bool =
  match typ with
  | Typ_float _ | Typ_bool | Typ_char -> false
  | Typ_int _ | Typ_size _ | Typ_fixed_int _ -> true

let unsupported_binop (op : binary_op) =
  let s = Tools.document_to_string (Ast_to_text.print_binop op) in
  failwith "Arith_core: unsupported binop: %s" s

let expr_make ?(loc : loc) ?(typ : typ_builtin option) (desc : expr_desc) : expr =
  { expr_desc = desc;
    expr_typ = typ;
    expr_loc = loc; }

let expr_make_like (e : expr) (desc : expr_desc) : expr =
  { e with expr_desc = desc }

(** [expr_int n] produces the integer value [n] *)
let expr_int ?(loc : loc) ~(typ: typ_builtin) (n : int) : expr =
  expr_make ?loc ~typ (Expr_int n)

(** [expr_float f] produces the float value [n] *)
let expr_float ?(loc : loc) ~(typ : typ_builtin) (f : float) : expr =
  expr_make ?loc ~typ (Expr_float f)

(** [expr_one typ] produces either [expr_int 1] or [expr_float 1.0] depending on the type *)
let expr_one ?loc (typ : typ_builtin) : expr =
  match typ with
  | Typ_float _ -> expr_float ~typ 1.0
  | Typ_int _ | Typ_size _ | Typ_fixed_int _ -> expr_int ~typ 1
  | _ -> failwith "Arith does not support types bool and char"

(** [expr_atom id] produces a variable [id], denoting an arbitrary subterm form ast.ml *)
let expr_atom ?(loc : loc) ?(typ : typ_builtin option) (id : id) (purity : purity) : expr =
  expr_make ?loc ?typ (Expr_atom (id, purity))

(** [expr_sum [(w1,e1);(w2,e2)]] produces [w1*e1 + w2*e2] *)
let expr_sum ?(loc : loc) ~(typ : typ_builtin) (wes : wexprs) : expr =
  expr_make ?loc ~typ (Expr_sum wes)

(** [expr_sum_nonweighted es] produces [e1 + e2 + ... + en] *)
let expr_sum_nonweighted ?(loc : loc) ~(typ : typ_builtin) (es : exprs) : expr =
   expr_sum ?loc ~typ (List.map (fun e -> (1,e)) es)

(** [expr_neg e1] produces [-e1] *)
let expr_neg ?(loc : loc) ~(typ : typ_builtin) (e1 : expr) : expr =
  expr_sum ?loc ~typ [(-1,e1)]

(** [expr_add e1 e2] produces [e1 + e2] *)
let expr_add ?(loc : loc) ~(typ : typ_builtin) (e1 : expr) (e2 : expr) : expr =
  expr_sum ?loc ~typ [(1,e1); (1,e2)]

(** [expr_sub e1 e2] produces [e1 - e2] *)
let expr_sub ?(loc : loc) ~(typ : typ_builtin) (e1 : expr) (e2 : expr) : expr =
  expr_sum ?loc ~typ [(1,e1); (-1,e2)]

(** [expr_prod [(w1,e1);(w2,e2)]] produces [e1^w1 * e2^w2] *)
let expr_prod ?(loc : loc) ~(typ : typ_builtin) (wes : wexprs) : expr =
  expr_make ?loc ~typ (Expr_prod wes)

(** [expr_prod_nonweighted es] produces [e1 * e2 * ... * en] *)
let expr_prod_nonweighted ?(loc : loc) ~(typ : typ_builtin) (es : exprs) : expr =
   expr_prod ?loc ~typ (List.map (fun e -> (1,e)) es)

(** [expr_pow e1 w] produces [e1 ^ w] *)
let expr_pow ?(loc : loc) ~(typ : typ_builtin) (e1 : expr) (w : int) : expr =
  expr_prod ?loc ~typ [(w,e1)]

(** [expr_mul e1 e2] produces [e1 * e2] *)
let expr_mul ?(loc : loc) ~(typ : typ_builtin) (e1 : expr) (e2 : expr) : expr =
  expr_prod ?loc ~typ [(1,e1); (1,e2)]

(** [expr_div e1 e2] produces [e1 / e2];
   if arguments are integers, then [e1] is assumed to be divisible by [e2] *)
let expr_div ?(loc : loc) ~(typ : typ_builtin) (e1 : expr) (e2 : expr) : expr =
  expr_prod ?loc ~typ [(1,e1); (-1,e2)]

(** [expr_binop op e1 e2] produces the operation [op e1 e2] *)
let expr_binop ?(loc : loc) ~(typ : typ_builtin) (op : binary_op) (e1 : expr) (e2 : expr) : expr =
  expr_make ?loc ~typ (Expr_binop (op, e1, e2))

(* LATER: might add constructors for other binary_ops *)

let expr_typ (e: expr) : typ_builtin =
  Option.unsome_or_else e.expr_typ (fun () -> failwith "Expression has an unset type")

(******************************************************************************)
(*                          Purity queries                                 *)
(******************************************************************************)

(** [for_all_atoms f e]  returns whether [f] evaluates to [true] on every [atoms] in [e] *)
let for_all_atoms (f : id -> purity -> bool) (e : expr) : bool =
  let res = ref true in
  let rec aux (e : expr) : unit =
    match e.expr_desc with
    | Expr_int _ | Expr_float _ -> ()
    | Expr_sum wes | Expr_prod wes -> List.iter (fun (_ai,ei) -> aux ei) wes
    | Expr_binop (_op, e1, e2) -> aux e1; aux e2
    | Expr_atom (id, purity) -> if not (f id purity) then res := false
    in
  aux e;
  !res

(** [is_deletable e]: indicates whether [e] is an expression that contains only
    deletable atoms. *)
let is_deletable (e : expr) : bool =
  for_all_atoms (fun _id purity -> purity.deletable) e

(** [is_redundant e]: indicates whether [e] is an expression that contains only
    redundant atoms. *)
let is_redundant (e : expr) : bool =
  for_all_atoms (fun _id purity -> purity.redundant) e


(** Exception [Unexpected_non_redundant] is used to cancelled computations when
    discovering a non-redundant expression *)
exception Unexpected_non_redundant

(** [check_redundant e] raises the exception [Unexpected_non_redundant] if [e]
    is not redundant. *)
let check_redundant (e : expr) : unit =
  if not (is_redundant e)
    then raise Unexpected_non_redundant

(** Exception [Unexpected_non_deletable] is used to cancelled computations when
    discovering a non-deletable expression *)
exception Unexpected_non_deletable

(** [check_deletable e] raises the exception [Unexpected_non_deletable] if [e]
    is not deletable. *)
let check_deletable (e : expr) : unit =
  if not (is_deletable e)
    then raise Unexpected_non_deletable


(******************************************************************************)
(*                          Normalize                                 *)
(******************************************************************************)

(** [normalize_one e]:
   - collapses nested sums onto a single sum, and likewise for nested products
   - turns a product of an expression with a constant integer as a weighted
     expression in the parent sum
   - eliminates products and sums with a single expression of weight one
   - eliminates products and sums with an empty list
   - eliminates deletable elements with weight zero
   - eliminates +0 is sums and *1 in products
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
          | (_ai, { expr_desc = Expr_float 0.; _ }) -> []
          | (1, { expr_desc = Expr_int n; _ }) -> [(n, expr_int ~typ:(expr_typ e) 1)]
          | (-1, { expr_desc = Expr_int n; _ }) -> [(-n, expr_int ~typ:(expr_typ e) 1)]
          | (0, ei) when is_deletable ei -> []
          | (1, { expr_desc = Expr_sum wesi; _ }) -> wesi
          | (-1, { expr_desc = Expr_sum wesi; _ }) -> List.map (fun (ai, ei) -> (-ai,ei)) wesi
          | (ai, { expr_desc = Expr_prod [(1, { expr_desc = Expr_int bi; _}); (1,ei)]; _ }) (* Optional? *)
          | (ai, { expr_desc = Expr_prod [(1,ei); (1, { expr_desc = Expr_int bi; _})]; _ }) (* Optional? *)
          | (ai, { expr_desc = Expr_prod [(bi, { expr_desc = Expr_int 1; _}); (1,ei)]; _ })
          | (ai, { expr_desc = Expr_prod [(1,ei); (bi, { expr_desc = Expr_int 1; _})]; _ }) -> [(ai * bi, ei)]
          | we -> [we]) wes))
    | Expr_prod wes ->
        let typ = expr_typ e in
        (* simplify integer coefficients, and keep track of whether a zero is found *)
        let found_zero = ref None in
        let wes2 = List.concat_map (function
          | (_ai, ({ expr_desc = Expr_int 0; _ } as ezero)) ->
            found_zero := Some ezero;
            []
          | (_ai, ({ expr_desc = Expr_float 0.; _} as ezero)) ->
            found_zero := Some ezero;
            []
          | (_ai, { expr_desc = Expr_int 1; _ }) -> []
          | (_ai, { expr_desc = Expr_float 1.; _}) -> []
          | (0, ei) when is_deletable ei -> []
          | (1, { expr_desc = Expr_prod wesi; _}) -> wesi
          | (-1, { expr_desc = Expr_prod wesi; _}) -> List.map (fun (w,ei) -> (-w, ei)) wesi
          | (ai, { expr_desc = Expr_sum [(bi, { expr_desc = Expr_int 1; _})]; expr_loc = loc; _}) -> [(ai, expr_int ?loc ~typ bi)]
          | (ai, { expr_desc = Expr_sum [(bi, ei)]; expr_loc = loc; _}) when is_integer_typ typ ->
            [(ai, expr_int ?loc ~typ bi); (ai, ei)]
          | we -> [we]) wes in
        (* if a zero is found and all expressions are deletable, then the result is zero;
           otherwise we keep one zero in the product, and keep all non-deletable expressions *)
        begin match !found_zero with
        | None -> mk (Expr_prod wes2)
        | Some zero_expr ->
            let wes_nondeletable = List.filter (fun (ai,ei) -> not (is_deletable ei)) wes2 in
            if wes_nondeletable = []
              then zero_expr
              else mk (Expr_prod ((1,zero_expr)::wes_nondeletable))
        end
    (* [e1 / 1 = 1] *)
    | Expr_binop (Binop_trunc_div, e1, { expr_desc = Expr_int 1; _}) -> e1
    | Expr_binop (Binop_trunc_div, e1, { expr_desc = Expr_prod []; _}) -> e1
    (* [0 mod e2 = 0] *)
    | Expr_binop (Binop_trunc_mod, ({ expr_desc = Expr_int 0; _} as ezero), e2)
      when is_deletable e2 ->
        ezero
    (* [e1 % 1 = 0] *)
    | Expr_binop (Binop_trunc_mod, e1, { expr_desc = Expr_int 1; _ })
      when is_deletable e1 ->
        expr_int ~typ:(expr_typ e) 0
    (* [e1 << 0 = e1] and [e1 >> 0 = e1] *)
    | Expr_binop ((Binop_shiftr | Binop_shiftl), e1, { expr_desc = Expr_int 0; _}) -> e1
    | _ -> e
    in
  match e.expr_desc with
  | Expr_sum [] -> expr_int ~typ:(expr_typ e) 0
  | Expr_prod [] -> expr_int ~typ:(expr_typ e) 1
  | Expr_sum [(1,e1)] -> e1
  | Expr_prod [(1,e1)] -> e1
  | _ -> e

(** [normalize e]: applies [normalize_one] in a bottom up fashion.
   Its definition appears further below. *)


(******************************************************************************)
(*                          From expr to string                                 *)
(******************************************************************************)

(** [is_one e]: checks if e == 1 *)
let is_one (e : expr) : bool =
  match e.expr_desc with
  | Expr_int 1 | Expr_float 1.0 -> true
  | _ -> false

(** [parens_if_neg n d]: if [n] is negative then it add parentheses around [d] *)
let parens_if_neg (n:int) (d:document) : document =
  if n < 0 then parens d else d

(** [id_to_string id] gives a human readable name for an atom id.
    To identify atoms, we use letters 'a', 'b', ... then
    'aa', 'ab', etc. In the very hypothethetical case where we'd need
    for than 256 distinct atoms in a single expression, we'd continue
    with numbers, e.g. 'a676', 'a677', etc... *)
let id_to_string (id : id) : string =
  let letter_of_small_int (i:int) : string =
    let c = Char.chr ((Char.code 'a') + i) in
    String.make 1 c in
  if id < 26 then begin
    letter_of_small_int id
  end else if id < 26*26 then begin
    let s1 = letter_of_small_int (id / 26) in
    let s2 = letter_of_small_int (id mod 26) in
    s1 ^ s2
  end else
    "a" ^ string_of_int id

(** [purity_to_string purity] gives a short string to display purity
    information: "^NR" or "^ND" or "^NR^ND" or "", to indicate non-redundant
    and non-deletable properties. *)
let purity_to_string (purity : purity) : string =
    (* if not purity.redundant && not purity.deletable then string "^NDR" else *)
     (if purity.redundant then "" else "^NR")
   ^ (if purity.deletable then "" else "^ND")

(** [expr_to_string ~inline_atoms ~print_atom_map atoms e]: convert an expression to a string.
    The flag [inline_atoms] controls whether the subterms associated with atoms
    should be printed next to atom identifiers, or should be printed in a separate
    atom map. Default is true.
    The flat [print_atom_map] controls whether to print the table of atoms after
    the expression. Default is true.
    Default values can be swapped by setting the global flag [debug_without_inlined_atoms]. *)
let expr_to_string
  ?(inline_atoms : bool = not !debug_without_inlined_atoms)
  ?(print_atom_map : bool = !debug_without_inlined_atoms)
  (atoms : atom_map) (e : expr) : string =
  let rec aux (e : expr) : document =
    let auxw ((w,e) : wexpr) : document =
      parens ((string (string_of_int w)) ^^ comma ^^ aux e) in
    let auxwes (we : wexprs) : document =
      Tools.list_to_doc ~bounds:[lbrace; rbrace] (List.map auxw we) in
    match e.expr_desc with
    | Expr_int n -> string (string_of_int n)
    | Expr_float n -> string (string_of_float n)
    | Expr_sum wes -> string "Sum" ^^ (auxwes wes)
    | Expr_prod wes -> string "Prod" ^^ (auxwes wes)
    | Expr_binop (op, e1, e2) ->
        let sop = match op with
          | Binop_trunc_div -> "TruncDiv"
          | Binop_trunc_mod -> "TruncMod"
          | Binop_exact_div -> "ExactDiv"
          | Binop_shiftl -> "ShiftL"
          | Binop_shiftr -> "ShiftR"
          | Binop_xor -> "Xor"
          | Binop_bitwise_and -> "BitwiseAnd"
          | Binop_bitwise_or -> "BitwiseOr"
          | _ -> unsupported_binop op
          in
        string sop ^^ string "(" ^^ aux e1 ^^ string "," ^^ aux e2 ^^ string ")"
    | Expr_atom (id, purity) ->
        (* format is e.g.:
            "{c}" for an atom with id 3,
            "{c: t[i]}" for an atom with id 3 and associated subterm t[i].
            "{c^NR}" if atom is non-redundant and atom map is not printed separately.
            *)
        let d_name = string (id_to_string id) in
        let d_purity =
          if print_atom_map then empty else string (purity_to_string purity) in
        let d_trm =
          if not inline_atoms then empty else begin
            let style = Ast_to_c.default_style() in
            match Atom_map.find_opt id atoms with
            | Some (t1,_purity) ->
                (* Deprecated code that was used for printing variables without braces.
                    Might be useful in the future.
                begin match t1.desc with
                | Trm_var x -> Ast_to_c.var_to_doc style x
                | Trm_apps ({desc = Trm_prim (_, Prim_unop Unop_get); _},
                    [{desc = Trm_var x; _}], _) -> Ast_to_c.var_to_doc style x
                | _ -> Ast_to_c.trm_to_doc style t1
                end *)
                string ":" ^^ blank 1 ^^ (Ast_to_c.trm_to_doc style t1)
            | _  ->
                Tools.warn "expr_to_string: atom occurrence without entry in atom map---broken invariant.";
                string ": <missing-entry-in-atom-map>"
          end in
        braces (d_name ^^ d_purity ^^ d_trm)
  in
  let d_atoms =
    (* The atom map takes the form "{{a: trma; b: trmb}}".
       Each atom may be decorated with purity information, e.g., "c^NR: t[i]". *)
    if not print_atom_map then empty else begin
      let style = Ast_to_c.default_style() in
      let atom_to_doc (id,(ti,purity)) =
           string (id_to_string id)
        ^^ string (purity_to_string purity)
        ^^ string ":"
        ^^ blank 1
        ^^ Ast_to_c.trm_to_doc style ti
        in
      let d_atoms_items = List.map atom_to_doc (atom_map_to_list atoms) in
         blank 1 ^^ string "where" ^^ blank 1
      ^^ (Tools.list_to_doc ~sep:(semi ^^ blank 1) ~bounds:[string "{{"; string "}}"] d_atoms_items)
    end in
  Tools.document_to_string (aux e ^^ d_atoms)

(** [expr_to_math_string atoms e]: converts an expression to a string, using mathematical notations *)
let expr_to_math_string (atoms : atom_map) (e : expr) : string =
  let power_to_doc (base : document) (power : int) : document =
     base ^^ string "^" ^^ string (string_of_int power)
     in
  let rec aux (e : expr) : document =
    match e.expr_desc with
    | Expr_int n -> string (string_of_int n)
    | Expr_float n -> string (string_of_float n)
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
          | Binop_trunc_div -> "/"
          | Binop_shiftr -> ">>"
          | Binop_shiftl -> "<<"
          | Binop_xor -> "^" (* LATER: use other symbol to avoid confusion *)
          | Binop_bitwise_and -> "&" (* LATER: use other symbol to avoid confusion *)
          | Binop_bitwise_or -> "|" (* LATER: use other symbol to avoid confusion *)
          | _ -> unsupported_binop op
          in
        parens (aux e1) ^^ string sop ^^ parens (aux e2)
     | Expr_atom (id,_purity) ->
        begin match Atom_map.find_opt id atoms with
        | Some (t1,_purity) -> Ast_to_c.(trm_to_doc (default_style())) t1
        | _  -> failwith "Arith_core.expr_to_math_string: couldn't convert
                          an atom expr to a trm"
        end
  in
  Tools.document_to_string (aux e)


(******************************************************************************)
(*                          Traversals                                        *)
(******************************************************************************)

(** [identity] transformation *)
let identity (e : expr) : expr =
  e

(** [apply_bottom_up]: is a combinator that takes a transformation and applies it recursively,
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

(** [normalize e]: applies [normalize_one] in a bottom up fashion *)
let normalize (e : expr) : expr =
  apply_bottom_up normalize_one e

(** [cleanup_true]: perform cleanup after transformation; else [cleanup_false] *)
let cleanup_true = true
let cleanup_false = false

(** [recurse_true]: apply transformation in depth; else [recurse_false] *)
let recurse_true = true
let recurse_false = false

(** [apply_bottom_up_if]: is a combinator for either applying a transformation recursively
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
      then Tools.debug "Step:\n\t%s\n\t%s\n\t%s" (expr_to_string no_atoms e)
        (expr_to_string no_atoms e2) (expr_to_string no_atoms e3);
    e3 in
  if recurse
    then apply_bottom_up f_with_cleanup e
    else f_with_cleanup e

(** [apply_bottom_up_debug e]: function used only for debugging purposes *)
let apply_bottom_up_debug (e : expr) : expr =
  let f ei =
    if debug then Tools.debug "Bottom-up %s" (expr_to_string no_atoms ei);
    ei in
  apply_bottom_up f e

(** [apply_bottom_up_if_debug]: function used only for debugging purposes *)
let apply_bottom_up_if_debug (recurse : bool) (cleanup : bool) (e : expr) : expr =
  let f ei =
    let ej = (if cleanup then normalize_one else identity) ei in
    if debug
      then Tools.debug "Bottom-up-if:\n\t%s\n\t%s"
            (expr_to_string no_atoms ei) (expr_to_string no_atoms ej); ej
    in
  apply_bottom_up_if recurse cleanup f e


(******************************************************************************)
(*                          From trm to expr                                 *)
(******************************************************************************)

(* DEPRECATED
(** [is_syntactically_ro t] approximates whether [t] should be
    considered redundant and deletable when resource information is missing *)
let is_syntactically_ro (t : trm) : bool =
  if !Flags.check_validity then Resources.trm_is_pure t else
    Pattern.pattern_match t [
      Pattern.(trm_var __) (fun () -> true);
      Pattern.(trm_int __) (fun () -> true);
      Pattern.(trm_float __) (fun () -> true);
      (* TODO: add get operations *)
      Pattern.__ (fun () -> false)
      (* TODO: inside contracts everything should be pure hence ro *)
    ]
  (* LATER: use an extended version of trm_is_pure that also allows
     get operations *)
*)

(** [get_purity t] computes whether [t]
    should be considered redundant and deletable *)
let get_purity (t : trm) : purity =
  (* Try syntactic criteria first
     LATER: if resources are available, we may want to try resource-based
            criteria first, because it does not recurse.
     LATER: optimize trm_is_pure to avoid allocation,
     LATER: extend trm_is_pure with an option to allow read operations. *)
  if Resources.trm_is_pure t then begin
    { redundant = true;
      deletable = true }
  end else begin
    let noinfo () = { redundant = false; deletable = false } in
    if not !Flags.check_validity then begin
      (* Second, if resources are never computed, don't try to read resources *)
      noinfo()
    end else begin
      try
        (* Else, try resource-based criteria *)
        let redundant = Resources.is_not_self_interfering t in
        let deletable = Resources.is_deletable t in
        { redundant; deletable }
      with _ ->
       (* If resources are not available,
          and syntactic criteria did fail, then assume nothing *)
        noinfo()
    end
  end


(** [create_atom_for_trm atoms t] creates a fresh id for the atom [t] and register
    it in the table [atoms]. *)
let create_atom_for_trm (atoms : atom_map ref) (purity : purity) (t : trm) : id =
  let new_id = next_id() in
  atoms := Atom_map.add new_id (t,purity) !atoms;
  new_id

(** [create_or_reuse_atom_for_trm atoms t]
   Try to find an existing id that was already bound to the term [t] in the table [atoms],
   else create a fresh id and extends the table [atoms] *)
let create_or_reuse_atom_for_trm (atoms : atom_map ref) (purity : purity) (t : trm) : id =
  (* A nonzero value for [occ] indicates an existing [id] has been found for [t]. *)
  let no_id = -1 in
  let id_found = ref no_id in
  Atom_map.iter (fun id (tid,_purity) ->
    if !id_found = no_id && Trm_unify.are_same_trm t tid then id_found := id) !atoms;
      (* LATER: ideally we should check that the purity computed for tid is the same as that of t *)
  (* If no existing id was found, create a new id, and extends the [atoms] table. *)
  if !id_found = no_id
    then create_atom_for_trm atoms purity t
    else !id_found

(** [trm_to_naive_expr]: conversion of a trm from the AST into an expr, plus a map that for each atom gives the corresponding term. If a term is not 'redundant' then multiple occurrences of that term are not collapsed onto a same atom identifier. *)
let trm_to_naive_expr (t : trm) : expr * atom_map =
  let atoms = ref Atom_map.empty in
  let rec aux (t : trm) : expr =
    let loc = t.loc in
    let force_atom () =
      let purity = get_purity t in
      let id =
        if not purity.redundant
          then create_atom_for_trm atoms purity t
          else create_or_reuse_atom_for_trm atoms purity t in
      let typ = Option.bind t.typ typ_builtin_inv in
      expr_atom ?loc ?typ id purity
    in
    let to_expr_typ (typ : typ) =
      match typ_builtin_inv typ with
      | Some btyp -> btyp
      | None -> failwith "trm_to_naive_expr: expression has a type incompatible with arith simplifications: %s" (Ast_to_text.ast_to_string typ)
    in
    if has_mark_nosimpl t then force_atom () else
    match t.desc with
     (* Recognize constants *)
     | Trm_lit (Lit_int (typ, n)) -> expr_int ?loc ~typ:(to_expr_typ typ) n
     | Trm_lit (Lit_float (typ, n)) -> expr_float ?loc ~typ:(to_expr_typ typ) n
     (* Recognize unary operators *)
     | Trm_apps (f, [t1], _, _) ->
       begin match trm_prim_inv f with
        | Some (typ, Prim_unop Unop_minus) -> expr_neg ?loc ~typ:(to_expr_typ typ) (aux t1)
        | _ -> force_atom ()
       end
     (* Recognize binary operators *)
     | Trm_apps (f, [t1; t2], _, _) ->
       begin match trm_prim_inv f with
        | Some (typ, Prim_binop op) ->
          let typ = to_expr_typ typ in
          begin match op with
          | Binop_add -> expr_add ?loc ~typ (aux t1) (aux t2)
          | Binop_sub -> expr_sub ?loc ~typ (aux t1) (aux t2)
          | Binop_mul -> expr_mul ?loc ~typ (aux t1) (aux t2)
          | Binop_exact_div -> expr_div ?loc ~typ (aux t1) (aux t2)
          | Binop_trunc_div | Binop_trunc_mod | Binop_shiftl | Binop_shiftr | Binop_xor | Binop_bitwise_and | Binop_bitwise_or ->
              expr_binop op ?loc ~typ (aux t1) (aux t2)
          | _ -> force_atom ()
          end
        | _ -> force_atom ()
       end
     | _ -> force_atom ()
     in
    let res = aux t in
    res, !atoms

(** [trm_to_expr t]: convert trm [t] to an expression*)
let trm_to_expr ?(normalized=true) (t : trm) : expr * atom_map =
  let expr, atoms = trm_to_naive_expr t in
  if debug && false
    then Tools.debug "Expr before conversion: %s" (Ast_to_text.ast_to_string t);
  if debug
    then Tools.debug "Expr after conversion: %s" (expr_to_string atoms expr);
  let res = if normalized then normalize expr else expr in
  if debug
    then Tools.debug "Expr after normalization: %s" (expr_to_string atoms res);
  res, atoms

(******************************************************************************)
(*                          From expr to trm                                 *)
(******************************************************************************)

(** [expr_to_trm atoms e]: converts expr [e] to trm  *)
let expr_to_trm (atoms : atom_map) (e : expr) : trm =
  let rec aux (e : expr) : trm =
    let loc = e.expr_loc in
    let expr_typ e =
      (* Delayed because atom may not have a type set *)
      Option.unsome ~error:"Missing type on Arith.expr" e.expr_typ
    in
    match e.expr_desc with
    | Expr_int n -> trm_int ~typ:(typ_builtin (expr_typ e)) ?loc n
    | Expr_float n -> trm_float ~typ:(typ_builtin (expr_typ e)) ?loc n
    | Expr_sum we ->
      let expr_typ = expr_typ e in
      let typ = typ_builtin expr_typ in
      if we = [] then failwith "expr_to_trm: assumes a normalized term";
      List.fold_lefti (fun i acc (w, e) ->
        let (w, e) = match (w,e) with
          | (n,{expr_desc = Expr_int 1; _}) ->
            if n >= 0 then (1, expr_int ~typ:expr_typ n) else (-1, expr_int ~typ:expr_typ (-n))
          | _ -> (w,e) in
        let e_trm = aux e in
        let abs_w = abs w in
        let abs_t =
          if abs_w = 1 then e_trm else trm_mul ?loc ~typ (trm_int ~typ abs_w) e_trm
        in
        if i = 0 then begin
          if w >= 0 then abs_t else trm_minus ?loc ~typ abs_t
        end else begin
          if w >= 0 then trm_add ?loc ~typ acc abs_t else trm_sub ?loc ~typ acc abs_t
        end
      ) (trm_unit ()) we
    | Expr_prod wes ->
      (* We have [e1^w1 * e2^w2 * e3^w3].
         We display [e1^w1 * e2^(-w2) * e3^w3 * e4^(-w4)] as [(e1^w1 * e3^w3) / (e2^w2 * e4^w3)],
         Note: the form [e1^0] is supposed to have been eliminated during normalization.
         We display [e1^1] as just [e1].
         We display [e1^3] as [e1 * e1 * e1] -- LATER: use a pow() function in the optitrust library.
         that is, we use at most one division symbol *)

      let expr_typ = expr_typ e in
      let typ = typ_builtin expr_typ in
      (* combinators *)
      let trm_one () =
        if is_integer_typ expr_typ then trm_int ~typ 1 else trm_float ~typ 1.0 in
      let trm_div (t1:trm) (t2:trm) : trm =
        trm_exact_div ?loc ~typ t1 t2 in
      let rec trm_mul_nonempty (ts : trm list) : trm =
        match ts with
        | [] -> assert false
        | [t1] -> t1
        | t1 :: t2 :: ts' -> trm_mul_nonempty ((trm_mul ?loc ~typ t1 t2)::ts')
        in
      let rec trm_power (t:trm) (n:int) : trm =
        if n = 0
          then failwith "Arith_core.expr_to_trm: assumes a normalized term, so (e,0) should not appear in the argument of [Expr_prod]"
          else if n < 0 then assert false (* DEPRECATED  trm_apps ?loc (trm_binop binop_div) [trm_float 1.0; power t (-n)] *)
          else if n = 1 then t
          else trm_mul ~typ ?loc t (trm_power t (n-1))
        in

      (* separate positive and negative exponents *)
      if wes = [] then failwith "expr_to_trm: assumes a normalized term";
      let wts = List.map (fun (w,e) -> (w, aux e)) wes in
      let wts_pos, wts_neg = List.partition (fun (w,_t) -> w >= 0) wts in
      let ts_pos = List.map (fun (w,t) -> trm_power t w) wts_pos in
      let ts_neg = List.map (fun (w,t) -> trm_power t (-w)) wts_neg in

      begin match ts_pos, ts_neg with
      | [], [] -> failwith "Arith_core.expr_to_trm: assumes a normalized term, so the argument [we] of [Expr_prod] should be nonempty"
      | _, [] -> trm_mul_nonempty ts_pos
      | [], _ -> trm_div (trm_one ()) (trm_mul_nonempty ts_neg)
      | _, _ -> trm_div (trm_mul_nonempty ts_pos) (trm_mul_nonempty ts_neg)
          (* LATER: this case is not expected to happen with exact_div *)
      end

    | Expr_binop (op, e1, e2) ->
      trm_apps ?loc (trm_binop (typ_builtin (expr_typ e)) op) [aux e1; aux e2]

    | Expr_atom (id, _purity) ->
        begin match Atom_map.find_opt id atoms with
        | Some (t1,_purity) -> t1
        | _ -> failwith "Arith_core.expr_to_trm: couldn't convert an atom expr to a trm"
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
    | (Expr_float a, Expr_float b) -> a = b
    | (Expr_atom (a,_puritya), Expr_atom (b,_purityb)) -> a = b
    | (Expr_sum a, Expr_sum b) | (Expr_prod a, Expr_prod b) ->
       List.for_all2 same_wexprs a b
    | (Expr_binop (op1, a1, a2), Expr_binop (op2, b1, b2)) ->
       (op1 = op2) && (same_expr a1 b1) && (same_expr a2 b2)
    | _ -> false
  and same_wexprs ((a_id, a_e) : wexpr) ((b_id, b_e) : wexpr) : bool =
    (a_id = b_id) && (same_expr a_e b_e)
  in
  same_desc a.expr_desc b.expr_desc

(** [cancel_div_floor_prod wes e] simplifies
   [Expr_div (Expr_prod wes) e] into [Expr_prod wes'].
   It returns [Some wes'], or [None] if no simplification is possible *)
let cancel_div_floor_prod (wes : wexprs) (e : expr) : wexprs option =
  if not (is_deletable e) then None else
  let rec aux wes e =
    match wes with
    | [] -> None
    | (wi,ei)::wes' when (same_expr ei e) && wi > 0 ->
        assert (is_deletable ei); (* because same expr as e *)
        if wi = 1
          then Some wes'
          else Some ((wi-1,ei)::wes')
    | (wi,ei)::wes' ->
        begin match aux wes' e with (* LATER: use a monadic notation? *)
        | None -> None
        | Some res -> Some ((wi,ei)::res)
        end in
  aux wes e

(** [gather_one e]: regroups similar expression that appear inside a same product
    or sum. For example, [2 * e1 + (-1)*e1] simplifies to [e1] and
    [e1 * e2 * e1^(-1)] simplifies to [e2].
    Also changes [(a / b) / c] into [a / (b * c)], and simplifies
    [(a1 * c * a2) / (b1 * c * b2)] into  [(a1 * a2) / (b1 * b2)]
    where [c] is a common item to the numerator and divisor (order-insensitive).
    This includes simplifications of [a / a] to [1] and of [(a*b)/a] to [b].
    Correctness: currently, if [e1] has several occurrences, the typechecking
    with resources will force the occurrences to have non-overlapping effects,
    hence the term will necessarily be treated as redundant; hence the present
    function needs no further checks. *)
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
  | Expr_binop (Binop_trunc_div, { expr_desc = Expr_binop (Binop_trunc_div, e1, e2); _ }, e3) ->
      let e23 = normalize_one (mk (Expr_prod [(1, e2); (1, e3)])) in
      gather_one (mk (Expr_binop (Binop_trunc_div, e1, e23))) (* attempt further simplifications *)
  (* simplify [a/a] to [1]. *)
  | Expr_binop (Binop_trunc_div, e1, e2) when e1 = e2 && is_deletable e1 ->
    expr_int ?loc ~typ:(Option.unsome e.expr_typ) 1
  (* simplify [(a*b)/(c*a)] to [b/c] and [(a^k*b)/(c*a)] to [(a^(k-1)*b)/c].
      LATER: check is_deletable is called where needed *)
  | Expr_binop (Binop_trunc_div, ({ expr_desc = Expr_prod wes1; _ } as e1),
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
     normalize_one (mk (Expr_binop (Binop_trunc_div, e1', e2')))
  (* simplify [(a*b)/a] to [b] and [(a^k*b)/a] to [a^(k-1)*b]. *)
  | Expr_binop (Binop_trunc_div, { expr_desc = Expr_prod wes; _ }, e2) -> (* when [e2] is not an [Expr_prod] *)
      begin match cancel_div_floor_prod wes e2 with
      | None -> e
      | Some wes' -> normalize_one (mk (Expr_prod wes'))
      end
  | _ -> e

(** [gather_common recurse_bool e]: apply [gather_one] in a full expression
    if recurse is set to true *)
let gather_common (recurse : bool) (e : expr) : expr =
  apply_bottom_up_if recurse cleanup_true gather_one e

(** [gather] and [gather_rec] can be passed as arguments to [Arith.simpl] *)
let gather = gather_common false
let gather_rec = gather_common true



(******************************************************************************)
(*                          Sort                                              *)
(******************************************************************************)

(** [expr_atom_var_inv atoms e] returns [Some "x"] if [e] corresponds
    to a [Trm_var x] or a [Unop_get] applied to a [Trm_var x]. *)
let expr_atom_var_inv (atoms : atom_map) (e:expr) : var option =
  match e.expr_desc with
  | Expr_atom (id, _purity) ->
      begin match Atom_map.find_opt id atoms with
      | Some (t,_purity) ->
          begin match trm_var_inv t with
          | Some x -> Some x
          | None -> trm_var_get_inv t
          end
      | _ -> Tools.warn "arith_core: broken invariant atoms in expr_atom_var_inv"; None
      end
  | _ -> None

(** [sort_wes] is an auxiliary function for [sort], to sort weighted items. *)
let sort_wes (atoms : atom_map) ~(constant_last:bool) (wes:wexprs) : wexprs =
  let cmp (w1,e1) (w2,e2) : int =
    match e1.expr_desc, e2.expr_desc with
    | Expr_int _, Expr_int _ ->
        (* if both are constants, keep the original order;
           this case does not happen if "gather" is called first,
           because the two integers would be added together *)
        0
    | Expr_int _, _ ->
        (* if e1 is constant, goes to back (if constant_last) *)
        if constant_last then 1 else -1
    | _, Expr_int _ ->
        (* if e2 is constant, goes to back (if constant_last) *)
        if constant_last then -1 else 1
    | Expr_atom _, Expr_atom _ ->
        begin match expr_atom_var_inv atoms e1, expr_atom_var_inv atoms e2 with
        | Some x1, Some x2 ->
            (* if one but not the other has weight one, it goes last; else alpha *)
            if w1 = 1 && w2 <> 1 then 1
            else if w1 <> 1 && w2 = 1 then -1
            else if x1 <= x2 then -1 else 1
        | Some x1, None -> 1 (* if e1 is compound but not e2, then e1 goes to head *)
        | None, Some x2 -> -1 (* if e2 is compound but not e1, then e2 goes to head *)
        | None, None -> 0 (* else keep original order *)
        end
    | Expr_atom _, _ -> 1
    | _, Expr_atom _ -> -1
    | _, _ ->
        if w1 > 0 && w2 < 0 then -1
        else if w1 < 0 && w2 > 0 then -1
        else 0
  in
  List.stable_sort cmp wes


(** [sort e] sorts elements in a sum by putting:
    1. compound subterms (unspecified order)
    2. subterms that consist of a weighted variable, in alphabetical order
    3. constants.
    For products, the order is similar except that constants go to the front. *)
let sort : arith_transfo  = fun (atoms,e) ->
  let e2 =
    match e.expr_desc with
    | Expr_sum wes -> { e with expr_desc = Expr_sum (sort_wes atoms ~constant_last:true wes) }
    | Expr_prod wes -> { e with expr_desc = Expr_prod (sort_wes atoms ~constant_last:false wes) }
    | _ -> e in
  (atoms,e2)


(******************************************************************************)
(*                          Expand                                 *)
(******************************************************************************)

(** [expand_one e]: expands a sum that appears inside a product.
    For example, [e1 * (e2 + e3)] becomes [e1 * e2 + e1 * e3].
    It can also expand e.g. [(e1 + e2) * (e3 + e4) * (e5 + e6)].
    Distribution is also supported:
    For example, [2 * (3*e2 + 4*e3)] becomes [6*e2 + 8*e3]
    The function performs nothing if no expansion can be performed.
    At the very end, it applies [normalize] to the result.
    Correctness: terms that are duplicated need to be redundant.
    We here assume that there is no empty [Expr_prod], else we'd need to check for deletable.
    LATER: the current check for redundancy is probably suboptimal in terms of efficiency. *)
let expand_one (e : expr) : expr =
  (* [aux (w,e) [e1; e2]]
     - if [w=1] and [e] is a sum [w1*a1 + w2*a2], then produce
       [w1*a1*e1 + w1*a1*e2 + w2*a2*e1 + w2*a2*e2],
       and all the [ai] and [ei] must be redundant
     - otherwise it computes [e^w*e1 + e^w*e2],
       and only [e] needs to be redundant if there is more than one [ei]. *)
  let aux (typ : typ_builtin) ((w,e) : wexpr) (acc : exprs) : exprs =
    if List.length acc > 1 then check_redundant e;
    (* LATER: check that e.typ does not conflict with typ *)
    match (w, e.expr_desc) with
    | 1, (Expr_sum wes) ->
        if List.length wes > 1 then List.iter check_redundant acc;
        List.concat_map (fun (wk,ak) ->
          List.map (fun ei ->
            expr_prod_nonweighted ~typ [(expr_int ~typ wk); ak; ei]) acc
        ) wes
    | _ ->
      (* LATER: see if we can use expr_mul instead, and perform simplications later *)
      (* [expr_distrib_we ei] produces [e^w * ei], with on-the-fly simplifications *)
      let expr_distrib_we (ei : expr) : expr =
        (* TODO: Check ei.expr_typ *)
        match ei.expr_desc with
        | Expr_prod [(_w, { expr_desc = Expr_int 1; _})] -> expr_prod ~typ [(w,e)]
        | Expr_prod wes -> expr_prod ~typ ((w,e) :: wes)
        | _ -> expr_prod ~typ [(w,e); (1,ei)]
        in
      List.map (fun ei -> expr_distrib_we ei) acc
    in
  let res =
    try
      let typ = Option.unsome e.expr_typ in
      match e.expr_desc with
      | Expr_sum wes -> (* case: [a1 * (b11*e11 + b12*e12) + a2 * (b21*e21 + b22*e22) + e3]
                           gives [(a1*b11) e11 + (a1*b12)*e12 + ... + e3] *)
          let expand_cst ((ai,ei) as aiei) =
            match ei.expr_desc with
            | Expr_sum wesi -> List.map (fun (bj,eij) -> (ai*bj, eij)) wesi
            | _ -> [aiei]
            in
          expr_sum ~typ (List.concat_map expand_cst wes)
      | Expr_prod wes -> (* case: [(e1 + e2) * (e3 + e4) * (e5 + e6)] *)
          let exprs_in_sum = List.fold_right (aux typ) wes [expr_one ?loc:e.expr_loc typ] in
          expr_sum_nonweighted ~typ exprs_in_sum
      | _ -> e
    with Unexpected_non_redundant -> e
    in
  normalize res

(** [expand_common recurse e]: calls [expand_one] recursively, calling the [gather] operations
    after each step. *)
let expand_common (recurse : bool) (e : expr) : expr =
  let tr (ei : expr) : expr =
    gather_rec (expand_one ei) in
  apply_bottom_up_if recurse cleanup_true tr e

(** [expand] and [expand_rec] can be passed as arguments to [Arith.simpl] *)
let expand = expand_common false

let expand_rec = expand_common true (* Warning: might be quadratic? *)

(******************************************************************************)
(*                          Euclidian                                         *)
(******************************************************************************)

(** [list_find_at_most_one f l] takes a list [l] and returns zero or one item
    satisfying [f], and returns the list of remaining items.
    LATER: implement as tail rec *)
let rec list_find_at_most_one (f:'a->bool) (l:'a list) : 'a option * 'a list =
  match l with
  | [] -> None, []
  | a::r ->
      if f a then
        (Some a, r)
      else begin
        let ao,ro = list_find_at_most_one f r in
        ao, (a::ro)
      end



(** [euclidian] exploits (a/q)*q + (a%q) = a.
The transformation looks for sums that contain:
  (1) the item [a % q] with multiplicative factor 1
  (2) the item [a / q] with multiplicative factor [q].
      where the division is an integer division with flooring.
Then it replaces the two terms with [a].

NOTE: must check [a] nonnegative.

LATER: generalize to: [a%q] with multiplicative factor k

implementation: extract all the pairs (a,q) that appear as [a%q] with
multiplicity 1, then for each of them, either extract [a/q] with multiplicity [q]
and put back [a] in the sum, or put back the original [a%q] in the sum.
*)
let euclidian (e : expr) : expr =
  match e.expr_desc with
  | Expr_sum wes ->
      (* filter [1 * (a % q)] items in the original sum [wes] *)
      let aq_pairs, wes_nonmod = List.partition_map
        (function
          | (1, { expr_desc = Expr_binop (Binop_trunc_mod, a, q); _ }) as modaq
             when is_deletable q && is_redundant a -> Left (a,q,modaq)
          | we -> Right(we))
          wes in
      let wes':wexprs = List.fold_left (fun (wes:wexprs) (a,q,modaq) ->
          (* auxiliary function to recognize [a/q] -- LATER: could use same_expr to do this? *)
          let is_a_div_q e =
            match e.expr_desc with
            | Expr_binop (Binop_trunc_div, a', q')
                when same_expr a a' && same_expr q q'
                -> true
            | _ -> false
            in
          (* filter at most one [q*(a/q)] or [(a/q)*q] item in the current sum [wes] *)
          let opt, wes_rest = list_find_at_most_one (
            function
            | (1, { expr_desc = Expr_prod [(1,e1);(1,e2)]; _ }) ->
                   (is_a_div_q e1 && same_expr q e2)
                || (is_a_div_q e2 && same_expr q e1)
            | _ -> false)
            wes in
          match opt with
          | None -> modaq :: wes_rest (* if [q*(a/q)]  not found, put back the original [a%q] *)
          | Some _ -> (1,a) :: wes_rest (* if [q*(a/q)] found, produce [a] *)
         ) wes_nonmod aq_pairs
        in
      expr_sum ~typ:(Option.unsome e.expr_typ) wes'
  | _ -> e



(******************************************************************************)
(*                          Compute                                 *)
(******************************************************************************)

(** [wexpr_is_numeric (w,e)] returns true if [e] is a constant [Expr_int]
   or [Expr_float]. *)

let wexpr_is_numeric ((w,e):wexpr) : bool =
  match e.expr_desc with
  | Expr_int _ | Expr_float _ -> true
  | _ -> false

(** [wexpr_is_int (w,e)] returns true if [e] is a constant [Expr_int]. *)
let wexpr_is_int ((w,e):wexpr) : bool =
  match e.expr_desc with
  | Expr_int _ -> true
  | _ -> false

(** [compute_power_int n w] computes [n^w] *)
let rec compute_power_int (n:int) (w:int) : int =
  assert (w >= 0);
  if w = 0 then 1 else n * compute_power_int n (w-1)

(** [compute_power_double f w] computes [f^w] *)
let rec compute_power_double (f:float) (w:int) : float =
  if w < 0 then begin
    let inv = compute_power_double f (-w) in
    if inv = 0. then failwith "compute_power_double: division by zero";
    1.0 /. inv
  end else begin
    if w = 0 then 1.0 else f *. compute_power_double f (w-1)
  end


let check_expr_typ_eq (ty1 : typ_builtin) (ty2 : typ_builtin) : unit =
  if (ty1 <> ty2) && !Flags.report_all_warnings then
    Tools.warn "arith types differ: %s and %s" (Ast_to_c.typ_to_string (typ_builtin ty1)) (Ast_to_c.typ_to_string (typ_builtin ty2))

(** [compute_wexpr_sum wes] assumes all items in [wes] to satisfy [wexpr_is_numeric],
   and it returns a single item [(w,e)] describing the numerical result.
   It is either of the form [(n, Expr_int 1)] in case the result is the integer [n],
   or of the form [(1, Expr_float f)] in case the result is the float value [f].
   When the sum involves only integers, the result is an integer;
   if, however, the sum involves at least one double, it is a double.
   FIXME: Documentation does not correspond to the code ! Code checks that all types are the same.
*)
let compute_wexpr_sum ~(typ : typ_builtin) ?(loc) (wes:wexprs) : wexpr =
  if List.for_all wexpr_is_int wes then begin
    let n = List.fold_left (fun acc (w,e) ->
      check_expr_typ_eq typ (Option.unsome e.expr_typ);
      acc + match e.expr_desc with
      | Expr_int n -> (w * n)
      | _ -> assert false
    ) 0 wes in
    (n, expr_int ~typ 1)
  end else begin
    let f = List.fold_left (fun acc (w,e) ->
      check_expr_typ_eq typ (Option.unsome e.expr_typ);
      acc +. match e.expr_desc with
      | Expr_int n -> float_of_int (w * n)
      | Expr_float f -> (float_of_int w) *. f
      | _ -> assert false
    ) 0. wes in
    (1, expr_float ~typ f)
  end

(** [compute_wexpr_prod wes] is similar to [compute_wexpr_sum], but for products.
   It returns a single item [(w,e)] describing the numerical result.
   It is either of the form [(1, Expr_int n)] in case the result is the integer [n],
   or of the forme [(1, Expr_float f)] in case the result is the float value [f]. *)
let compute_wexpr_prod ~(typ : typ_builtin) ?(loc) (wes:wexprs) : wexpr =
  if List.for_all wexpr_is_int wes then begin
    let wes_pos = List.filter (fun (w,_e) -> w >= 0) wes in
    let wes_neg = List.filter (fun (w,_e) -> w < 0) wes in
    let wes_neg = List.map (fun (w,e) -> (-w,e)) wes_neg in
    let wes_prod (wes_select:wexprs) : int =
      List.fold_left (fun acc (w,e) ->
        check_expr_typ_eq typ (Option.unsome e.expr_typ);
        acc * match e.expr_desc with
        | Expr_int n -> compute_power_int n w
        | _ -> assert false
      ) 1 wes_select in
    let num = wes_prod wes_pos in
    let denum = wes_prod wes_neg in
    if denum = 0 then loc_fail loc (Printf.sprintf "compute_wexpr_prod: exact integer division by zero: %d / %d" num denum);
    if num mod denum <> 0 then loc_fail loc (Printf.sprintf "compute_wexpr_prod: exact integer division is not exact: %d / %d" num denum);
    let n = num / denum in
    (1, expr_int ~typ n)
  end else begin
    let f = List.fold_left (fun acc (w,e) ->
      check_expr_typ_eq typ (Option.unsome e.expr_typ);
      acc *. match e.expr_desc with
      | Expr_int n -> compute_power_double (float_of_int n) w
      | Expr_float f -> compute_power_double f w
      | _ -> assert false
    ) 1. wes in
    (1, expr_float ~typ f)
  end

(** [compute_one e]: performs simplification of operations between known constants.
    For example, [4 + a + 3] becomes [a + 7].
    For example, [4.3 + a + 3] becomes [a + 7.3].
    For example, [10 / 2.5 - 3] becomes [1.0].
    The operation [normalize] is called at the end. *)
let compute_one (e : expr) : expr =
  let loc = e.expr_loc in
  let mk desc = expr_make_like e desc in
  let res = match e.expr_desc with
  | Expr_sum wes ->
      let wes_num, wes_rest = List.partition wexpr_is_numeric wes in
      if wes_num = [] then e else
      let we_num = compute_wexpr_sum ~typ:(Option.unsome e.expr_typ) ?loc wes_num in
      mk (Expr_sum (we_num :: wes_rest))
  | Expr_prod wes ->
      let wes_num, wes_rest = List.partition wexpr_is_numeric wes in
      if wes_num = [] then e else
      let we_num = compute_wexpr_prod ~typ:(Option.unsome e.expr_typ) ?loc wes_num in
      mk (Expr_prod (we_num :: wes_rest))
  (* binary operations on integers *)
  | Expr_binop (op, { expr_desc = Expr_int n1; _}, { expr_desc = Expr_int n2; _}) ->
    let typ = Option.unsome e.expr_typ in
    begin match op with
    | Binop_exact_div ->
      if n2 = 0 then loc_fail loc (Printf.sprintf "compute_one: integer division by zero: exact_div(%d, %d)" n1 n2);
      if (n1 mod n2) <> 0 then loc_fail loc (Printf.sprintf "compute_one: division is not exact: exact_div(%d, %d)" n1 n2);
      expr_int ~typ (n1 / n2) (* integer division with rounding *)
    | Binop_trunc_div ->
      if n2 = 0 then loc_fail loc (Printf.sprintf "compute_one: integer division by zero: %d / %d" n1 n2);
      expr_int ~typ (n1 / n2) (* integer division with rounding *)
    | Binop_trunc_mod ->
      if n2 = 0 then loc_fail loc (Printf.sprintf "compute_one: modulo by zero: %d / %d" n1 n2);
      expr_int ~typ (n1 mod n2)
    | Binop_shiftl ->
      expr_int ~typ (n1 lsl n2)
    | Binop_shiftr ->
      expr_int ~typ (n1 lsr n2)
    | Binop_xor ->
      expr_int ~typ (n1 lxor n2)
    | Binop_bitwise_and ->
      expr_int ~typ (n1 land n2)
    | Binop_bitwise_or ->
      expr_int ~typ (n1 lor n2)
    | _ -> e
    end
  | _ -> e (* LATER: don't use a catch-all branch, as it is error prone in case we add constructors *)
  in
  normalize res


(** [compute] can be passed as arguments to [Arith.simpl] *)
let compute (e:expr) : expr =
  apply_bottom_up_if recurse_true cleanup_true compute_one e


(******************************************************************************)
(*                          Transformation on terms                           *)
(******************************************************************************)


(** [map_on_arith_nodes tr t]: applies arithmetic simplification [tr] in depth of [t]*)
let rec map_on_arith_nodes (tr : trm -> trm) (t : trm) : trm =
  if has_mark_nosimpl t
    then t
    else if is_prim_arith_call t then tr t
    else
      trm_map (map_on_arith_nodes tr) t

(* DEBUG let c = ref 0 *)
(** [simplify_at_node f_atom f t] transforms a term [t] by reifying it as an [expr]
    AST, with non-arithmetic subterms described as "atoms"; then it applies the
    transformation [f_atom] on every atom; then it applies the transformation [f]
    on the reified expression; finally, it reconstructs the output term. *)
let simplify_at_node (f_atom : trm -> trm) (f : arith_transfo) (t : trm) : trm =
  try (
  let expr, atoms0 = trm_to_expr t in

  let atoms1 =
    (* If we could not extract any structure, then we don't process recursively the atoms
       using [f_atom], else we would trigger an infinite loop when [indepth=true]. *)
    match expr.expr_desc with
    | Expr_atom (_id,_purity) -> atoms0
    | _ -> Atom_map.map (fun (t_atom,purity) -> (f_atom t_atom,purity)) atoms0
    in
  let atoms2, expr2 = f (atoms1,expr) in
  if debug then Tools.debug "Expr after transformation: %s" (expr_to_string atoms1 expr2);
  (* let expr3 = normalize expr2 in
  if debug then Tools.debug "Expr after normalization: %s" (expr_to_string atoms expr3); *)
  expr_to_trm atoms2 expr2
  )
  with e ->
    Tools.warn "Arith.simplify_at_node: error on processing at loc %s:\n%s" (loc_to_string t.loc) (Printexc.to_string e);
    (* LATER: Sanitize this module to remove errors when reaching an unsupported case and stop catching the exception here *)
    t

(** [simplify indepth f t]: converts node [t] to an expression, then applies the
     simplifier [f], then it converts it back to a trm
    params:
      [f]: simplifier function
      [t]: the node on which the simplifications should be performed
    return:
      update t with the simplified expressions
  LATER: should [simplify false f t] fail if [t] is not an application of prim_arith? *)
let rec simplify (indepth : bool) (f : arith_transfo) (t : trm) : trm =
  if not indepth then begin
    let f_atom_identity = (fun ti -> ti) in
    simplify_at_node f_atom_identity f t
  end else begin
    let f_atom_simplify = simplify indepth f in
    map_on_arith_nodes (simplify_at_node f_atom_simplify f) t
  end

(* TODO: export a better name *)
let simplify2 = simplify

let simplify (indepth : bool) (f : expr -> expr) (t : trm) : trm =
  simplify indepth (fun (atoms,e) -> (atoms,f e)) t


(** [show_expr t] applies to a term [t] and replaces it with a term of the form
    [ARITH(t, "<expr-descr>")] where the term [t] is decorated,
    as part of a call to an identity function, with its representation as an [expr],
    as well as the description of the atoms involved. Moreover, for each atom the
    description indicates whether it is "non-duplicable" or "non-redundant".
    The keyword "ND" means non-duplicable, "NR" means non-redundant. *)
let show_expr ?(normalized = true) (t : trm) : trm =
  let expr, atoms = trm_to_expr ~normalized t in
  let sexpr = expr_to_string atoms expr in
  trm_apps (trm_var (toplevel_var "__ARITH")) [t; trm_string sexpr]


(******************************************************************************)
(*                          Static checks on terms                           *)
(******************************************************************************)

let trm_int_inv t =
  let neg, abs_t = match trm_unop_inv Unop_minus t with
    | Some abs_t -> true, abs_t
    | _ -> false, t
  in
  Option.map (fun n -> if neg then -n else n) (Trm.trm_int_inv abs_t)


(** [check_int_compare cmp t1 t2] tries to statically check that [cmp t1 t2] always holds. *)
let check_int_compare (cmp: int -> int -> bool) (t1: trm) (t2: trm) : bool =
  let t = simplify true (fun e -> gather (compute e)) (trm_sub_int t1 t2) in
  match trm_int_inv t with
  | Some i when cmp i 0 -> true
  | _ -> false

let check_eq = check_int_compare (=)
let check_neq = check_int_compare (<>)
let check_gt = check_int_compare (>)
let check_geq = check_int_compare (>=)
let check_lt = check_int_compare (<)
let check_leq = check_int_compare (<=)





(* TODO ARTHUR

D[ic / cn * cn + ic % cn] = (uint16_t)S[ic / cn * cn + ic % cn] +
 (uint16_t)S[(1 + ic / cn) * cn + ic % cn] +
 (uint16_t)S[(2 + ic / cn) * cn + ic % cn];

*)
