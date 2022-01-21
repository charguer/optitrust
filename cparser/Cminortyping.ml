open AST
open BinNums
open Cminor
open Datatypes
open Errors
open Maps
open Unityping

type __ = Obj.t

(** val type_constant : constant -> typ **)

let type_constant = function
| Ointconst _ -> Tint
| Ofloatconst _ -> Tfloat
| Osingleconst _ -> Tsingle
| Olongconst _ -> Tlong
| _ -> coq_Tptr

(** val type_unop : unary_operation -> typ * typ **)

let type_unop = function
| Onegf -> (Tfloat, Tfloat)
| Oabsf -> (Tfloat, Tfloat)
| Onegfs -> (Tsingle, Tsingle)
| Oabsfs -> (Tsingle, Tsingle)
| Osingleoffloat -> (Tfloat, Tsingle)
| Ofloatofsingle -> (Tsingle, Tfloat)
| Ointoffloat -> (Tfloat, Tint)
| Ointuoffloat -> (Tfloat, Tint)
| Ofloatofint -> (Tint, Tfloat)
| Ofloatofintu -> (Tint, Tfloat)
| Ointofsingle -> (Tsingle, Tint)
| Ointuofsingle -> (Tsingle, Tint)
| Osingleofint -> (Tint, Tsingle)
| Osingleofintu -> (Tint, Tsingle)
| Onegl -> (Tlong, Tlong)
| Onotl -> (Tlong, Tlong)
| Ointoflong -> (Tlong, Tint)
| Olongofint -> (Tint, Tlong)
| Olongofintu -> (Tint, Tlong)
| Olongoffloat -> (Tfloat, Tlong)
| Olonguoffloat -> (Tfloat, Tlong)
| Ofloatoflong -> (Tlong, Tfloat)
| Ofloatoflongu -> (Tlong, Tfloat)
| Olongofsingle -> (Tsingle, Tlong)
| Olonguofsingle -> (Tsingle, Tlong)
| Osingleoflong -> (Tlong, Tsingle)
| Osingleoflongu -> (Tlong, Tsingle)
| _ -> (Tint, Tint)

(** val type_binop : binary_operation -> (typ * typ) * typ **)

let type_binop = function
| Oaddf -> ((Tfloat, Tfloat), Tfloat)
| Osubf -> ((Tfloat, Tfloat), Tfloat)
| Omulf -> ((Tfloat, Tfloat), Tfloat)
| Odivf -> ((Tfloat, Tfloat), Tfloat)
| Oaddfs -> ((Tsingle, Tsingle), Tsingle)
| Osubfs -> ((Tsingle, Tsingle), Tsingle)
| Omulfs -> ((Tsingle, Tsingle), Tsingle)
| Odivfs -> ((Tsingle, Tsingle), Tsingle)
| Oaddl -> ((Tlong, Tlong), Tlong)
| Osubl -> ((Tlong, Tlong), Tlong)
| Omull -> ((Tlong, Tlong), Tlong)
| Odivl -> ((Tlong, Tlong), Tlong)
| Odivlu -> ((Tlong, Tlong), Tlong)
| Omodl -> ((Tlong, Tlong), Tlong)
| Omodlu -> ((Tlong, Tlong), Tlong)
| Oandl -> ((Tlong, Tlong), Tlong)
| Oorl -> ((Tlong, Tlong), Tlong)
| Oxorl -> ((Tlong, Tlong), Tlong)
| Oshll -> ((Tlong, Tint), Tlong)
| Oshrl -> ((Tlong, Tint), Tlong)
| Oshrlu -> ((Tlong, Tint), Tlong)
| Ocmpf _ -> ((Tfloat, Tfloat), Tint)
| Ocmpfs _ -> ((Tsingle, Tsingle), Tint)
| Ocmpl _ -> ((Tlong, Tlong), Tint)
| Ocmplu _ -> ((Tlong, Tlong), Tint)
| _ -> ((Tint, Tint), Tint)

module RTLtypes =
 struct
  type t = typ

  (** val eq : typ -> typ -> bool **)

  let eq =
    typ_eq

  (** val default : typ **)

  let default =
    Tint
 end

module S = UniSolver(RTLtypes)

(** val expect : S.typenv -> typ -> typ -> S.typenv res **)

let expect e t1 t2 =
  if typ_eq t1 t2
  then OK e
  else Error
         (msg
           ('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))

(** val type_expr : S.typenv -> expr -> typ -> S.typenv res **)

let rec type_expr e a t0 =
  match a with
  | Evar id -> S.set e id t0
  | Econst c -> expect e (type_constant c) t0
  | Eunop (op, a1) ->
    let (targ1, tres) = type_unop op in
    (match type_expr e a1 targ1 with
     | OK x -> expect x tres t0
     | Error msg0 -> Error msg0)
  | Ebinop (op, a1, a2) ->
    let (p, tres) = type_binop op in
    let (targ1, targ2) = p in
    (match type_expr e a1 targ1 with
     | OK x ->
       (match type_expr x a2 targ2 with
        | OK x0 -> expect x0 tres t0
        | Error msg0 -> Error msg0)
     | Error msg0 -> Error msg0)
  | Eload (chunk, a1) ->
    (match type_expr e a1 coq_Tptr with
     | OK x -> expect x (type_of_chunk chunk) t0
     | Error msg0 -> Error msg0)

(** val type_exprlist : S.typenv -> expr list -> typ list -> S.typenv res **)

let rec type_exprlist e al tl =
  match al with
  | [] ->
    (match tl with
     | [] -> OK e
     | _ :: _ ->
       Error
         (msg
           ('a'::('r'::('i'::('t'::('y'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))
  | a :: al0 ->
    (match tl with
     | [] ->
       Error
         (msg
           ('a'::('r'::('i'::('t'::('y'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))
     | t0 :: tl0 ->
       (match type_expr e a t0 with
        | OK x -> type_exprlist x al0 tl0
        | Error msg0 -> Error msg0))

(** val type_assign : S.typenv -> ident -> expr -> S.typenv res **)

let type_assign e id = function
| Evar id' ->
  (match S.move e id id' with
   | OK p -> let (_, y) = p in OK y
   | Error msg0 -> Error msg0)
| Econst c -> S.set e id (type_constant c)
| Eunop (op, a1) ->
  let (targ1, tres) = type_unop op in
  (match type_expr e a1 targ1 with
   | OK x -> S.set x id tres
   | Error msg0 -> Error msg0)
| Ebinop (op, a1, a2) ->
  let (p, tres) = type_binop op in
  let (targ1, targ2) = p in
  (match type_expr e a1 targ1 with
   | OK x ->
     (match type_expr x a2 targ2 with
      | OK x0 -> S.set x0 id tres
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Eload (chunk, a1) ->
  (match type_expr e a1 coq_Tptr with
   | OK x -> S.set x id (type_of_chunk chunk)
   | Error msg0 -> Error msg0)

(** val opt_set : S.typenv -> ident option -> typ -> S.typenv res **)

let opt_set e optid ty =
  match optid with
  | Some id -> S.set e id ty
  | None -> OK e

(** val type_stmt : rettype -> S.typenv -> stmt -> S.typenv res **)

let rec type_stmt tret e = function
| Sassign (id, a) -> type_assign e id a
| Sstore (chunk, a1, a2) ->
  (match type_expr e a1 coq_Tptr with
   | OK x -> type_expr x a2 (type_of_chunk chunk)
   | Error msg0 -> Error msg0)
| Scall (optid, sg, fn, args) ->
  (match type_expr e fn coq_Tptr with
   | OK x ->
     (match type_exprlist x args sg.sig_args with
      | OK x0 -> opt_set x0 optid (proj_sig_res sg)
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Stailcall (sg, fn, args) ->
  if rettype_eq sg.sig_res tret
  then (match type_expr e fn coq_Tptr with
        | OK x -> type_exprlist x args sg.sig_args
        | Error msg0 -> Error msg0)
  else assertion_failed
| Sbuiltin (optid, ef, args) ->
  let sg = ef_sig ef in
  (match type_exprlist e args sg.sig_args with
   | OK x -> opt_set x optid (proj_sig_res sg)
   | Error msg0 -> Error msg0)
| Sseq (s1, s2) ->
  (match type_stmt tret e s1 with
   | OK x -> type_stmt tret x s2
   | Error msg0 -> Error msg0)
| Sifthenelse (a, s1, s2) ->
  (match type_expr e a Tint with
   | OK x ->
     (match type_stmt tret x s1 with
      | OK x0 -> type_stmt tret x0 s2
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Sloop s1 -> type_stmt tret e s1
| Sblock s1 -> type_stmt tret e s1
| Sswitch (sz, a, _, _) -> type_expr e a (if sz then Tlong else Tint)
| Sreturn opta ->
  (match opta with
   | Some a -> type_expr e a (proj_rettype tret)
   | None -> OK e)
| Slabel (_, s1) -> type_stmt tret e s1
| _ -> OK e

type typenv = ident -> typ

(** val type_function : coq_function -> typenv res **)

let type_function f =
  match S.set_list S.initial f.fn_params f.fn_sig.sig_args with
  | OK x ->
    (match type_stmt f.fn_sig.sig_res x f.fn_body with
     | OK x0 -> S.solve x0
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

type known_idents = unit PTree.t

(** val is_known : known_idents -> ident -> bool **)

let is_known ki id =
  match PTree.get id ki with
  | Some _ -> true
  | None -> false

(** val safe_unop : unary_operation -> bool **)

let safe_unop = function
| Ointoffloat -> false
| Ointuoffloat -> false
| Ofloatofint -> false
| Ofloatofintu -> false
| Ointofsingle -> false
| Ointuofsingle -> false
| Osingleofint -> false
| Osingleofintu -> false
| Olongoffloat -> false
| Olonguoffloat -> false
| Ofloatoflong -> false
| Ofloatoflongu -> false
| Olongofsingle -> false
| Olonguofsingle -> false
| Osingleoflong -> false
| Osingleoflongu -> false
| _ -> true

(** val safe_binop : binary_operation -> bool **)

let safe_binop = function
| Odiv -> false
| Odivu -> false
| Omod -> false
| Omodu -> false
| Odivl -> false
| Odivlu -> false
| Omodl -> false
| Omodlu -> false
| Ocmpl _ -> false
| Ocmplu _ -> false
| _ -> true

(** val safe_expr : known_idents -> expr -> bool **)

let rec safe_expr ki = function
| Evar v -> is_known ki v
| Econst _ -> true
| Eunop (op, e1) -> (&&) (safe_unop op) (safe_expr ki e1)
| Ebinop (op, e1, e2) ->
  (&&) ((&&) (safe_binop op) (safe_expr ki e1)) (safe_expr ki e2)
| Eload (_, _) -> false
