open AST
open BinNums
open Clight
open Compopts
open Conventions1
open Cop
open Coqlib
open Ctypes
open Datatypes
open Errors
open FSetAVL
open Int0
open List0
open Ordered

module VSet = Make(OrderedPositive)

type compilenv = VSet.t

(** val is_liftable_var : compilenv -> expr -> ident option **)

let is_liftable_var cenv = function
| Evar (id, _) -> if VSet.mem id cenv then Some id else None
| _ -> None

(** val make_cast : expr -> coq_type -> expr **)

let make_cast a tto =
  match classify_cast (typeof a) tto with
  | Coq_cast_case_pointer -> a
  | Coq_cast_case_i2i (sz2, _) ->
    (match sz2 with
     | I32 -> a
     | _ -> Ecast (a, tto))
  | Coq_cast_case_f2f -> a
  | Coq_cast_case_s2s -> a
  | Coq_cast_case_l2l -> a
  | Coq_cast_case_struct (_, _) -> a
  | Coq_cast_case_union (_, _) -> a
  | Coq_cast_case_void -> a
  | _ -> Ecast (a, tto)

(** val coq_Sdebug_temp : ident -> coq_type -> statement **)

let coq_Sdebug_temp id ty =
  Sbuiltin (None, (EF_debug ((Coq_xO Coq_xH), id, ((typ_of_type ty) :: []))),
    (Tcons ((typeconv ty), Tnil)), ((Etempvar (id, ty)) :: []))

(** val coq_Sdebug_var : ident -> coq_type -> statement **)

let coq_Sdebug_var id ty =
  Sbuiltin (None, (EF_debug ((Coq_xI (Coq_xO Coq_xH)), id,
    (coq_Tptr :: []))), (Tcons ((Tpointer (ty, noattr)), Tnil)), ((Eaddrof
    ((Evar (id, ty)), (Tpointer (ty, noattr)))) :: []))

(** val coq_Sset_debug : ident -> coq_type -> expr -> statement **)

let coq_Sset_debug id ty a =
  if debug ()
  then Ssequence ((Sset (id, (make_cast a ty))), (coq_Sdebug_temp id ty))
  else Sset (id, (make_cast a ty))

(** val simpl_expr : compilenv -> expr -> expr **)

let rec simpl_expr cenv a = match a with
| Evar (id, ty) ->
  if VSet.mem id cenv then Etempvar (id, ty) else Evar (id, ty)
| Etempvar (id, ty) -> Etempvar (id, ty)
| Ederef (a1, ty) -> Ederef ((simpl_expr cenv a1), ty)
| Eaddrof (a1, ty) -> Eaddrof ((simpl_expr cenv a1), ty)
| Eunop (op, a1, ty) -> Eunop (op, (simpl_expr cenv a1), ty)
| Ebinop (op, a1, a2, ty) ->
  Ebinop (op, (simpl_expr cenv a1), (simpl_expr cenv a2), ty)
| Ecast (a1, ty) -> Ecast ((simpl_expr cenv a1), ty)
| Efield (a1, fld, ty) -> Efield ((simpl_expr cenv a1), fld, ty)
| _ -> a

(** val simpl_exprlist : compilenv -> expr list -> expr list **)

let simpl_exprlist cenv al =
  map (simpl_expr cenv) al

(** val check_temp : compilenv -> ident -> unit res **)

let check_temp cenv id =
  if VSet.mem id cenv
  then Error ((MSG
         ('b'::('a'::('d'::(' '::('t'::('e'::('m'::('p'::('o'::('r'::('a'::('r'::('y'::(' '::[]))))))))))))))) :: ((CTX
         id) :: []))
  else OK ()

(** val check_opttemp : compilenv -> ident option -> unit res **)

let check_opttemp cenv = function
| Some id -> check_temp cenv id
| None -> OK ()

(** val simpl_stmt : compilenv -> statement -> statement res **)

let rec simpl_stmt cenv = function
| Sassign (a1, a2) ->
  (match is_liftable_var cenv a1 with
   | Some id -> OK (coq_Sset_debug id (typeof a1) (simpl_expr cenv a2))
   | None -> OK (Sassign ((simpl_expr cenv a1), (simpl_expr cenv a2))))
| Sset (id, a) ->
  (match check_temp cenv id with
   | OK _ -> OK (Sset (id, (simpl_expr cenv a)))
   | Error msg -> Error msg)
| Scall (optid, a, al) ->
  (match check_opttemp cenv optid with
   | OK _ -> OK (Scall (optid, (simpl_expr cenv a), (simpl_exprlist cenv al)))
   | Error msg -> Error msg)
| Sbuiltin (optid, ef, tyargs, al) ->
  (match check_opttemp cenv optid with
   | OK _ -> OK (Sbuiltin (optid, ef, tyargs, (simpl_exprlist cenv al)))
   | Error msg -> Error msg)
| Ssequence (s1, s2) ->
  (match simpl_stmt cenv s1 with
   | OK x ->
     (match simpl_stmt cenv s2 with
      | OK x0 -> OK (Ssequence (x, x0))
      | Error msg -> Error msg)
   | Error msg -> Error msg)
| Sifthenelse (a, s1, s2) ->
  (match simpl_stmt cenv s1 with
   | OK x ->
     (match simpl_stmt cenv s2 with
      | OK x0 -> OK (Sifthenelse ((simpl_expr cenv a), x, x0))
      | Error msg -> Error msg)
   | Error msg -> Error msg)
| Sloop (s1, s2) ->
  (match simpl_stmt cenv s1 with
   | OK x ->
     (match simpl_stmt cenv s2 with
      | OK x0 -> OK (Sloop (x, x0))
      | Error msg -> Error msg)
   | Error msg -> Error msg)
| Sreturn opta -> OK (Sreturn (option_map (simpl_expr cenv) opta))
| Sswitch (a, ls) ->
  (match simpl_lblstmt cenv ls with
   | OK x -> OK (Sswitch ((simpl_expr cenv a), x))
   | Error msg -> Error msg)
| Slabel (lbl, s0) ->
  (match simpl_stmt cenv s0 with
   | OK x -> OK (Slabel (lbl, x))
   | Error msg -> Error msg)
| x -> OK x

(** val simpl_lblstmt :
    compilenv -> labeled_statements -> labeled_statements res **)

and simpl_lblstmt cenv = function
| LSnil -> OK LSnil
| LScons (c, s, ls1) ->
  (match simpl_stmt cenv s with
   | OK x ->
     (match simpl_lblstmt cenv ls1 with
      | OK x0 -> OK (LScons (c, x, x0))
      | Error msg -> Error msg)
   | Error msg -> Error msg)

(** val store_params :
    compilenv -> (ident * coq_type) list -> statement -> statement **)

let rec store_params cenv params s =
  match params with
  | [] -> s
  | p :: params' ->
    let (id, ty) = p in
    if VSet.mem id cenv
    then if parameter_needs_normalization (rettype_of_type ty)
         then Ssequence ((Sset (id, (make_cast (Etempvar (id, ty)) ty))),
                (store_params cenv params' s))
         else store_params cenv params' s
    else Ssequence ((Sassign ((Evar (id, ty)), (Etempvar (id, ty)))),
           (store_params cenv params' s))

(** val addr_taken_expr : expr -> VSet.t **)

let rec addr_taken_expr = function
| Ederef (a1, _) -> addr_taken_expr a1
| Eaddrof (a1, _) ->
  (match a1 with
   | Evar (id, _) -> VSet.singleton id
   | _ -> addr_taken_expr a1)
| Eunop (_, a1, _) -> addr_taken_expr a1
| Ebinop (_, a1, a2, _) ->
  VSet.union (addr_taken_expr a1) (addr_taken_expr a2)
| Ecast (a1, _) -> addr_taken_expr a1
| Efield (a1, _, _) -> addr_taken_expr a1
| _ -> VSet.empty

(** val addr_taken_exprlist : expr list -> VSet.t **)

let rec addr_taken_exprlist = function
| [] -> VSet.empty
| a :: l' -> VSet.union (addr_taken_expr a) (addr_taken_exprlist l')

(** val addr_taken_stmt : statement -> VSet.t **)

let rec addr_taken_stmt = function
| Sassign (a, b) -> VSet.union (addr_taken_expr a) (addr_taken_expr b)
| Sset (_, a) -> addr_taken_expr a
| Scall (_, a, bl) -> VSet.union (addr_taken_expr a) (addr_taken_exprlist bl)
| Sbuiltin (_, _, _, bl) -> addr_taken_exprlist bl
| Ssequence (s1, s2) -> VSet.union (addr_taken_stmt s1) (addr_taken_stmt s2)
| Sifthenelse (a, s1, s2) ->
  VSet.union (addr_taken_expr a)
    (VSet.union (addr_taken_stmt s1) (addr_taken_stmt s2))
| Sloop (s1, s2) -> VSet.union (addr_taken_stmt s1) (addr_taken_stmt s2)
| Sreturn o -> (match o with
                | Some a -> addr_taken_expr a
                | None -> VSet.empty)
| Sswitch (a, ls) -> VSet.union (addr_taken_expr a) (addr_taken_lblstmt ls)
| Slabel (_, s0) -> addr_taken_stmt s0
| _ -> VSet.empty

(** val addr_taken_lblstmt : labeled_statements -> VSet.t **)

and addr_taken_lblstmt = function
| LSnil -> VSet.empty
| LScons (_, s, ls') ->
  VSet.union (addr_taken_stmt s) (addr_taken_lblstmt ls')

(** val add_local_variable :
    VSet.t -> (ident * coq_type) -> compilenv -> compilenv **)

let add_local_variable atk id_ty cenv =
  let (id, ty) = id_ty in
  (match access_mode ty with
   | By_value _ -> if VSet.mem id atk then cenv else VSet.add id cenv
   | _ -> cenv)

(** val cenv_for : coq_function -> compilenv **)

let cenv_for f =
  let atk = addr_taken_stmt f.fn_body in
  fold_right (add_local_variable atk) VSet.empty (app f.fn_params f.fn_vars)

(** val add_debug_var : (ident * coq_type) -> statement -> statement **)

let add_debug_var id_ty s =
  let (id, ty) = id_ty in Ssequence ((coq_Sdebug_var id ty), s)

(** val add_debug_vars : (ident * coq_type) list -> statement -> statement **)

let add_debug_vars vars s =
  if debug () then fold_right add_debug_var s vars else s

(** val add_debug_param : (ident * coq_type) -> statement -> statement **)

let add_debug_param id_ty s =
  let (id, ty) = id_ty in Ssequence ((coq_Sdebug_temp id ty), s)

(** val add_debug_params :
    (ident * coq_type) list -> statement -> statement **)

let add_debug_params params s =
  if debug () then fold_right add_debug_param s params else s

(** val remove_lifted :
    compilenv -> (ident * coq_type) list -> (VSet.elt * coq_type) list **)

let remove_lifted cenv vars =
  filter (fun id_ty -> negb (VSet.mem (fst id_ty) cenv)) vars

(** val add_lifted :
    compilenv -> (ident * coq_type) list -> (ident * coq_type) list ->
    (VSet.elt * coq_type) list **)

let add_lifted cenv vars1 vars2 =
  app (filter (fun id_ty -> VSet.mem (fst id_ty) cenv) vars1) vars2

(** val transf_function : coq_function -> coq_function res **)

let transf_function f =
  let cenv = cenv_for f in
  if list_disjoint_dec ident_eq (var_names f.fn_params) (var_names f.fn_temps)
  then (match simpl_stmt cenv f.fn_body with
        | OK x ->
          let vars' = remove_lifted cenv (app f.fn_params f.fn_vars) in
          let temps' = add_lifted cenv f.fn_vars f.fn_temps in
          OK { fn_return = f.fn_return; fn_callconv = f.fn_callconv;
          fn_params = f.fn_params; fn_vars = vars'; fn_temps = temps';
          fn_body =
          (add_debug_params f.fn_params
            (store_params cenv f.fn_params (add_debug_vars vars' x))) }
        | Error msg -> Error msg)
  else assertion_failed

(** val transf_fundef : Clight.fundef -> Clight.fundef res **)

let transf_fundef = function
| Internal f ->
  (match transf_function f with
   | OK x -> OK (Internal x)
   | Error msg -> Error msg)
| External (ef, targs, tres, cconv) -> OK (External (ef, targs, tres, cconv))

(** val transf_program : Clight.program -> Clight.program res **)

let transf_program p =
  match transform_partial_program transf_fundef (program_of_program p) with
  | OK x ->
    OK { prog_defs = x.AST.prog_defs; prog_public = x.AST.prog_public;
      prog_main = x.AST.prog_main; prog_types = p.prog_types; prog_comp_env =
      p.prog_comp_env }
  | Error msg -> Error msg
