open AST
open BinNums
open Cop
open Ctypes
open Integers
open List0
open Values

type expr =
| Eval of coq_val * coq_type
| Evar of ident * coq_type
| Efield of expr * ident * coq_type
| Evalof of expr * coq_type
| Ederef of expr * coq_type
| Eaddrof of expr * coq_type
| Eunop of unary_operation * expr * coq_type
| Ebinop of binary_operation * expr * expr * coq_type
| Ecast of expr * coq_type
| Eseqand of expr * expr * coq_type
| Eseqor of expr * expr * coq_type
| Econdition of expr * expr * expr * coq_type
| Esizeof of coq_type * coq_type
| Ealignof of coq_type * coq_type
| Eassign of expr * expr * coq_type
| Eassignop of binary_operation * expr * expr * coq_type * coq_type
| Epostincr of incr_or_decr * expr * coq_type
| Ecomma of expr * expr * coq_type
| Ecall of expr * exprlist * coq_type
| Ebuiltin of external_function * typelist * exprlist * coq_type
| Eloc of block * Ptrofs.int * bitfield * coq_type
| Eparen of expr * coq_type * coq_type
and exprlist =
| Enil
| Econs of expr * exprlist

(** val coq_Eindex : expr -> expr -> coq_type -> expr **)

let coq_Eindex r1 r2 ty =
  Ederef ((Ebinop (Oadd, r1, r2, (Tpointer (ty, noattr)))), ty)

(** val coq_Epreincr : incr_or_decr -> expr -> coq_type -> expr **)

let coq_Epreincr id l ty =
  Eassignop ((match id with
              | Incr -> Oadd
              | Decr -> Osub), l, (Eval ((Vint Int.one), type_int32s)),
    (typeconv ty), ty)

(** val coq_Eselection : expr -> expr -> expr -> coq_type -> expr **)

let coq_Eselection r1 r2 r3 ty =
  let t = typ_of_type ty in
  let sg = { sig_args = (AST.Tint :: (t :: (t :: []))); sig_res = (Tret t);
    sig_cc = cc_default }
  in
  Ebuiltin ((EF_builtin
  (('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('s'::('e'::('l'::[]))))))))))))),
  sg)), (Tcons (type_bool, (Tcons (ty, (Tcons (ty, Tnil)))))), (Econs (r1,
  (Econs (r2, (Econs (r3, Enil)))))), ty)

(** val typeof : expr -> coq_type **)

let typeof = function
| Eval (_, ty) -> ty
| Evar (_, ty) -> ty
| Efield (_, _, ty) -> ty
| Evalof (_, ty) -> ty
| Ederef (_, ty) -> ty
| Eaddrof (_, ty) -> ty
| Eunop (_, _, ty) -> ty
| Ebinop (_, _, _, ty) -> ty
| Ecast (_, ty) -> ty
| Eseqand (_, _, ty) -> ty
| Eseqor (_, _, ty) -> ty
| Econdition (_, _, _, ty) -> ty
| Esizeof (_, ty) -> ty
| Ealignof (_, ty) -> ty
| Eassign (_, _, ty) -> ty
| Eassignop (_, _, _, _, ty) -> ty
| Epostincr (_, _, ty) -> ty
| Ecomma (_, _, ty) -> ty
| Ecall (_, _, ty) -> ty
| Ebuiltin (_, _, _, ty) -> ty
| Eloc (_, _, _, ty) -> ty
| Eparen (_, _, ty) -> ty

type label = ident

type statement =
| Sskip
| Sdo of expr
| Ssequence of statement * statement
| Sifthenelse of expr * statement * statement
| Swhile of expr * statement
| Sdowhile of expr * statement
| Sfor of statement * expr * statement * statement
| Sbreak
| Scontinue
| Sreturn of expr option
| Sswitch of expr * labeled_statements
| Slabel of label * statement
| Sgoto of label
and labeled_statements =
| LSnil
| LScons of coq_Z option * statement * labeled_statements

type coq_function = { fn_return : coq_type; fn_callconv : calling_convention;
                      fn_params : (ident * coq_type) list;
                      fn_vars : (ident * coq_type) list; fn_body : statement }

(** val var_names : (ident * coq_type) list -> ident list **)

let var_names vars =
  map fst vars

type fundef = coq_function Ctypes.fundef

(** val type_of_function : coq_function -> coq_type **)

let type_of_function f =
  Tfunction ((type_of_params f.fn_params), f.fn_return, f.fn_callconv)

(** val type_of_fundef : fundef -> coq_type **)

let type_of_fundef = function
| Internal fd -> type_of_function fd
| External (_, args, res, cc) -> Tfunction (args, res, cc)

type program = coq_function Ctypes.program
