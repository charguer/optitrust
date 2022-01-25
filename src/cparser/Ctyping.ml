open AST
open Archi
open BinInt
open BinNat
open Cop
open Coqlib
open Csyntax
open Ctypes
open Errors
open Floats
open Integers
open Maps
open Values

(** val strict : bool **)

let strict =
  false

(** val size_t : coq_type **)

let size_t =
  if ptr64 then Tlong (Unsigned, noattr) else Tint (I32, Unsigned, noattr)

(** val ptrdiff_t : coq_type **)

let ptrdiff_t =
  if ptr64 then Tlong (Signed, noattr) else Tint (I32, Signed, noattr)

(** val attr_add_volatile : bool -> attr -> attr **)

let attr_add_volatile vol a =
  { attr_volatile = ((||) a.attr_volatile vol); attr_alignas =
    a.attr_alignas }

(** val type_of_member : attr -> ident -> members -> coq_type res **)

let type_of_member a f m =
  match field_type f m with
  | OK x -> OK (change_attributes (attr_add_volatile a.attr_volatile) x)
  | Error msg0 -> Error msg0

(** val type_unop : unary_operation -> coq_type -> coq_type res **)

let type_unop op ty =
  match op with
  | Onotbool ->
    (match classify_bool ty with
     | Coq_bool_default ->
       Error
         (msg
           ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('!'::[])))))))))))
     | _ -> OK (Tint (I32, Signed, noattr)))
  | Onotint ->
    (match classify_notint ty with
     | Coq_notint_case_i sg -> OK (Tint (I32, sg, noattr))
     | Coq_notint_case_l sg -> OK (Tlong (sg, noattr))
     | Coq_notint_default ->
       Error
         (msg
           ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('~'::[]))))))))))))
  | Oneg ->
    (match classify_neg ty with
     | Coq_neg_case_i sg -> OK (Tint (I32, sg, noattr))
     | Coq_neg_case_f -> OK (Tfloat (F64, noattr))
     | Coq_neg_case_s -> OK (Tfloat (F32, noattr))
     | Coq_neg_case_l sg -> OK (Tlong (sg, noattr))
     | Coq_neg_default ->
       Error
         (msg
           ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('p'::('r'::('e'::('f'::('i'::('x'::(' '::('-'::[])))))))))))))))))))
  | Oabsfloat ->
    (match classify_neg ty with
     | Coq_neg_default ->
       Error
         (msg
           ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('f'::('a'::('b'::('s'::[]))))))))))))))))))))))))
     | _ -> OK (Tfloat (F64, noattr)))

(** val binarith_type : coq_type -> coq_type -> char list -> coq_type res **)

let binarith_type ty1 ty2 m =
  match classify_binarith ty1 ty2 with
  | Coq_bin_case_i sg -> OK (Tint (I32, sg, noattr))
  | Coq_bin_case_l sg -> OK (Tlong (sg, noattr))
  | Coq_bin_case_f -> OK (Tfloat (F64, noattr))
  | Coq_bin_case_s -> OK (Tfloat (F32, noattr))
  | Coq_bin_default -> Error (msg m)

(** val binarith_int_type :
    coq_type -> coq_type -> char list -> coq_type res **)

let binarith_int_type ty1 ty2 m =
  match classify_binarith ty1 ty2 with
  | Coq_bin_case_i sg -> OK (Tint (I32, sg, noattr))
  | Coq_bin_case_l sg -> OK (Tlong (sg, noattr))
  | _ -> Error (msg m)

(** val shift_op_type : coq_type -> coq_type -> char list -> coq_type res **)

let shift_op_type ty1 ty2 m =
  match classify_shift ty1 ty2 with
  | Coq_shift_case_ii sg -> OK (Tint (I32, sg, noattr))
  | Coq_shift_case_ll sg -> OK (Tlong (sg, noattr))
  | Coq_shift_case_il sg -> OK (Tint (I32, sg, noattr))
  | Coq_shift_case_li sg -> OK (Tlong (sg, noattr))
  | Coq_shift_default -> Error (msg m)

(** val comparison_type :
    coq_type -> coq_type -> char list -> coq_type res **)

let comparison_type ty1 ty2 m =
  match classify_cmp ty1 ty2 with
  | Coq_cmp_default ->
    (match classify_binarith ty1 ty2 with
     | Coq_bin_default -> Error (msg m)
     | _ -> OK (Tint (I32, Signed, noattr)))
  | _ -> OK (Tint (I32, Signed, noattr))

(** val type_binop :
    binary_operation -> coq_type -> coq_type -> coq_type res **)

let type_binop op ty1 ty2 =
  match op with
  | Oadd ->
    (match classify_add ty1 ty2 with
     | Coq_add_case_pi (ty, _) -> OK (Tpointer (ty, noattr))
     | Coq_add_case_pl ty -> OK (Tpointer (ty, noattr))
     | Coq_add_case_ip (_, ty) -> OK (Tpointer (ty, noattr))
     | Coq_add_case_lp ty -> OK (Tpointer (ty, noattr))
     | Coq_add_default ->
       binarith_type ty1 ty2
         ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('+'::[])))))))))))
  | Osub ->
    (match classify_sub ty1 ty2 with
     | Coq_sub_case_pi (ty, _) -> OK (Tpointer (ty, noattr))
     | Coq_sub_case_pp _ -> OK ptrdiff_t
     | Coq_sub_case_pl ty -> OK (Tpointer (ty, noattr))
     | Coq_sub_default ->
       binarith_type ty1 ty2
         ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('i'::('n'::('f'::('i'::('x'::(' '::('-'::[])))))))))))))))))
  | Omul ->
    binarith_type ty1 ty2
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('i'::('n'::('f'::('i'::('x'::(' '::('*'::[]))))))))))))))))
  | Odiv ->
    binarith_type ty1 ty2
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('/'::[]))))))))))
  | Omod ->
    binarith_int_type ty1 ty2
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('%'::[]))))))))))
  | Oand ->
    binarith_int_type ty1 ty2
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('&'::[]))))))))))
  | Oor ->
    binarith_int_type ty1 ty2
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('|'::[]))))))))))
  | Oxor ->
    binarith_int_type ty1 ty2
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('^'::[]))))))))))
  | Oshl ->
    shift_op_type ty1 ty2
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('<'::('<'::[])))))))))))
  | Oshr ->
    shift_op_type ty1 ty2
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('>'::('>'::[])))))))))))
  | Oeq ->
    comparison_type ty1 ty2
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('='::('='::[])))))))))))
  | One ->
    comparison_type ty1 ty2
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('!'::('='::[])))))))))))
  | Olt ->
    comparison_type ty1 ty2
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('<'::[]))))))))))
  | Ogt ->
    comparison_type ty1 ty2
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('>'::[]))))))))))
  | Ole ->
    comparison_type ty1 ty2
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('<'::('='::[])))))))))))
  | Oge ->
    comparison_type ty1 ty2
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('>'::('='::[])))))))))))

(** val type_deref : coq_type -> coq_type res **)

let type_deref ty = match ty with
| Tpointer (tyelt, _) -> OK tyelt
| Tarray (tyelt, _, _) -> OK tyelt
| Tfunction (_, _, _) -> OK ty
| _ ->
  Error
    (msg
      ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::(' '::('p'::('r'::('e'::('f'::('i'::('x'::(' '::('*'::[]))))))))))))))))))

(** val attr_combine : attr -> attr -> attr **)

let attr_combine a1 a2 =
  { attr_volatile = ((||) a1.attr_volatile a2.attr_volatile); attr_alignas =
    (match a1.attr_alignas with
     | Some n1 ->
       (match a2.attr_alignas with
        | Some n2 -> Some (N.max n1 n2)
        | None -> Some n1)
     | None -> a2.attr_alignas) }

(** val intsize_eq : intsize -> intsize -> bool **)

let intsize_eq x y =
  match x with
  | I8 -> (match y with
           | I8 -> true
           | _ -> false)
  | I16 -> (match y with
            | I16 -> true
            | _ -> false)
  | I32 -> (match y with
            | I32 -> true
            | _ -> false)
  | IBool -> (match y with
              | IBool -> true
              | _ -> false)

(** val signedness_eq : signedness -> signedness -> bool **)

let signedness_eq x y =
  match x with
  | Signed -> (match y with
               | Signed -> true
               | Unsigned -> false)
  | Unsigned -> (match y with
                 | Signed -> false
                 | Unsigned -> true)

(** val floatsize_eq : floatsize -> floatsize -> bool **)

let floatsize_eq x y =
  match x with
  | F32 -> (match y with
            | F32 -> true
            | F64 -> false)
  | F64 -> (match y with
            | F32 -> false
            | F64 -> true)

(** val callconv_combine :
    calling_convention -> calling_convention -> calling_convention res **)

let callconv_combine cc1 cc2 =
  if option_eq Z.eq_dec cc1.cc_vararg cc2.cc_vararg
  then OK { cc_vararg = cc1.cc_vararg; cc_unproto =
         ((&&) cc1.cc_unproto cc2.cc_unproto); cc_structret =
         cc1.cc_structret }
  else Error
         (msg
           ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('c'::('a'::('l'::('l'::('i'::('n'::('g'::(' '::('c'::('o'::('n'::('v'::('e'::('n'::('t'::('i'::('o'::('n'::('s'::[])))))))))))))))))))))))))))))))))

(** val type_combine : coq_type -> coq_type -> coq_type res **)

let rec type_combine ty1 ty2 =
  match ty1 with
  | Tvoid ->
    (match ty2 with
     | Tvoid -> OK Tvoid
     | _ ->
       Error
         (msg
           ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('t'::('y'::('p'::('e'::('s'::[]))))))))))))))))))))
  | Tint (sz1, sg1, a1) ->
    (match ty2 with
     | Tint (sz2, sg2, a2) ->
       if (&&) ((fun x -> x) (intsize_eq sz1 sz2))
            ((fun x -> x) (signedness_eq sg1 sg2))
       then OK (Tint (sz1, sg1, (attr_combine a1 a2)))
       else Error
              (msg
                ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('i'::('n'::('t'::(' '::('t'::('y'::('p'::('e'::('s'::[])))))))))))))))))))))))
     | _ ->
       Error
         (msg
           ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('t'::('y'::('p'::('e'::('s'::[]))))))))))))))))))))
  | Tlong (sg1, a1) ->
    (match ty2 with
     | Tlong (sg2, a2) ->
       if signedness_eq sg1 sg2
       then OK (Tlong (sg1, (attr_combine a1 a2)))
       else Error
              (msg
                ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('l'::('o'::('n'::('g'::(' '::('t'::('y'::('p'::('e'::('s'::[]))))))))))))))))))))))))
     | _ ->
       Error
         (msg
           ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('t'::('y'::('p'::('e'::('s'::[]))))))))))))))))))))
  | Tfloat (sz1, a1) ->
    (match ty2 with
     | Tfloat (sz2, a2) ->
       if floatsize_eq sz1 sz2
       then OK (Tfloat (sz1, (attr_combine a1 a2)))
       else Error
              (msg
                ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('f'::('l'::('o'::('a'::('t'::(' '::('t'::('y'::('p'::('e'::('s'::[])))))))))))))))))))))))))
     | _ ->
       Error
         (msg
           ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('t'::('y'::('p'::('e'::('s'::[]))))))))))))))))))))
  | Tpointer (t1, a1) ->
    (match ty2 with
     | Tpointer (t2, a2) ->
       (match type_combine t1 t2 with
        | OK x -> OK (Tpointer (x, (attr_combine a1 a2)))
        | Error msg0 -> Error msg0)
     | _ ->
       Error
         (msg
           ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('t'::('y'::('p'::('e'::('s'::[]))))))))))))))))))))
  | Tarray (t1, sz1, a1) ->
    (match ty2 with
     | Tarray (t2, sz2, a2) ->
       (match type_combine t1 t2 with
        | OK x ->
          if zeq sz1 sz2
          then OK (Tarray (x, sz1, (attr_combine a1 a2)))
          else Error
                 (msg
                   ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('a'::('r'::('r'::('a'::('y'::(' '::('t'::('y'::('p'::('e'::('s'::[])))))))))))))))))))))))))
        | Error msg0 -> Error msg0)
     | _ ->
       Error
         (msg
           ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('t'::('y'::('p'::('e'::('s'::[]))))))))))))))))))))
  | Tfunction (args1, res1, cc1) ->
    (match ty2 with
     | Tfunction (args2, res2, cc2) ->
       (match type_combine res1 res2 with
        | OK x ->
          (match if cc1.cc_unproto
                 then OK args2
                 else if cc2.cc_unproto
                      then OK args1
                      else typelist_combine args1 args2 with
           | OK x0 ->
             (match callconv_combine cc1 cc2 with
              | OK x1 -> OK (Tfunction (x0, x, x1))
              | Error msg0 -> Error msg0)
           | Error msg0 -> Error msg0)
        | Error msg0 -> Error msg0)
     | _ ->
       Error
         (msg
           ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('t'::('y'::('p'::('e'::('s'::[]))))))))))))))))))))
  | Tstruct (id1, a1) ->
    (match ty2 with
     | Tstruct (id2, a2) ->
       if ident_eq id1 id2
       then OK (Tstruct (id1, (attr_combine a1 a2)))
       else Error
              (msg
                ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('s'::('t'::('r'::('u'::('c'::('t'::(' '::('t'::('y'::('p'::('e'::('s'::[]))))))))))))))))))))))))))
     | _ ->
       Error
         (msg
           ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('t'::('y'::('p'::('e'::('s'::[]))))))))))))))))))))
  | Tunion (id1, a1) ->
    (match ty2 with
     | Tunion (id2, a2) ->
       if ident_eq id1 id2
       then OK (Tunion (id1, (attr_combine a1 a2)))
       else Error
              (msg
                ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('u'::('n'::('i'::('o'::('n'::(' '::('t'::('y'::('p'::('e'::('s'::[])))))))))))))))))))))))))
     | _ ->
       Error
         (msg
           ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('t'::('y'::('p'::('e'::('s'::[]))))))))))))))))))))

(** val typelist_combine : typelist -> typelist -> typelist res **)

and typelist_combine tl1 tl2 =
  match tl1 with
  | Tnil ->
    (match tl2 with
     | Tnil -> OK Tnil
     | Tcons (_, _) ->
       Error
         (msg
           ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('t'::('y'::('p'::('e'::('s'::[])))))))))))))))))))))))))))))
  | Tcons (t1, tl3) ->
    (match tl2 with
     | Tnil ->
       Error
         (msg
           ('i'::('n'::('c'::('o'::('m'::('p'::('a'::('t'::('i'::('b'::('l'::('e'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('t'::('y'::('p'::('e'::('s'::[]))))))))))))))))))))))))))))
     | Tcons (t2, tl4) ->
       (match type_combine t1 t2 with
        | OK x ->
          (match typelist_combine tl3 tl4 with
           | OK x0 -> OK (Tcons (x, x0))
           | Error msg0 -> Error msg0)
        | Error msg0 -> Error msg0))

(** val is_void : coq_type -> bool **)

let is_void = function
| Tvoid -> true
| _ -> false

(** val type_conditional : coq_type -> coq_type -> coq_type res **)

let type_conditional ty1 ty2 =
  match typeconv ty1 with
  | Tint (i, s, a) ->
    let t1 = Tint (i, s, a) in
    (match typeconv ty2 with
     | Tint (_, _, _) ->
       binarith_type ty1 ty2
         ('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::('a'::('l'::(' '::('e'::('x'::('p'::('r'::('e'::('s'::('s'::('i'::('o'::('n'::[]))))))))))))))))))))))
     | Tlong (_, _) ->
       binarith_type ty1 ty2
         ('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::('a'::('l'::(' '::('e'::('x'::('p'::('r'::('e'::('s'::('s'::('i'::('o'::('n'::[]))))))))))))))))))))))
     | Tfloat (_, _) ->
       binarith_type ty1 ty2
         ('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::('a'::('l'::(' '::('e'::('x'::('p'::('r'::('e'::('s'::('s'::('i'::('o'::('n'::[]))))))))))))))))))))))
     | Tpointer (t2, _) -> OK (Tpointer (t2, noattr))
     | x -> type_combine t1 x)
  | Tlong (s, a) ->
    let t1 = Tlong (s, a) in
    (match typeconv ty2 with
     | Tint (_, _, _) ->
       binarith_type ty1 ty2
         ('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::('a'::('l'::(' '::('e'::('x'::('p'::('r'::('e'::('s'::('s'::('i'::('o'::('n'::[]))))))))))))))))))))))
     | Tlong (_, _) ->
       binarith_type ty1 ty2
         ('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::('a'::('l'::(' '::('e'::('x'::('p'::('r'::('e'::('s'::('s'::('i'::('o'::('n'::[]))))))))))))))))))))))
     | Tfloat (_, _) ->
       binarith_type ty1 ty2
         ('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::('a'::('l'::(' '::('e'::('x'::('p'::('r'::('e'::('s'::('s'::('i'::('o'::('n'::[]))))))))))))))))))))))
     | x -> type_combine t1 x)
  | Tfloat (f, a) ->
    let t1 = Tfloat (f, a) in
    (match typeconv ty2 with
     | Tint (_, _, _) ->
       binarith_type ty1 ty2
         ('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::('a'::('l'::(' '::('e'::('x'::('p'::('r'::('e'::('s'::('s'::('i'::('o'::('n'::[]))))))))))))))))))))))
     | Tlong (_, _) ->
       binarith_type ty1 ty2
         ('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::('a'::('l'::(' '::('e'::('x'::('p'::('r'::('e'::('s'::('s'::('i'::('o'::('n'::[]))))))))))))))))))))))
     | Tfloat (_, _) ->
       binarith_type ty1 ty2
         ('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::('a'::('l'::(' '::('e'::('x'::('p'::('r'::('e'::('s'::('s'::('i'::('o'::('n'::[]))))))))))))))))))))))
     | x -> type_combine t1 x)
  | Tpointer (t1, a1) ->
    let t2 = Tpointer (t1, a1) in
    (match typeconv ty2 with
     | Tint (_, _, _) -> OK (Tpointer (t1, noattr))
     | Tpointer (t3, _) ->
       let t0 =
         if (||) (is_void t1) (is_void t3)
         then Tvoid
         else (match type_combine t1 t3 with
               | OK t0 -> t0
               | Error _ -> Tvoid)
       in
       OK (Tpointer (t0, noattr))
     | x -> type_combine t2 x)
  | x -> type_combine x (typeconv ty2)

type typenv = coq_type PTree.t

(** val bind_vars : typenv -> (ident * coq_type) list -> typenv **)

let rec bind_vars e = function
| [] -> e
| p :: l0 -> let (id, ty) = p in bind_vars (PTree.set id ty e) l0

(** val bind_globdef :
    typenv -> (ident * (Csyntax.fundef, coq_type) globdef) list -> typenv **)

let rec bind_globdef e = function
| [] -> e
| p :: l0 ->
  let (id, g) = p in
  (match g with
   | Gfun fd -> bind_globdef (PTree.set id (type_of_fundef fd) e) l0
   | Gvar v -> bind_globdef (PTree.set id v.gvar_info e) l0)

(** val check_cast : coq_type -> coq_type -> unit res **)

let check_cast t1 t2 =
  match classify_cast t1 t2 with
  | Coq_cast_case_default ->
    Error
      (msg
        ('i'::('l'::('l'::('e'::('g'::('a'::('l'::(' '::('c'::('a'::('s'::('t'::[])))))))))))))
  | _ -> OK ()

(** val check_bool : coq_type -> unit res **)

let check_bool t0 =
  match classify_bool t0 with
  | Coq_bool_default ->
    Error
      (msg
        ('n'::('o'::('t'::(' '::('a'::(' '::('b'::('o'::('o'::('l'::('e'::('a'::('n'::[]))))))))))))))
  | _ -> OK ()

(** val check_arguments : exprlist -> typelist -> unit res **)

let rec check_arguments el tyl =
  match el with
  | Enil ->
    (match tyl with
     | Tnil -> OK ()
     | Tcons (_, _) ->
       Error
         (msg
           ('n'::('o'::('t'::(' '::('e'::('n'::('o'::('u'::('g'::('h'::(' '::('a'::('r'::('g'::('u'::('m'::('e'::('n'::('t'::('s'::[]))))))))))))))))))))))
  | Econs (e1, el0) ->
    (match tyl with
     | Tnil ->
       if strict
       then Error
              (msg
                ('t'::('o'::('o'::(' '::('m'::('a'::('n'::('y'::(' '::('a'::('r'::('g'::('u'::('m'::('e'::('n'::('t'::('s'::[])))))))))))))))))))
       else OK ()
     | Tcons (ty1, tyl0) ->
       (match check_cast (typeof e1) ty1 with
        | OK _ -> check_arguments el0 tyl0
        | Error msg0 -> Error msg0))

(** val check_rval : expr -> unit res **)

let check_rval = function
| Evar (_, _) ->
  Error
    (msg
      ('n'::('o'::('t'::(' '::('a'::(' '::('r'::('-'::('v'::('a'::('l'::('u'::('e'::[]))))))))))))))
| Efield (_, _, _) ->
  Error
    (msg
      ('n'::('o'::('t'::(' '::('a'::(' '::('r'::('-'::('v'::('a'::('l'::('u'::('e'::[]))))))))))))))
| Ederef (_, _) ->
  Error
    (msg
      ('n'::('o'::('t'::(' '::('a'::(' '::('r'::('-'::('v'::('a'::('l'::('u'::('e'::[]))))))))))))))
| Eloc (_, _, _, _) ->
  Error
    (msg
      ('n'::('o'::('t'::(' '::('a'::(' '::('r'::('-'::('v'::('a'::('l'::('u'::('e'::[]))))))))))))))
| _ -> OK ()

(** val check_lval : expr -> unit res **)

let check_lval = function
| Evar (_, _) -> OK ()
| Efield (_, _, _) -> OK ()
| Ederef (_, _) -> OK ()
| Eloc (_, _, _, _) -> OK ()
| _ ->
  Error
    (msg
      ('n'::('o'::('t'::(' '::('a'::(' '::('l'::('-'::('v'::('a'::('l'::('u'::('e'::[]))))))))))))))

(** val check_rvals : exprlist -> unit res **)

let rec check_rvals = function
| Enil -> OK ()
| Econs (e1, el0) ->
  (match check_rval e1 with
   | OK _ -> check_rvals el0
   | Error msg0 -> Error msg0)

(** val evar : typenv -> ident -> expr res **)

let evar e x =
  match PTree.get x e with
  | Some ty -> OK (Evar (x, ty))
  | None ->
    Error ((MSG
      ('u'::('n'::('b'::('o'::('u'::('n'::('d'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(' '::[])))))))))))))))))) :: ((CTX
      x) :: []))

(** val ederef : expr -> expr res **)

let ederef r =
  match check_rval r with
  | OK _ ->
    (match type_deref (typeof r) with
     | OK x -> OK (Ederef (r, x))
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val efield : composite_env -> expr -> ident -> expr res **)

let efield ce r f =
  match check_rval r with
  | OK _ ->
    (match typeof r with
     | Tstruct (id, a) ->
       (match PTree.get id ce with
        | Some co ->
          (match type_of_member a f co.co_members with
           | OK x -> OK (Efield (r, f, x))
           | Error msg0 -> Error msg0)
        | None ->
          Error ((MSG
            ('u'::('n'::('b'::('o'::('u'::('n'::('d'::(' '::('c'::('o'::('m'::('p'::('o'::('s'::('i'::('t'::('e'::(' '::[]))))))))))))))))))) :: ((CTX
            id) :: [])))
     | Tunion (id, a) ->
       (match PTree.get id ce with
        | Some co ->
          (match type_of_member a f co.co_members with
           | OK x -> OK (Efield (r, f, x))
           | Error msg0 -> Error msg0)
        | None ->
          Error ((MSG
            ('u'::('n'::('b'::('o'::('u'::('n'::('d'::(' '::('c'::('o'::('m'::('p'::('o'::('s'::('i'::('t'::('e'::(' '::[]))))))))))))))))))) :: ((CTX
            id) :: [])))
     | _ ->
       Error ((MSG
         ('a'::('r'::('g'::('u'::('m'::('e'::('n'::('t'::(' '::('o'::('f'::(' '::('.'::[])))))))))))))) :: ((CTX
         f) :: ((MSG
         (' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('t'::('r'::('u'::('c'::('t'::(' '::('o'::('r'::(' '::('u'::('n'::('i'::('o'::('n'::[])))))))))))))))))))))))))) :: []))))
  | Error msg0 -> Error msg0

(** val econst_int : Int.int -> signedness -> expr **)

let econst_int n sg =
  Eval ((Vint n), (Tint (I32, sg, noattr)))

(** val econst_ptr_int : Int.int -> coq_type -> expr **)

let econst_ptr_int n ty =
  Eval ((if ptr64 then Vlong (Int64.repr (Int.unsigned n)) else Vint n),
    (Tpointer (ty, noattr)))

(** val econst_long : Int64.int -> signedness -> expr **)

let econst_long n sg =
  Eval ((Vlong n), (Tlong (sg, noattr)))

(** val econst_float : float -> expr **)

let econst_float n =
  Eval ((Vfloat n), (Tfloat (F64, noattr)))

(** val econst_single : float32 -> expr **)

let econst_single n =
  Eval ((Vsingle n), (Tfloat (F32, noattr)))

(** val evalof : expr -> expr res **)

let evalof l =
  match check_lval l with
  | OK _ -> OK (Evalof (l, (typeof l)))
  | Error msg0 -> Error msg0

(** val eaddrof : expr -> expr res **)

let eaddrof l =
  match check_lval l with
  | OK _ -> OK (Eaddrof (l, (Tpointer ((typeof l), noattr))))
  | Error msg0 -> Error msg0

(** val eunop : unary_operation -> expr -> expr res **)

let eunop op r =
  match check_rval r with
  | OK _ ->
    (match type_unop op (typeof r) with
     | OK x -> OK (Eunop (op, r, x))
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val ebinop : binary_operation -> expr -> expr -> expr res **)

let ebinop op r1 r2 =
  match check_rval r1 with
  | OK _ ->
    (match check_rval r2 with
     | OK _ ->
       (match type_binop op (typeof r1) (typeof r2) with
        | OK x -> OK (Ebinop (op, r1, r2, x))
        | Error msg0 -> Error msg0)
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val ecast : coq_type -> expr -> expr res **)

let ecast ty r =
  match check_rval r with
  | OK _ ->
    (match check_cast (typeof r) ty with
     | OK _ -> OK (Ecast (r, ty))
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val eseqand : expr -> expr -> expr res **)

let eseqand r1 r2 =
  match check_rval r1 with
  | OK _ ->
    (match check_rval r2 with
     | OK _ ->
       (match check_bool (typeof r1) with
        | OK _ ->
          (match check_bool (typeof r2) with
           | OK _ -> OK (Eseqand (r1, r2, type_int32s))
           | Error msg0 -> Error msg0)
        | Error msg0 -> Error msg0)
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val eseqor : expr -> expr -> expr res **)

let eseqor r1 r2 =
  match check_rval r1 with
  | OK _ ->
    (match check_rval r2 with
     | OK _ ->
       (match check_bool (typeof r1) with
        | OK _ ->
          (match check_bool (typeof r2) with
           | OK _ -> OK (Eseqor (r1, r2, type_int32s))
           | Error msg0 -> Error msg0)
        | Error msg0 -> Error msg0)
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val econdition : expr -> expr -> expr -> expr res **)

let econdition r1 r2 r3 =
  match check_rval r1 with
  | OK _ ->
    (match check_rval r2 with
     | OK _ ->
       (match check_rval r3 with
        | OK _ ->
          (match check_bool (typeof r1) with
           | OK _ ->
             (match type_conditional (typeof r2) (typeof r3) with
              | OK x -> OK (Econdition (r1, r2, r3, x))
              | Error msg0 -> Error msg0)
           | Error msg0 -> Error msg0)
        | Error msg0 -> Error msg0)
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val esizeof : coq_type -> expr **)

let esizeof ty =
  Esizeof (ty, size_t)

(** val ealignof : coq_type -> expr **)

let ealignof ty =
  Ealignof (ty, size_t)

(** val eassign : expr -> expr -> expr res **)

let eassign l r =
  match check_lval l with
  | OK _ ->
    (match check_rval r with
     | OK _ ->
       (match check_cast (typeof r) (typeof l) with
        | OK _ -> OK (Eassign (l, r, (typeof l)))
        | Error msg0 -> Error msg0)
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val eassignop : binary_operation -> expr -> expr -> expr res **)

let eassignop op l r =
  match check_lval l with
  | OK _ ->
    (match check_rval r with
     | OK _ ->
       (match type_binop op (typeof l) (typeof r) with
        | OK x ->
          (match check_cast x (typeof l) with
           | OK _ -> OK (Eassignop (op, l, r, x, (typeof l)))
           | Error msg0 -> Error msg0)
        | Error msg0 -> Error msg0)
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val epostincrdecr : incr_or_decr -> expr -> expr res **)

let epostincrdecr id l =
  match check_lval l with
  | OK _ ->
    (match type_binop (match id with
                       | Incr -> Oadd
                       | Decr -> Osub) (typeof l) type_int32s with
     | OK _ ->
       (match check_cast (incrdecr_type (typeof l)) (typeof l) with
        | OK _ -> OK (Epostincr (id, l, (typeof l)))
        | Error msg0 -> Error msg0)
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val epostincr : expr -> expr res **)

let epostincr l =
  epostincrdecr Incr l

(** val epostdecr : expr -> expr res **)

let epostdecr l =
  epostincrdecr Decr l

(** val epreincr : expr -> expr res **)

let epreincr l =
  eassignop Oadd l (Eval ((Vint Int.one), type_int32s))

(** val epredecr : expr -> expr res **)

let epredecr l =
  eassignop Osub l (Eval ((Vint Int.one), type_int32s))

(** val ecomma : expr -> expr -> expr res **)

let ecomma r1 r2 =
  match check_rval r1 with
  | OK _ ->
    (match check_rval r2 with
     | OK _ -> OK (Ecomma (r1, r2, (typeof r2)))
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val ecall : expr -> exprlist -> expr res **)

let ecall fn args =
  match check_rval fn with
  | OK _ ->
    (match check_rvals args with
     | OK _ ->
       (match classify_fun (typeof fn) with
        | Coq_fun_case_f (tyargs, tyres, _) ->
          (match check_arguments args tyargs with
           | OK _ -> OK (Ecall (fn, args, tyres))
           | Error msg0 -> Error msg0)
        | Coq_fun_default ->
          Error
            (msg
              ('c'::('a'::('l'::('l'::(':'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val ebuiltin :
    external_function -> typelist -> exprlist -> coq_type -> expr res **)

let ebuiltin ef tyargs args tyres =
  match check_rvals args with
  | OK _ ->
    (match check_arguments args tyargs with
     | OK _ ->
       if (&&) ((fun x -> x) (type_eq tyres Tvoid))
            ((fun x -> x) (rettype_eq (ef_sig ef).sig_res AST.Tvoid))
       then OK (Ebuiltin (ef, tyargs, args, tyres))
       else Error
              (msg
                ('b'::('u'::('i'::('l'::('t'::('i'::('n'::(':'::(' '::('w'::('r'::('o'::('n'::('g'::(' '::('t'::('y'::('p'::('e'::(' '::('d'::('e'::('c'::('o'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val eselection : expr -> expr -> expr -> expr res **)

let eselection r1 r2 r3 =
  match check_rval r1 with
  | OK _ ->
    (match check_rval r2 with
     | OK _ ->
       (match check_rval r3 with
        | OK _ ->
          (match check_bool (typeof r1) with
           | OK _ ->
             (match type_conditional (typeof r2) (typeof r3) with
              | OK x -> OK (coq_Eselection r1 r2 r3 x)
              | Error msg0 -> Error msg0)
           | Error msg0 -> Error msg0)
        | Error msg0 -> Error msg0)
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val sdo : expr -> statement res **)

let sdo a =
  match check_rval a with
  | OK _ -> OK (Sdo a)
  | Error msg0 -> Error msg0

(** val sifthenelse : expr -> statement -> statement -> statement res **)

let sifthenelse a s1 s2 =
  match check_rval a with
  | OK _ ->
    (match check_bool (typeof a) with
     | OK _ -> OK (Sifthenelse (a, s1, s2))
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val swhile : expr -> statement -> statement res **)

let swhile a s =
  match check_rval a with
  | OK _ ->
    (match check_bool (typeof a) with
     | OK _ -> OK (Swhile (a, s))
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val sdowhile : expr -> statement -> statement res **)

let sdowhile a s =
  match check_rval a with
  | OK _ ->
    (match check_bool (typeof a) with
     | OK _ -> OK (Sdowhile (a, s))
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val sfor :
    statement -> expr -> statement -> statement -> statement res **)

let sfor s1 a s2 s3 =
  match check_rval a with
  | OK _ ->
    (match check_bool (typeof a) with
     | OK _ -> OK (Sfor (s1, a, s2, s3))
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val sreturn : coq_type -> expr -> statement res **)

let sreturn rt a =
  match check_rval a with
  | OK _ ->
    (match check_cast (typeof a) rt with
     | OK _ -> OK (Sreturn (Some a))
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0

(** val sswitch : expr -> labeled_statements -> statement res **)

let sswitch a sl =
  match check_rval a with
  | OK _ ->
    (match typeof a with
     | Tint (_, _, _) -> OK (Sswitch (a, sl))
     | Tlong (_, _) -> OK (Sswitch (a, sl))
     | _ ->
       Error
         (msg
           ('w'::('r'::('o'::('n'::('g'::(' '::('t'::('y'::('p'::('e'::(' '::('f'::('o'::('r'::(' '::('a'::('r'::('g'::('u'::('m'::('e'::('n'::('t'::(' '::('o'::('f'::(' '::('s'::('w'::('i'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))
  | Error msg0 -> Error msg0

(** val retype_expr : composite_env -> typenv -> expr -> expr res **)

let rec retype_expr ce e = function
| Eval (v, ty0) ->
  (match v with
   | Vint n ->
     (match ty0 with
      | Tint (_, sg, _) -> OK (econst_int n sg)
      | Tpointer (ty, _) -> OK (econst_ptr_int n ty)
      | _ ->
        Error
          (msg
            ('b'::('a'::('d'::(' '::('l'::('i'::('t'::('e'::('r'::('a'::('l'::[])))))))))))))
   | Vlong n ->
     (match ty0 with
      | Tlong (sg, _) -> OK (econst_long n sg)
      | _ ->
        Error
          (msg
            ('b'::('a'::('d'::(' '::('l'::('i'::('t'::('e'::('r'::('a'::('l'::[])))))))))))))
   | Vfloat n -> OK (econst_float n)
   | Vsingle n -> OK (econst_single n)
   | _ ->
     Error
       (msg
         ('b'::('a'::('d'::(' '::('l'::('i'::('t'::('e'::('r'::('a'::('l'::[])))))))))))))
| Evar (x, _) -> evar e x
| Efield (r, f, _) ->
  (match retype_expr ce e r with
   | OK x -> efield ce x f
   | Error msg0 -> Error msg0)
| Evalof (l, _) ->
  (match retype_expr ce e l with
   | OK x -> evalof x
   | Error msg0 -> Error msg0)
| Ederef (r, _) ->
  (match retype_expr ce e r with
   | OK x -> ederef x
   | Error msg0 -> Error msg0)
| Eaddrof (l, _) ->
  (match retype_expr ce e l with
   | OK x -> eaddrof x
   | Error msg0 -> Error msg0)
| Eunop (op, r, _) ->
  (match retype_expr ce e r with
   | OK x -> eunop op x
   | Error msg0 -> Error msg0)
| Ebinop (op, r1, r2, _) ->
  (match retype_expr ce e r1 with
   | OK x ->
     (match retype_expr ce e r2 with
      | OK x0 -> ebinop op x x0
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Ecast (r, ty) ->
  (match retype_expr ce e r with
   | OK x -> ecast ty x
   | Error msg0 -> Error msg0)
| Eseqand (r1, r2, _) ->
  (match retype_expr ce e r1 with
   | OK x ->
     (match retype_expr ce e r2 with
      | OK x0 -> eseqand x x0
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Eseqor (r1, r2, _) ->
  (match retype_expr ce e r1 with
   | OK x ->
     (match retype_expr ce e r2 with
      | OK x0 -> eseqor x x0
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Econdition (r1, r2, r3, _) ->
  (match retype_expr ce e r1 with
   | OK x ->
     (match retype_expr ce e r2 with
      | OK x0 ->
        (match retype_expr ce e r3 with
         | OK x1 -> econdition x x0 x1
         | Error msg0 -> Error msg0)
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Esizeof (ty, _) -> OK (esizeof ty)
| Ealignof (ty, _) -> OK (ealignof ty)
| Eassign (l, r, _) ->
  (match retype_expr ce e l with
   | OK x ->
     (match retype_expr ce e r with
      | OK x0 -> eassign x x0
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Eassignop (op, l, r, _, _) ->
  (match retype_expr ce e l with
   | OK x ->
     (match retype_expr ce e r with
      | OK x0 -> eassignop op x x0
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Epostincr (id, l, _) ->
  (match retype_expr ce e l with
   | OK x -> epostincrdecr id x
   | Error msg0 -> Error msg0)
| Ecomma (r1, r2, _) ->
  (match retype_expr ce e r1 with
   | OK x ->
     (match retype_expr ce e r2 with
      | OK x0 -> ecomma x x0
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Ecall (r1, rl, _) ->
  (match retype_expr ce e r1 with
   | OK x ->
     (match retype_exprlist ce e rl with
      | OK x0 -> ecall x x0
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Ebuiltin (ef, tyargs, rl, tyres) ->
  (match retype_exprlist ce e rl with
   | OK x -> ebuiltin ef tyargs x tyres
   | Error msg0 -> Error msg0)
| Eloc (_, _, _, _) ->
  Error
    (msg
      ('E'::('l'::('o'::('c'::(' '::('i'::('n'::(' '::('s'::('o'::('u'::('r'::('c'::('e'::[])))))))))))))))
| Eparen (_, _, _) ->
  Error
    (msg
      ('E'::('p'::('a'::('r'::('e'::('n'::(' '::('i'::('n'::(' '::('s'::('o'::('u'::('r'::('c'::('e'::[])))))))))))))))))

(** val retype_exprlist :
    composite_env -> typenv -> exprlist -> exprlist res **)

and retype_exprlist ce e = function
| Enil -> OK Enil
| Econs (a1, al0) ->
  (match retype_expr ce e a1 with
   | OK x ->
     (match retype_exprlist ce e al0 with
      | OK x0 ->
        (match check_rval x with
         | OK _ -> OK (Econs (x, x0))
         | Error msg0 -> Error msg0)
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)

(** val retype_stmt :
    composite_env -> typenv -> coq_type -> statement -> statement res **)

let rec retype_stmt ce e rt = function
| Sdo a ->
  (match retype_expr ce e a with
   | OK x -> sdo x
   | Error msg0 -> Error msg0)
| Ssequence (s1, s2) ->
  (match retype_stmt ce e rt s1 with
   | OK x ->
     (match retype_stmt ce e rt s2 with
      | OK x0 -> OK (Ssequence (x, x0))
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Sifthenelse (a, s1, s2) ->
  (match retype_expr ce e a with
   | OK x ->
     (match retype_stmt ce e rt s1 with
      | OK x0 ->
        (match retype_stmt ce e rt s2 with
         | OK x1 -> sifthenelse x x0 x1
         | Error msg0 -> Error msg0)
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Swhile (a, s0) ->
  (match retype_expr ce e a with
   | OK x ->
     (match retype_stmt ce e rt s0 with
      | OK x0 -> swhile x x0
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Sdowhile (a, s0) ->
  (match retype_expr ce e a with
   | OK x ->
     (match retype_stmt ce e rt s0 with
      | OK x0 -> sdowhile x x0
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Sfor (s1, a, s2, s3) ->
  (match retype_expr ce e a with
   | OK x ->
     (match retype_stmt ce e rt s1 with
      | OK x0 ->
        (match retype_stmt ce e rt s2 with
         | OK x1 ->
           (match retype_stmt ce e rt s3 with
            | OK x2 -> sfor x0 x x1 x2
            | Error msg0 -> Error msg0)
         | Error msg0 -> Error msg0)
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Sreturn o ->
  (match o with
   | Some a ->
     (match retype_expr ce e a with
      | OK x -> sreturn rt x
      | Error msg0 -> Error msg0)
   | None -> OK (Sreturn None))
| Sswitch (a, sl) ->
  (match retype_expr ce e a with
   | OK x ->
     (match retype_lblstmts ce e rt sl with
      | OK x0 -> sswitch x x0
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Slabel (lbl, s0) ->
  (match retype_stmt ce e rt s0 with
   | OK x -> OK (Slabel (lbl, x))
   | Error msg0 -> Error msg0)
| x -> OK x

(** val retype_lblstmts :
    composite_env -> typenv -> coq_type -> labeled_statements ->
    labeled_statements res **)

and retype_lblstmts ce e rt = function
| LSnil -> OK LSnil
| LScons (case, s, sl0) ->
  (match retype_stmt ce e rt s with
   | OK x ->
     (match retype_lblstmts ce e rt sl0 with
      | OK x0 -> OK (LScons (case, x, x0))
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)

(** val retype_function :
    composite_env -> typenv -> coq_function -> coq_function res **)

let retype_function ce e f =
  let e0 = bind_vars (bind_vars e f.fn_params) f.fn_vars in
  (match retype_stmt ce e0 f.fn_return f.fn_body with
   | OK x ->
     OK { fn_return = f.fn_return; fn_callconv = f.fn_callconv; fn_params =
       f.fn_params; fn_vars = f.fn_vars; fn_body = x }
   | Error msg0 -> Error msg0)

(** val retype_fundef :
    composite_env -> typenv -> Csyntax.fundef -> Csyntax.fundef res **)

let retype_fundef ce e fd = match fd with
| Internal f ->
  (match retype_function ce e f with
   | OK x -> OK (Internal x)
   | Error msg0 -> Error msg0)
| External (ef, _, res0, _) ->
  if rettype_eq (ef_sig ef).sig_res (rettype_of_type res0)
  then OK fd
  else assertion_failed

(** val typecheck_program : Csyntax.program -> Csyntax.program res **)

let typecheck_program p =
  let e = bind_globdef PTree.empty p.prog_defs in
  let ce = p.prog_comp_env in
  (match transform_partial_program (retype_fundef ce e) (program_of_program p) with
   | OK x ->
     OK { prog_defs = x.AST.prog_defs; prog_public = p.prog_public;
       prog_main = p.prog_main; prog_types = p.prog_types; prog_comp_env =
       ce }
   | Error msg0 -> Error msg0)
