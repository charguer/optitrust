open AST
open Archi
open BinInt
open BinNat
open BinNums
open Bool
open Coqlib
open Datatypes
open Errors
open Maps
open Nat
open Zpower

type signedness =
| Signed
| Unsigned

type intsize =
| I8
| I16
| I32
| IBool

type floatsize =
| F32
| F64

type attr = { attr_volatile : bool; attr_alignas : coq_N option }

(** val noattr : attr **)

let noattr =
  { attr_volatile = false; attr_alignas = None }

type coq_type =
| Tvoid
| Tint of intsize * signedness * attr
| Tlong of signedness * attr
| Tfloat of floatsize * attr
| Tpointer of coq_type * attr
| Tarray of coq_type * coq_Z * attr
| Tfunction of typelist * coq_type * calling_convention
| Tstruct of ident * attr
| Tunion of ident * attr
and typelist =
| Tnil
| Tcons of coq_type * typelist

(** val intsize_eq : intsize -> intsize -> bool **)

let intsize_eq s1 s2 =
  match s1 with
  | I8 -> (match s2 with
           | I8 -> true
           | _ -> false)
  | I16 -> (match s2 with
            | I16 -> true
            | _ -> false)
  | I32 -> (match s2 with
            | I32 -> true
            | _ -> false)
  | IBool -> (match s2 with
              | IBool -> true
              | _ -> false)

(** val signedness_eq : signedness -> signedness -> bool **)

let signedness_eq s1 s2 =
  match s1 with
  | Signed -> (match s2 with
               | Signed -> true
               | Unsigned -> false)
  | Unsigned -> (match s2 with
                 | Signed -> false
                 | Unsigned -> true)

(** val attr_eq : attr -> attr -> bool **)

let attr_eq a1 a2 =
  let { attr_volatile = attr_volatile0; attr_alignas = attr_alignas0 } = a1 in
  let { attr_volatile = attr_volatile1; attr_alignas = attr_alignas1 } = a2 in
  if bool_dec attr_volatile0 attr_volatile1
  then (match attr_alignas0 with
        | Some x ->
          (match attr_alignas1 with
           | Some n -> N.eq_dec x n
           | None -> false)
        | None -> (match attr_alignas1 with
                   | Some _ -> false
                   | None -> true))
  else false

(** val type_eq : coq_type -> coq_type -> bool **)

let rec type_eq =
  let h = fun x y ->
    match x with
    | F32 -> (match y with
              | F32 -> true
              | F64 -> false)
    | F64 -> (match y with
              | F32 -> false
              | F64 -> true)
  in
  (fun ty1 ty2 ->
  match ty1 with
  | Tvoid -> (match ty2 with
              | Tvoid -> true
              | _ -> false)
  | Tint (i, s, a) ->
    (match ty2 with
     | Tint (i0, s0, a0) ->
       if intsize_eq i i0
       then if signedness_eq s s0 then attr_eq a a0 else false
       else false
     | _ -> false)
  | Tlong (s, a) ->
    (match ty2 with
     | Tlong (s0, a0) -> if signedness_eq s s0 then attr_eq a a0 else false
     | _ -> false)
  | Tfloat (f, a) ->
    (match ty2 with
     | Tfloat (f0, a0) -> if h f f0 then attr_eq a a0 else false
     | _ -> false)
  | Tpointer (t0, a) ->
    (match ty2 with
     | Tpointer (t1, a0) -> if type_eq t0 t1 then attr_eq a a0 else false
     | _ -> false)
  | Tarray (t0, z, a) ->
    (match ty2 with
     | Tarray (t1, z0, a0) ->
       if type_eq t0 t1
       then if zeq z z0 then attr_eq a a0 else false
       else false
     | _ -> false)
  | Tfunction (t0, t1, c) ->
    (match ty2 with
     | Tfunction (t2, t3, c0) ->
       if typelist_eq t0 t2
       then if type_eq t1 t3
            then let { cc_vararg = cc_vararg0; cc_unproto = cc_unproto0;
                   cc_structret = cc_structret0 } = c
                 in
                 let { cc_vararg = cc_vararg1; cc_unproto = cc_unproto1;
                   cc_structret = cc_structret1 } = c0
                 in
                 if match cc_vararg0 with
                    | Some x ->
                      (match cc_vararg1 with
                       | Some z -> zeq x z
                       | None -> false)
                    | None ->
                      (match cc_vararg1 with
                       | Some _ -> false
                       | None -> true)
                 then if bool_dec cc_unproto0 cc_unproto1
                      then bool_dec cc_structret0 cc_structret1
                      else false
                 else false
            else false
       else false
     | _ -> false)
  | Tstruct (i, a) ->
    (match ty2 with
     | Tstruct (i0, a0) -> if ident_eq i i0 then attr_eq a a0 else false
     | _ -> false)
  | Tunion (i, a) ->
    (match ty2 with
     | Tunion (i0, a0) -> if ident_eq i i0 then attr_eq a a0 else false
     | _ -> false))

(** val typelist_eq : typelist -> typelist -> bool **)

and typelist_eq tyl1 tyl2 =
  match tyl1 with
  | Tnil -> (match tyl2 with
             | Tnil -> true
             | Tcons (_, _) -> false)
  | Tcons (t0, t1) ->
    (match tyl2 with
     | Tnil -> false
     | Tcons (t2, t3) -> if type_eq t0 t2 then typelist_eq t1 t3 else false)

(** val attr_of_type : coq_type -> attr **)

let attr_of_type = function
| Tint (_, _, a) -> a
| Tlong (_, a) -> a
| Tfloat (_, a) -> a
| Tpointer (_, a) -> a
| Tarray (_, _, a) -> a
| Tstruct (_, a) -> a
| Tunion (_, a) -> a
| _ -> noattr

(** val change_attributes : (attr -> attr) -> coq_type -> coq_type **)

let change_attributes f ty = match ty with
| Tint (sz, si, a) -> Tint (sz, si, (f a))
| Tlong (si, a) -> Tlong (si, (f a))
| Tfloat (sz, a) -> Tfloat (sz, (f a))
| Tpointer (elt, a) -> Tpointer (elt, (f a))
| Tarray (elt, sz, a) -> Tarray (elt, sz, (f a))
| Tstruct (id, a) -> Tstruct (id, (f a))
| Tunion (id, a) -> Tunion (id, (f a))
| _ -> ty

(** val remove_attributes : coq_type -> coq_type **)

let remove_attributes ty =
  change_attributes (fun _ -> noattr) ty

(** val attr_union : attr -> attr -> attr **)

let attr_union a1 a2 =
  { attr_volatile = ((||) a1.attr_volatile a2.attr_volatile); attr_alignas =
    (match a1.attr_alignas with
     | Some n1 ->
       (match a2.attr_alignas with
        | Some n2 -> Some (N.max n1 n2)
        | None -> Some n1)
     | None -> a2.attr_alignas) }

(** val merge_attributes : coq_type -> attr -> coq_type **)

let merge_attributes ty a =
  change_attributes (attr_union a) ty

(** val bitsize_intsize : intsize -> coq_Z **)

let bitsize_intsize = function
| I8 -> Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| I16 -> Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
| I32 -> Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
| IBool -> Zpos Coq_xH

type struct_or_union =
| Struct
| Union

type member =
| Member_plain of ident * coq_type
| Member_bitfield of ident * intsize * signedness * attr * coq_Z * bool

type members = member list

type composite_definition =
| Composite of ident * struct_or_union * members * attr

(** val name_member : member -> ident **)

let name_member = function
| Member_plain (id, _) -> id
| Member_bitfield (id, _, _, _, _, _) -> id

(** val type_member : member -> coq_type **)

let type_member = function
| Member_plain (_, t0) -> t0
| Member_bitfield (_, sz, sg, a, w, _) ->
  let sg' = if zlt w (bitsize_intsize sz) then Signed else sg in
  Tint (sz, sg', a)

(** val member_is_padding : member -> bool **)

let member_is_padding = function
| Member_plain (_, _) -> false
| Member_bitfield (_, _, _, _, _, p) -> p

type composite = { co_su : struct_or_union; co_members : members;
                   co_attr : attr; co_sizeof : coq_Z; co_alignof : coq_Z;
                   co_rank : nat }

type composite_env = composite PTree.t

type bitfield =
| Full
| Bits of intsize * signedness * coq_Z * coq_Z

(** val type_int32s : coq_type **)

let type_int32s =
  Tint (I32, Signed, noattr)

(** val type_bool : coq_type **)

let type_bool =
  Tint (IBool, Signed, noattr)

(** val typeconv : coq_type -> coq_type **)

let typeconv ty = match ty with
| Tint (i, _, _) ->
  (match i with
   | I32 -> remove_attributes ty
   | _ -> Tint (I32, Signed, noattr))
| Tarray (t0, _, _) -> Tpointer (t0, noattr)
| Tfunction (_, _, _) -> Tpointer (ty, noattr)
| _ -> remove_attributes ty

(** val default_argument_conversion : coq_type -> coq_type **)

let default_argument_conversion ty = match ty with
| Tint (i, _, _) ->
  (match i with
   | I32 -> remove_attributes ty
   | _ -> Tint (I32, Signed, noattr))
| Tfloat (_, _) -> Tfloat (F64, noattr)
| Tarray (t0, _, _) -> Tpointer (t0, noattr)
| Tfunction (_, _, _) -> Tpointer (ty, noattr)
| _ -> remove_attributes ty

(** val complete_type : composite_env -> coq_type -> bool **)

let rec complete_type env = function
| Tvoid -> false
| Tarray (t', _, _) -> complete_type env t'
| Tfunction (_, _, _) -> false
| Tstruct (id, _) ->
  (match PTree.get id env with
   | Some _ -> true
   | None -> false)
| Tunion (id, _) ->
  (match PTree.get id env with
   | Some _ -> true
   | None -> false)
| _ -> true

(** val align_attr : attr -> coq_Z -> coq_Z **)

let align_attr a al =
  match a.attr_alignas with
  | Some l -> two_p (Z.of_N l)
  | None -> al

(** val alignof : composite_env -> coq_type -> coq_Z **)

let rec alignof env t0 =
  align_attr (attr_of_type t0)
    (match t0 with
     | Tint (i, _, _) ->
       (match i with
        | I16 -> Zpos (Coq_xO Coq_xH)
        | I32 -> Zpos (Coq_xO (Coq_xO Coq_xH))
        | _ -> Zpos Coq_xH)
     | Tlong (_, _) -> align_int64
     | Tfloat (f, _) ->
       (match f with
        | F32 -> Zpos (Coq_xO (Coq_xO Coq_xH))
        | F64 -> align_float64)
     | Tpointer (_, _) ->
       if ptr64
       then Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
       else Zpos (Coq_xO (Coq_xO Coq_xH))
     | Tarray (t', _, _) -> alignof env t'
     | Tstruct (id, _) ->
       (match PTree.get id env with
        | Some co -> co.co_alignof
        | None -> Zpos Coq_xH)
     | Tunion (id, _) ->
       (match PTree.get id env with
        | Some co -> co.co_alignof
        | None -> Zpos Coq_xH)
     | _ -> Zpos Coq_xH)

(** val sizeof : composite_env -> coq_type -> coq_Z **)

let rec sizeof env = function
| Tint (i, _, _) ->
  (match i with
   | I16 -> Zpos (Coq_xO Coq_xH)
   | I32 -> Zpos (Coq_xO (Coq_xO Coq_xH))
   | _ -> Zpos Coq_xH)
| Tlong (_, _) -> Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| Tfloat (f, _) ->
  (match f with
   | F32 -> Zpos (Coq_xO (Coq_xO Coq_xH))
   | F64 -> Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
| Tpointer (_, _) ->
  if ptr64
  then Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
  else Zpos (Coq_xO (Coq_xO Coq_xH))
| Tarray (t', n, _) -> Z.mul (sizeof env t') (Z.max Z0 n)
| Tstruct (id, _) ->
  (match PTree.get id env with
   | Some co -> co.co_sizeof
   | None -> Z0)
| Tunion (id, _) ->
  (match PTree.get id env with
   | Some co -> co.co_sizeof
   | None -> Z0)
| _ -> Zpos Coq_xH

(** val bitalignof : composite_env -> coq_type -> coq_Z **)

let bitalignof env t0 =
  Z.mul (alignof env t0) (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))

(** val bitsizeof : composite_env -> coq_type -> coq_Z **)

let bitsizeof env t0 =
  Z.mul (sizeof env t0) (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))

(** val bitalignof_intsize : intsize -> coq_Z **)

let bitalignof_intsize = function
| I16 -> Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
| I32 -> Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
| _ -> Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))

(** val next_field : composite_env -> coq_Z -> member -> coq_Z **)

let next_field env pos = function
| Member_plain (_, t0) ->
  Z.add (align pos (bitalignof env t0)) (bitsizeof env t0)
| Member_bitfield (_, sz, _, _, w, _) ->
  let s = bitalignof_intsize sz in
  if zle w Z0
  then align pos s
  else let curr = floor pos s in
       let next = Z.add curr s in
       if zle (Z.add pos w) next then Z.add pos w else Z.add next w

(** val layout_field :
    composite_env -> coq_Z -> member -> (coq_Z * bitfield) res **)

let layout_field env pos = function
| Member_plain (_, t0) ->
  OK
    ((Z.div (align pos (bitalignof env t0)) (Zpos (Coq_xO (Coq_xO (Coq_xO
       Coq_xH))))), Full)
| Member_bitfield (_, sz, sg, _, w, _) ->
  if zle w Z0
  then Error
         (msg
           ('a'::('c'::('c'::('e'::('s'::('s'::('i'::('n'::('g'::(' '::('z'::('e'::('r'::('o'::('-'::('w'::('i'::('d'::('t'::('h'::(' '::('b'::('i'::('t'::('f'::('i'::('e'::('l'::('d'::[]))))))))))))))))))))))))))))))
  else if zlt (bitsize_intsize sz) w
       then Error
              (msg
                ('b'::('i'::('t'::('f'::('i'::('e'::('l'::('d'::(' '::('t'::('o'::('o'::(' '::('w'::('i'::('d'::('e'::[]))))))))))))))))))
       else let s = bitalignof_intsize sz in
            let start = floor pos s in
            let next = Z.add start s in
            if zle (Z.add pos w) next
            then OK ((Z.div start (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))),
                   (Bits (sz, sg, (Z.sub pos start), w)))
            else OK ((Z.div next (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))),
                   (Bits (sz, sg, Z0, w)))

(** val alignof_composite : composite_env -> members -> coq_Z **)

let rec alignof_composite env = function
| [] -> Zpos Coq_xH
| m :: ms0 ->
  if member_is_padding m
  then alignof_composite env ms0
  else Z.max (alignof env (type_member m)) (alignof_composite env ms0)

(** val bitsizeof_struct : composite_env -> coq_Z -> members -> coq_Z **)

let rec bitsizeof_struct env cur = function
| [] -> cur
| m :: ms0 -> bitsizeof_struct env (next_field env cur m) ms0

(** val bytes_of_bits : coq_Z -> coq_Z **)

let bytes_of_bits n =
  Z.div (Z.add n (Zpos (Coq_xI (Coq_xI Coq_xH)))) (Zpos (Coq_xO (Coq_xO
    (Coq_xO Coq_xH))))

(** val sizeof_struct : composite_env -> members -> coq_Z **)

let sizeof_struct env m =
  bytes_of_bits (bitsizeof_struct env Z0 m)

(** val sizeof_union : composite_env -> members -> coq_Z **)

let rec sizeof_union env = function
| [] -> Z0
| m :: ms0 -> Z.max (sizeof env (type_member m)) (sizeof_union env ms0)

(** val field_type : ident -> members -> coq_type res **)

let rec field_type id = function
| [] ->
  Error ((MSG
    ('U'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('f'::('i'::('e'::('l'::('d'::(' '::[]))))))))))))))) :: ((CTX
    id) :: []))
| m :: ms0 ->
  if ident_eq id (name_member m)
  then OK (type_member m)
  else field_type id ms0

(** val field_offset_rec :
    composite_env -> ident -> members -> coq_Z -> (coq_Z * bitfield) res **)

let rec field_offset_rec env id ms pos =
  match ms with
  | [] ->
    Error ((MSG
      ('U'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('f'::('i'::('e'::('l'::('d'::(' '::[]))))))))))))))) :: ((CTX
      id) :: []))
  | m :: ms0 ->
    if ident_eq id (name_member m)
    then layout_field env pos m
    else field_offset_rec env id ms0 (next_field env pos m)

(** val field_offset :
    composite_env -> ident -> members -> (coq_Z * bitfield) res **)

let field_offset env id ms =
  field_offset_rec env id ms Z0

(** val union_field_offset :
    composite_env -> ident -> members -> (coq_Z * bitfield) res **)

let rec union_field_offset env id = function
| [] ->
  Error ((MSG
    ('U'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('f'::('i'::('e'::('l'::('d'::(' '::[]))))))))))))))) :: ((CTX
    id) :: []))
| m :: ms0 ->
  if ident_eq id (name_member m)
  then layout_field env Z0 m
  else union_field_offset env id ms0

type mode =
| By_value of memory_chunk
| By_reference
| By_copy
| By_nothing

(** val access_mode : coq_type -> mode **)

let access_mode = function
| Tvoid -> By_nothing
| Tint (i, s, _) ->
  (match i with
   | I8 ->
     (match s with
      | Signed -> By_value Mint8signed
      | Unsigned -> By_value Mint8unsigned)
   | I16 ->
     (match s with
      | Signed -> By_value Mint16signed
      | Unsigned -> By_value Mint16unsigned)
   | I32 -> By_value Mint32
   | IBool -> By_value Mint8unsigned)
| Tlong (_, _) -> By_value Mint64
| Tfloat (f, _) ->
  (match f with
   | F32 -> By_value Mfloat32
   | F64 -> By_value Mfloat64)
| Tpointer (_, _) -> By_value coq_Mptr
| Tarray (_, _, _) -> By_reference
| Tfunction (_, _, _) -> By_reference
| _ -> By_copy

(** val type_is_volatile : coq_type -> bool **)

let type_is_volatile ty =
  match access_mode ty with
  | By_value _ -> (attr_of_type ty).attr_volatile
  | _ -> false

(** val alignof_blockcopy : composite_env -> coq_type -> coq_Z **)

let rec alignof_blockcopy env = function
| Tint (i, _, _) ->
  (match i with
   | I16 -> Zpos (Coq_xO Coq_xH)
   | I32 -> Zpos (Coq_xO (Coq_xO Coq_xH))
   | _ -> Zpos Coq_xH)
| Tlong (_, _) -> Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| Tfloat (f, _) ->
  (match f with
   | F32 -> Zpos (Coq_xO (Coq_xO Coq_xH))
   | F64 -> Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
| Tpointer (_, _) ->
  if ptr64
  then Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
  else Zpos (Coq_xO (Coq_xO Coq_xH))
| Tarray (t', _, _) -> alignof_blockcopy env t'
| Tstruct (id, _) ->
  (match PTree.get id env with
   | Some co -> Z.min (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))) co.co_alignof
   | None -> Zpos Coq_xH)
| Tunion (id, _) ->
  (match PTree.get id env with
   | Some co -> Z.min (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))) co.co_alignof
   | None -> Zpos Coq_xH)
| _ -> Zpos Coq_xH

(** val rank_type : composite_env -> coq_type -> nat **)

let rec rank_type ce = function
| Tarray (t', _, _) -> S (rank_type ce t')
| Tstruct (id, _) ->
  (match PTree.get id ce with
   | Some co -> S co.co_rank
   | None -> O)
| Tunion (id, _) ->
  (match PTree.get id ce with
   | Some co -> S co.co_rank
   | None -> O)
| _ -> O

(** val rank_members : composite_env -> members -> nat **)

let rec rank_members ce = function
| [] -> O
| m0 :: m1 ->
  (match m0 with
   | Member_plain (_, t0) -> max (rank_type ce t0) (rank_members ce m1)
   | Member_bitfield (_, _, _, _, _, _) -> rank_members ce m1)

(** val type_of_params : (ident * coq_type) list -> typelist **)

let rec type_of_params = function
| [] -> Tnil
| p :: rem -> let (_, ty) = p in Tcons (ty, (type_of_params rem))

(** val typ_of_type : coq_type -> typ **)

let typ_of_type = function
| Tvoid -> AST.Tint
| Tint (_, _, _) -> AST.Tint
| Tlong (_, _) -> AST.Tlong
| Tfloat (f, _) -> (match f with
                    | F32 -> Tsingle
                    | F64 -> AST.Tfloat)
| _ -> coq_Tptr

(** val rettype_of_type : coq_type -> rettype **)

let rettype_of_type = function
| Tint (i, s, _) ->
  (match i with
   | I8 -> (match s with
            | Signed -> Tint8signed
            | Unsigned -> Tint8unsigned)
   | I16 -> (match s with
             | Signed -> Tint16signed
             | Unsigned -> Tint16unsigned)
   | I32 -> Tret AST.Tint
   | IBool -> Tint8unsigned)
| Tlong (_, _) -> Tret AST.Tlong
| Tfloat (f, _) -> (match f with
                    | F32 -> Tret Tsingle
                    | F64 -> Tret AST.Tfloat)
| Tpointer (_, _) -> Tret coq_Tptr
| _ -> AST.Tvoid

(** val typlist_of_typelist : typelist -> typ list **)

let rec typlist_of_typelist = function
| Tnil -> []
| Tcons (hd, tl0) -> (typ_of_type hd) :: (typlist_of_typelist tl0)

(** val signature_of_type :
    typelist -> coq_type -> calling_convention -> signature **)

let signature_of_type args res0 cc =
  { sig_args = (typlist_of_typelist args); sig_res = (rettype_of_type res0);
    sig_cc = cc }

(** val sizeof_composite :
    composite_env -> struct_or_union -> members -> coq_Z **)

let sizeof_composite env su m =
  match su with
  | Struct -> sizeof_struct env m
  | Union -> sizeof_union env m

(** val complete_members : composite_env -> members -> bool **)

let rec complete_members env = function
| [] -> true
| m :: ms0 ->
  (&&) (complete_type env (type_member m)) (complete_members env ms0)

(** val composite_of_def :
    composite_env -> ident -> struct_or_union -> members -> attr -> composite
    res **)

let composite_of_def env id su m a =
  match PTree.get id env with
  | Some _ ->
    Error ((MSG
      ('M'::('u'::('l'::('t'::('i'::('p'::('l'::('e'::(' '::('d'::('e'::('f'::('i'::('n'::('i'::('t'::('i'::('o'::('n'::('s'::(' '::('o'::('f'::(' '::('s'::('t'::('r'::('u'::('c'::('t'::(' '::('o'::('r'::(' '::('u'::('n'::('i'::('o'::('n'::(' '::[]))))))))))))))))))))))))))))))))))))))))) :: ((CTX
      id) :: []))
  | None ->
    if complete_members env m
    then let al = align_attr a (alignof_composite env m) in
         OK { co_su = su; co_members = m; co_attr = a; co_sizeof =
         (align (sizeof_composite env su m) al); co_alignof = al; co_rank =
         (rank_members env m) }
    else Error ((MSG
           ('I'::('n'::('c'::('o'::('m'::('p'::('l'::('e'::('t'::('e'::(' '::('s'::('t'::('r'::('u'::('c'::('t'::(' '::('o'::('r'::(' '::('u'::('n'::('i'::('o'::('n'::(' '::[])))))))))))))))))))))))))))) :: ((CTX
           id) :: []))

(** val add_composite_definitions :
    composite_env -> composite_definition list -> composite_env res **)

let rec add_composite_definitions env = function
| [] -> OK env
| c :: defs0 ->
  let Composite (id, su, m, a) = c in
  (match composite_of_def env id su m a with
   | OK x -> add_composite_definitions (PTree.set id x env) defs0
   | Error msg0 -> Error msg0)

(** val build_composite_env :
    composite_definition list -> composite_env res **)

let build_composite_env defs =
  add_composite_definitions PTree.empty defs

type 'f fundef =
| Internal of 'f
| External of external_function * typelist * coq_type * calling_convention

type 'f program = { prog_defs : (ident * ('f fundef, coq_type) globdef) list;
                    prog_public : ident list; prog_main : ident;
                    prog_types : composite_definition list;
                    prog_comp_env : composite_env }

(** val program_of_program :
    'a1 program -> ('a1 fundef, coq_type) AST.program **)

let program_of_program p =
  { AST.prog_defs = p.prog_defs; AST.prog_public = p.prog_public;
    AST.prog_main = p.prog_main }

(** val make_program :
    composite_definition list -> (ident * ('a1 fundef, coq_type) globdef)
    list -> ident list -> ident -> 'a1 program res **)

let make_program types defs public main =
  let filtered_var = build_composite_env types in
  (match filtered_var with
   | OK ce ->
     OK { prog_defs = defs; prog_public = public; prog_main = main;
       prog_types = types; prog_comp_env = ce }
   | Error e -> Error e)
