open AST
open Coqlib
open Datatypes
open Floats
open Integers
open List0
open Memdata
open String0
open Values

type __ = Obj.t

type builtin_sem =
  coq_val list -> coq_val option
  (* singleton inductive, whose constructor was mkbuiltin *)

(** val bs_sem : rettype -> builtin_sem -> coq_val list -> coq_val option **)

let bs_sem _ b =
  b

(** val mkbuiltin_v1t : rettype -> (coq_val -> coq_val) -> builtin_sem **)

let mkbuiltin_v1t _ f = function
| [] -> None
| v1 :: l -> (match l with
              | [] -> Some (f v1)
              | _ :: _ -> None)

(** val mkbuiltin_v2t :
    rettype -> (coq_val -> coq_val -> coq_val) -> builtin_sem **)

let mkbuiltin_v2t _ f = function
| [] -> None
| v1 :: l ->
  (match l with
   | [] -> None
   | v2 :: l0 -> (match l0 with
                  | [] -> Some (f v1 v2)
                  | _ :: _ -> None))

(** val mkbuiltin_v1p :
    rettype -> (coq_val -> coq_val option) -> builtin_sem **)

let mkbuiltin_v1p _ f = function
| [] -> None
| v1 :: l -> (match l with
              | [] -> f v1
              | _ :: _ -> None)

(** val mkbuiltin_v2p :
    rettype -> (coq_val -> coq_val -> coq_val option) -> builtin_sem **)

let mkbuiltin_v2p _ f = function
| [] -> None
| v1 :: l ->
  (match l with
   | [] -> None
   | v2 :: l0 -> (match l0 with
                  | [] -> f v1 v2
                  | _ :: _ -> None))

type valty = __

type valretty = __

(** val inj_num : rettype -> valretty -> coq_val **)

let inj_num = function
| Tret t0 ->
  (match t0 with
   | Tint -> Obj.magic (fun x -> Vint x)
   | Tfloat -> Obj.magic (fun x -> Vfloat x)
   | Tlong -> Obj.magic (fun x -> Vlong x)
   | Tsingle -> Obj.magic (fun x -> Vsingle x)
   | _ -> (fun _ -> Vundef))
| Tvoid -> (fun _ -> Vundef)
| _ -> (fun n -> Vint (Obj.magic n))

(** val proj_num : typ -> 'a1 -> coq_val -> (valty -> 'a1) -> 'a1 **)

let proj_num t k0 v k1 =
  match t with
  | Tint -> (match v with
             | Vint n -> Obj.magic k1 n
             | _ -> k0)
  | Tfloat -> (match v with
               | Vfloat n -> Obj.magic k1 n
               | _ -> k0)
  | Tlong -> (match v with
              | Vlong n -> Obj.magic k1 n
              | _ -> k0)
  | Tsingle -> (match v with
                | Vsingle n -> Obj.magic k1 n
                | _ -> k0)
  | _ -> k0

(** val mkbuiltin_n1t :
    typ -> rettype -> (valty -> valretty) -> builtin_sem **)

let mkbuiltin_n1t targ1 tres f =
  mkbuiltin_v1t tres (fun v1 ->
    proj_num targ1 Vundef v1 (fun x -> inj_num tres (f x)))

(** val mkbuiltin_n2t :
    typ -> typ -> rettype -> (valty -> valty -> valretty) -> builtin_sem **)

let mkbuiltin_n2t targ1 targ2 tres f =
  mkbuiltin_v2t tres (fun v1 v2 ->
    proj_num targ1 Vundef v1 (fun x1 ->
      proj_num targ2 Vundef v2 (fun x2 -> inj_num tres (f x1 x2))))

(** val mkbuiltin_n1p :
    typ -> rettype -> (valty -> valretty option) -> builtin_sem **)

let mkbuiltin_n1p targ1 tres f =
  mkbuiltin_v1p tres (fun v1 ->
    proj_num targ1 None v1 (fun x -> option_map (inj_num tres) (f x)))

(** val lookup_builtin :
    ('a1 -> signature) -> char list -> signature -> (char list * 'a1) list ->
    'a1 option **)

let rec lookup_builtin sig_of name sg = function
| [] -> None
| p :: l0 ->
  let (n, b) = p in
  if (&&) ((fun x -> x) (string_dec name n))
       ((fun x -> x) (signature_eq sg (sig_of b)))
  then Some b
  else lookup_builtin sig_of name sg l0

type standard_builtin =
| BI_select of typ
| BI_fabs
| BI_fabsf
| BI_fsqrt
| BI_negl
| BI_addl
| BI_subl
| BI_mull
| BI_i16_bswap
| BI_i32_bswap
| BI_i64_bswap
| BI_unreachable
| BI_i64_umulh
| BI_i64_smulh
| BI_i64_sdiv
| BI_i64_udiv
| BI_i64_smod
| BI_i64_umod
| BI_i64_shl
| BI_i64_shr
| BI_i64_sar
| BI_i64_dtos
| BI_i64_dtou
| BI_i64_stod
| BI_i64_utod
| BI_i64_stof
| BI_i64_utof

(** val standard_builtin_table : (char list * standard_builtin) list **)

let standard_builtin_table =
  (('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('s'::('e'::('l'::[]))))))))))))),
    (BI_select
    Tint)) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('s'::('e'::('l'::[]))))))))))))),
    (BI_select
    Tlong)) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('s'::('e'::('l'::[]))))))))))))),
    (BI_select
    Tfloat)) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('s'::('e'::('l'::[]))))))))))))),
    (BI_select
    Tsingle)) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('f'::('a'::('b'::('s'::[])))))))))))))),
    BI_fabs) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('f'::('a'::('b'::('s'::('f'::[]))))))))))))))),
    BI_fabsf) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('f'::('s'::('q'::('r'::('t'::[]))))))))))))))),
    BI_fsqrt) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('s'::('q'::('r'::('t'::[])))))))))))))),
    BI_fsqrt) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('n'::('e'::('g'::('l'::[])))))))))))))),
    BI_negl) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('a'::('d'::('d'::('l'::[])))))))))))))),
    BI_addl) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('s'::('u'::('b'::('l'::[])))))))))))))),
    BI_subl) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('m'::('u'::('l'::('l'::[])))))))))))))),
    BI_mull) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('b'::('s'::('w'::('a'::('p'::('1'::('6'::[]))))))))))))))))),
    BI_i16_bswap) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('b'::('s'::('w'::('a'::('p'::[]))))))))))))))),
    BI_i32_bswap) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('b'::('s'::('w'::('a'::('p'::('3'::('2'::[]))))))))))))))))),
    BI_i32_bswap) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('b'::('s'::('w'::('a'::('p'::('6'::('4'::[]))))))))))))))))),
    BI_i64_bswap) :: ((('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('u'::('n'::('r'::('e'::('a'::('c'::('h'::('a'::('b'::('l'::('e'::[]))))))))))))))))))))),
    BI_unreachable) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('u'::('m'::('u'::('l'::('h'::[])))))))))))))))))))),
    BI_i64_umulh) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('m'::('u'::('l'::('h'::[])))))))))))))))))))),
    BI_i64_smulh) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('d'::('i'::('v'::[]))))))))))))))))))),
    BI_i64_sdiv) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('u'::('d'::('i'::('v'::[]))))))))))))))))))),
    BI_i64_udiv) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('m'::('o'::('d'::[]))))))))))))))))))),
    BI_i64_smod) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('u'::('m'::('o'::('d'::[]))))))))))))))))))),
    BI_i64_umod) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('h'::('l'::[])))))))))))))))))),
    BI_i64_shl) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('h'::('r'::[])))))))))))))))))),
    BI_i64_shr) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('a'::('r'::[])))))))))))))))))),
    BI_i64_sar) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('d'::('t'::('o'::('s'::[]))))))))))))))))))),
    BI_i64_dtos) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('d'::('t'::('o'::('u'::[]))))))))))))))))))),
    BI_i64_dtou) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('t'::('o'::('d'::[]))))))))))))))))))),
    BI_i64_stod) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('u'::('t'::('o'::('d'::[]))))))))))))))))))),
    BI_i64_utod) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('t'::('o'::('f'::[]))))))))))))))))))),
    BI_i64_stof) :: ((('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('u'::('t'::('o'::('f'::[]))))))))))))))))))),
    BI_i64_utof) :: [])))))))))))))))))))))))))))))))

(** val standard_builtin_sig : standard_builtin -> signature **)

let standard_builtin_sig = function
| BI_select t ->
  { sig_args = (Tint :: (t :: (t :: []))); sig_res = (Tret t); sig_cc =
    cc_default }
| BI_fabs ->
  { sig_args = (Tfloat :: []); sig_res = (Tret Tfloat); sig_cc = cc_default }
| BI_fabsf ->
  { sig_args = (Tsingle :: []); sig_res = (Tret Tsingle); sig_cc =
    cc_default }
| BI_fsqrt ->
  { sig_args = (Tfloat :: []); sig_res = (Tret Tfloat); sig_cc = cc_default }
| BI_negl ->
  { sig_args = (Tlong :: []); sig_res = (Tret Tlong); sig_cc = cc_default }
| BI_mull ->
  { sig_args = (Tint :: (Tint :: [])); sig_res = (Tret Tlong); sig_cc =
    cc_default }
| BI_i16_bswap ->
  { sig_args = (Tint :: []); sig_res = (Tret Tint); sig_cc = cc_default }
| BI_i32_bswap ->
  { sig_args = (Tint :: []); sig_res = (Tret Tint); sig_cc = cc_default }
| BI_i64_bswap ->
  { sig_args = (Tlong :: []); sig_res = (Tret Tlong); sig_cc = cc_default }
| BI_unreachable -> { sig_args = []; sig_res = Tvoid; sig_cc = cc_default }
| BI_i64_shl ->
  { sig_args = (Tlong :: (Tint :: [])); sig_res = (Tret Tlong); sig_cc =
    cc_default }
| BI_i64_shr ->
  { sig_args = (Tlong :: (Tint :: [])); sig_res = (Tret Tlong); sig_cc =
    cc_default }
| BI_i64_sar ->
  { sig_args = (Tlong :: (Tint :: [])); sig_res = (Tret Tlong); sig_cc =
    cc_default }
| BI_i64_dtos ->
  { sig_args = (Tfloat :: []); sig_res = (Tret Tlong); sig_cc = cc_default }
| BI_i64_dtou ->
  { sig_args = (Tfloat :: []); sig_res = (Tret Tlong); sig_cc = cc_default }
| BI_i64_stod ->
  { sig_args = (Tlong :: []); sig_res = (Tret Tfloat); sig_cc = cc_default }
| BI_i64_utod ->
  { sig_args = (Tlong :: []); sig_res = (Tret Tfloat); sig_cc = cc_default }
| BI_i64_stof ->
  { sig_args = (Tlong :: []); sig_res = (Tret Tsingle); sig_cc = cc_default }
| BI_i64_utof ->
  { sig_args = (Tlong :: []); sig_res = (Tret Tsingle); sig_cc = cc_default }
| _ ->
  { sig_args = (Tlong :: (Tlong :: [])); sig_res = (Tret Tlong); sig_cc =
    cc_default }

(** val standard_builtin_sem : standard_builtin -> builtin_sem **)

let standard_builtin_sem = function
| BI_select t ->
  (fun vargs ->
    match vargs with
    | [] -> None
    | v :: l ->
      (match v with
       | Vint n ->
         (match l with
          | [] -> None
          | v1 :: l0 ->
            (match l0 with
             | [] -> None
             | v2 :: l1 ->
               (match l1 with
                | [] ->
                  Some
                    (Val.normalize (if Int.eq n Int.zero then v2 else v1) t)
                | _ :: _ -> None)))
       | _ -> None))
| BI_fabs -> mkbuiltin_n1t Tfloat (Tret Tfloat) (Obj.magic Float.abs)
| BI_fabsf -> mkbuiltin_n1t Tsingle (Tret Tsingle) (Obj.magic Float32.abs)
| BI_fsqrt -> mkbuiltin_n1t Tfloat (Tret Tfloat) (Obj.magic Float.sqrt)
| BI_negl -> mkbuiltin_n1t Tlong (Tret Tlong) (Obj.magic Int64.neg)
| BI_addl -> mkbuiltin_v2t (Tret Tlong) Val.addl
| BI_subl -> mkbuiltin_v2t (Tret Tlong) Val.subl
| BI_mull -> mkbuiltin_v2t (Tret Tlong) Val.mull'
| BI_i16_bswap ->
  mkbuiltin_n1t Tint (Tret Tint) (fun n ->
    Obj.magic Int.repr
      (decode_int (rev (encode_int (S (S O)) (Int.unsigned (Obj.magic n))))))
| BI_i32_bswap ->
  mkbuiltin_n1t Tint (Tret Tint) (fun n ->
    Obj.magic Int.repr
      (decode_int
        (rev (encode_int (S (S (S (S O)))) (Int.unsigned (Obj.magic n))))))
| BI_i64_bswap ->
  mkbuiltin_n1t Tlong (Tret Tlong) (fun n ->
    Obj.magic Int64.repr
      (decode_int
        (rev
          (encode_int (S (S (S (S (S (S (S (S O))))))))
            (Int64.unsigned (Obj.magic n))))))
| BI_unreachable -> (fun _ -> None)
| BI_i64_umulh ->
  mkbuiltin_n2t Tlong Tlong (Tret Tlong) (Obj.magic Int64.mulhu)
| BI_i64_smulh ->
  mkbuiltin_n2t Tlong Tlong (Tret Tlong) (Obj.magic Int64.mulhs)
| BI_i64_sdiv -> mkbuiltin_v2p (Tret Tlong) Val.divls
| BI_i64_udiv -> mkbuiltin_v2p (Tret Tlong) Val.divlu
| BI_i64_smod -> mkbuiltin_v2p (Tret Tlong) Val.modls
| BI_i64_umod -> mkbuiltin_v2p (Tret Tlong) Val.modlu
| BI_i64_shl -> mkbuiltin_v2t (Tret Tlong) Val.shll
| BI_i64_shr -> mkbuiltin_v2t (Tret Tlong) Val.shrlu
| BI_i64_sar -> mkbuiltin_v2t (Tret Tlong) Val.shrl
| BI_i64_dtos -> mkbuiltin_n1p Tfloat (Tret Tlong) (Obj.magic Float.to_long)
| BI_i64_dtou -> mkbuiltin_n1p Tfloat (Tret Tlong) (Obj.magic Float.to_longu)
| BI_i64_stod -> mkbuiltin_n1t Tlong (Tret Tfloat) (Obj.magic Float.of_long)
| BI_i64_utod -> mkbuiltin_n1t Tlong (Tret Tfloat) (Obj.magic Float.of_longu)
| BI_i64_stof ->
  mkbuiltin_n1t Tlong (Tret Tsingle) (Obj.magic Float32.of_long)
| BI_i64_utof ->
  mkbuiltin_n1t Tlong (Tret Tsingle) (Obj.magic Float32.of_longu)
