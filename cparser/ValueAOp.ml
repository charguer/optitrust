open Archi
open BinNums
open Compopts
open Integers
open Op
open ValueDomain

(** val eval_static_condition : condition -> aval list -> abool **)

let eval_static_condition cond vl =
  match cond with
  | Ccomp c ->
    (match vl with
     | [] -> Bnone
     | v1 :: l ->
       (match l with
        | [] -> Bnone
        | v2 :: l0 -> (match l0 with
                       | [] -> cmp_bool c v1 v2
                       | _ :: _ -> Bnone)))
  | Ccompu c ->
    (match vl with
     | [] -> Bnone
     | v1 :: l ->
       (match l with
        | [] -> Bnone
        | v2 :: l0 ->
          (match l0 with
           | [] -> cmpu_bool c v1 v2
           | _ :: _ -> Bnone)))
  | Ccompimm (c, n) ->
    (match vl with
     | [] -> Bnone
     | v1 :: l -> (match l with
                   | [] -> cmp_bool c v1 (I n)
                   | _ :: _ -> Bnone))
  | Ccompuimm (c, n) ->
    (match vl with
     | [] -> Bnone
     | v1 :: l -> (match l with
                   | [] -> cmpu_bool c v1 (I n)
                   | _ :: _ -> Bnone))
  | Ccompl c ->
    (match vl with
     | [] -> Bnone
     | v1 :: l ->
       (match l with
        | [] -> Bnone
        | v2 :: l0 ->
          (match l0 with
           | [] -> cmpl_bool c v1 v2
           | _ :: _ -> Bnone)))
  | Ccomplu c ->
    (match vl with
     | [] -> Bnone
     | v1 :: l ->
       (match l with
        | [] -> Bnone
        | v2 :: l0 ->
          (match l0 with
           | [] -> cmplu_bool c v1 v2
           | _ :: _ -> Bnone)))
  | Ccomplimm (c, n) ->
    (match vl with
     | [] -> Bnone
     | v1 :: l -> (match l with
                   | [] -> cmpl_bool c v1 (L n)
                   | _ :: _ -> Bnone))
  | Ccompluimm (c, n) ->
    (match vl with
     | [] -> Bnone
     | v1 :: l -> (match l with
                   | [] -> cmplu_bool c v1 (L n)
                   | _ :: _ -> Bnone))
  | Ccompf c ->
    (match vl with
     | [] -> Bnone
     | v1 :: l ->
       (match l with
        | [] -> Bnone
        | v2 :: l0 ->
          (match l0 with
           | [] -> cmpf_bool c v1 v2
           | _ :: _ -> Bnone)))
  | Cnotcompf c ->
    (match vl with
     | [] -> Bnone
     | v1 :: l ->
       (match l with
        | [] -> Bnone
        | v2 :: l0 ->
          (match l0 with
           | [] -> cnot (cmpf_bool c v1 v2)
           | _ :: _ -> Bnone)))
  | Ccompfs c ->
    (match vl with
     | [] -> Bnone
     | v1 :: l ->
       (match l with
        | [] -> Bnone
        | v2 :: l0 ->
          (match l0 with
           | [] -> cmpfs_bool c v1 v2
           | _ :: _ -> Bnone)))
  | Cnotcompfs c ->
    (match vl with
     | [] -> Bnone
     | v1 :: l ->
       (match l with
        | [] -> Bnone
        | v2 :: l0 ->
          (match l0 with
           | [] -> cnot (cmpfs_bool c v1 v2)
           | _ :: _ -> Bnone)))
  | Cmaskzero n ->
    (match vl with
     | [] -> Bnone
     | v1 :: l -> (match l with
                   | [] -> maskzero v1 n
                   | _ :: _ -> Bnone))
  | Cmasknotzero n ->
    (match vl with
     | [] -> Bnone
     | v1 :: l -> (match l with
                   | [] -> cnot (maskzero v1 n)
                   | _ :: _ -> Bnone))

(** val eval_static_addressing_32 : addressing -> aval list -> aval **)

let eval_static_addressing_32 addr vl =
  match addr with
  | Aindexed n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> add v1 (I (Int.repr n))
        | _ :: _ -> Vbot))
  | Aindexed2 n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 ->
          (match l0 with
           | [] -> add (add v1 v2) (I (Int.repr n))
           | _ :: _ -> Vbot)))
  | Ascaled (sc, ofs) ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> add (mul v1 (I (Int.repr sc))) (I (Int.repr ofs))
        | _ :: _ -> Vbot))
  | Aindexed2scaled (sc, ofs) ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 ->
          (match l0 with
           | [] -> add v1 (add (mul v2 (I (Int.repr sc))) (I (Int.repr ofs)))
           | _ :: _ -> Vbot)))
  | Aglobal (s, ofs) ->
    (match vl with
     | [] -> Ptr (Gl (s, ofs))
     | _ :: _ -> Vbot)
  | Abased (s, ofs) ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> add (Ptr (Gl (s, ofs))) v1
        | _ :: _ -> Vbot))
  | Abasedscaled (sc, s, ofs) ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> add (Ptr (Gl (s, ofs))) (mul v1 (I (Int.repr sc)))
        | _ :: _ -> Vbot))
  | Ainstack ofs -> (match vl with
                     | [] -> Ptr (Stk ofs)
                     | _ :: _ -> Vbot)

(** val eval_static_addressing_64 : addressing -> aval list -> aval **)

let eval_static_addressing_64 addr vl =
  match addr with
  | Aindexed n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> addl v1 (L (Int64.repr n))
        | _ :: _ -> Vbot))
  | Aindexed2 n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 ->
          (match l0 with
           | [] -> addl (addl v1 v2) (L (Int64.repr n))
           | _ :: _ -> Vbot)))
  | Ascaled (sc, ofs) ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> addl (mull v1 (L (Int64.repr sc))) (L (Int64.repr ofs))
        | _ :: _ -> Vbot))
  | Aindexed2scaled (sc, ofs) ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 ->
          (match l0 with
           | [] ->
             addl v1 (addl (mull v2 (L (Int64.repr sc))) (L (Int64.repr ofs)))
           | _ :: _ -> Vbot)))
  | Aglobal (s, ofs) ->
    (match vl with
     | [] -> Ptr (Gl (s, ofs))
     | _ :: _ -> Vbot)
  | Abased (s, ofs) ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> addl (Ptr (Gl (s, ofs))) v1
        | _ :: _ -> Vbot))
  | Abasedscaled (sc, s, ofs) ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> addl (Ptr (Gl (s, ofs))) (mull v1 (L (Int64.repr sc)))
        | _ :: _ -> Vbot))
  | Ainstack ofs -> (match vl with
                     | [] -> Ptr (Stk ofs)
                     | _ :: _ -> Vbot)

(** val eval_static_addressing : addressing -> aval list -> aval **)

let eval_static_addressing addr vl =
  if ptr64
  then eval_static_addressing_64 addr vl
  else eval_static_addressing_32 addr vl

(** val eval_static_operation : operation -> aval list -> aval **)

let eval_static_operation op vl =
  match op with
  | Omove ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> v1
                   | _ :: _ -> Vbot))
  | Ointconst n -> (match vl with
                    | [] -> I n
                    | _ :: _ -> Vbot)
  | Olongconst n -> (match vl with
                     | [] -> L n
                     | _ :: _ -> Vbot)
  | Ofloatconst n ->
    (match vl with
     | [] -> if propagate_float_constants () then F n else ntop
     | _ :: _ -> Vbot)
  | Osingleconst n ->
    (match vl with
     | [] -> if propagate_float_constants () then FS n else ntop
     | _ :: _ -> Vbot)
  | Oindirectsymbol id ->
    (match vl with
     | [] -> Ifptr (Gl (id, Ptrofs.zero))
     | _ :: _ -> Vbot)
  | Ocast8signed ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> sign_ext (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))) v1
        | _ :: _ -> Vbot))
  | Ocast8unsigned ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> zero_ext (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))) v1
        | _ :: _ -> Vbot))
  | Ocast16signed ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> sign_ext (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))) v1
        | _ :: _ -> Vbot))
  | Ocast16unsigned ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> zero_ext (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))) v1
        | _ :: _ -> Vbot))
  | Oneg ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> neg v1
                   | _ :: _ -> Vbot))
  | Osub ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> sub v1 v2
                       | _ :: _ -> Vbot)))
  | Omul ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> mul v1 v2
                       | _ :: _ -> Vbot)))
  | Omulimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> mul v1 (I n)
                   | _ :: _ -> Vbot))
  | Omulhs ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> mulhs v1 v2
                       | _ :: _ -> Vbot)))
  | Omulhu ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> mulhu v1 v2
                       | _ :: _ -> Vbot)))
  | Odiv ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> divs v1 v2
                       | _ :: _ -> Vbot)))
  | Odivu ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> divu v1 v2
                       | _ :: _ -> Vbot)))
  | Omod ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> mods v1 v2
                       | _ :: _ -> Vbot)))
  | Omodu ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> modu v1 v2
                       | _ :: _ -> Vbot)))
  | Oand ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> coq_and v1 v2
                       | _ :: _ -> Vbot)))
  | Oandimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> coq_and v1 (I n)
                   | _ :: _ -> Vbot))
  | Oor ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> coq_or v1 v2
                       | _ :: _ -> Vbot)))
  | Oorimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> coq_or v1 (I n)
                   | _ :: _ -> Vbot))
  | Oxor ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> xor v1 v2
                       | _ :: _ -> Vbot)))
  | Oxorimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> xor v1 (I n)
                   | _ :: _ -> Vbot))
  | Onot ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> notint v1
                   | _ :: _ -> Vbot))
  | Oshl ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> shl v1 v2
                       | _ :: _ -> Vbot)))
  | Oshlimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> shl v1 (I n)
                   | _ :: _ -> Vbot))
  | Oshr ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> shr v1 v2
                       | _ :: _ -> Vbot)))
  | Oshrimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> shr v1 (I n)
                   | _ :: _ -> Vbot))
  | Oshrximm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> shrx v1 (I n)
                   | _ :: _ -> Vbot))
  | Oshru ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> shru v1 v2
                       | _ :: _ -> Vbot)))
  | Oshruimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> shru v1 (I n)
                   | _ :: _ -> Vbot))
  | Ororimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> ror v1 (I n)
                   | _ :: _ -> Vbot))
  | Oshldimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 ->
          (match l0 with
           | [] ->
             coq_or (shl v1 (I n)) (shru v2 (I (Int.sub Int.iwordsize n)))
           | _ :: _ -> Vbot)))
  | Olea addr -> eval_static_addressing_32 addr vl
  | Omakelong ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> longofwords v1 v2
                       | _ :: _ -> Vbot)))
  | Olowlong ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> loword v1
                   | _ :: _ -> Vbot))
  | Ohighlong ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> hiword v1
                   | _ :: _ -> Vbot))
  | Ocast32signed ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> longofint v1
                   | _ :: _ -> Vbot))
  | Ocast32unsigned ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> longofintu v1
                   | _ :: _ -> Vbot))
  | Onegl ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> negl v1
                   | _ :: _ -> Vbot))
  | Oaddlimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> addl v1 (L n)
                   | _ :: _ -> Vbot))
  | Osubl ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> subl v1 v2
                       | _ :: _ -> Vbot)))
  | Omull ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> mull v1 v2
                       | _ :: _ -> Vbot)))
  | Omullimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> mull v1 (L n)
                   | _ :: _ -> Vbot))
  | Omullhs ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> mullhs v1 v2
                       | _ :: _ -> Vbot)))
  | Omullhu ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> mullhu v1 v2
                       | _ :: _ -> Vbot)))
  | Odivl ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> divls v1 v2
                       | _ :: _ -> Vbot)))
  | Odivlu ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> divlu v1 v2
                       | _ :: _ -> Vbot)))
  | Omodl ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> modls v1 v2
                       | _ :: _ -> Vbot)))
  | Omodlu ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> modlu v1 v2
                       | _ :: _ -> Vbot)))
  | Oandl ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> andl v1 v2
                       | _ :: _ -> Vbot)))
  | Oandlimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> andl v1 (L n)
                   | _ :: _ -> Vbot))
  | Oorl ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> orl v1 v2
                       | _ :: _ -> Vbot)))
  | Oorlimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> orl v1 (L n)
                   | _ :: _ -> Vbot))
  | Oxorl ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> xorl v1 v2
                       | _ :: _ -> Vbot)))
  | Oxorlimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> xorl v1 (L n)
                   | _ :: _ -> Vbot))
  | Onotl ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> notl v1
                   | _ :: _ -> Vbot))
  | Oshll ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> shll v1 v2
                       | _ :: _ -> Vbot)))
  | Oshllimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> shll v1 (I n)
                   | _ :: _ -> Vbot))
  | Oshrl ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> shrl v1 v2
                       | _ :: _ -> Vbot)))
  | Oshrlimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> shrl v1 (I n)
                   | _ :: _ -> Vbot))
  | Oshrxlimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> shrxl v1 (I n)
                   | _ :: _ -> Vbot))
  | Oshrlu ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> shrlu v1 v2
                       | _ :: _ -> Vbot)))
  | Oshrluimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> shrlu v1 (I n)
                   | _ :: _ -> Vbot))
  | Ororlimm n ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> rorl v1 (I n)
                   | _ :: _ -> Vbot))
  | Oleal addr -> eval_static_addressing_64 addr vl
  | Onegf ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> negf v1
                   | _ :: _ -> Vbot))
  | Oabsf ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> absf v1
                   | _ :: _ -> Vbot))
  | Oaddf ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> addf v1 v2
                       | _ :: _ -> Vbot)))
  | Osubf ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> subf v1 v2
                       | _ :: _ -> Vbot)))
  | Omulf ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> mulf v1 v2
                       | _ :: _ -> Vbot)))
  | Odivf ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> divf v1 v2
                       | _ :: _ -> Vbot)))
  | Onegfs ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> negfs v1
                   | _ :: _ -> Vbot))
  | Oabsfs ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> absfs v1
                   | _ :: _ -> Vbot))
  | Oaddfs ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> addfs v1 v2
                       | _ :: _ -> Vbot)))
  | Osubfs ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> subfs v1 v2
                       | _ :: _ -> Vbot)))
  | Omulfs ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> mulfs v1 v2
                       | _ :: _ -> Vbot)))
  | Odivfs ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: l0 -> (match l0 with
                       | [] -> divfs v1 v2
                       | _ :: _ -> Vbot)))
  | Osingleoffloat ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> singleoffloat v1
                   | _ :: _ -> Vbot))
  | Ofloatofsingle ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> floatofsingle v1
                   | _ :: _ -> Vbot))
  | Ointoffloat ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> intoffloat v1
                   | _ :: _ -> Vbot))
  | Ofloatofint ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> floatofint v1
                   | _ :: _ -> Vbot))
  | Ointofsingle ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> intofsingle v1
                   | _ :: _ -> Vbot))
  | Osingleofint ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> singleofint v1
                   | _ :: _ -> Vbot))
  | Olongoffloat ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> longoffloat v1
                   | _ :: _ -> Vbot))
  | Ofloatoflong ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> floatoflong v1
                   | _ :: _ -> Vbot))
  | Olongofsingle ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> longofsingle v1
                   | _ :: _ -> Vbot))
  | Osingleoflong ->
    (match vl with
     | [] -> Vbot
     | v1 :: l -> (match l with
                   | [] -> singleoflong v1
                   | _ :: _ -> Vbot))
  | Ocmp c -> of_optbool (eval_static_condition c vl)
  | Osel (c, _) ->
    (match vl with
     | [] -> Vbot
     | v1 :: l ->
       (match l with
        | [] -> Vbot
        | v2 :: vl0 -> select (eval_static_condition c vl0) v1 v2))
