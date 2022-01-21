open AST
open Archi
open BinInt
open BinNums
open BoolEqual
open Coqlib
open Datatypes
open Floats
open Integers

type condition =
| Ccomp of comparison
| Ccompu of comparison
| Ccompimm of comparison * Int.int
| Ccompuimm of comparison * Int.int
| Ccompl of comparison
| Ccomplu of comparison
| Ccomplimm of comparison * Int64.int
| Ccompluimm of comparison * Int64.int
| Ccompf of comparison
| Cnotcompf of comparison
| Ccompfs of comparison
| Cnotcompfs of comparison
| Cmaskzero of Int.int
| Cmasknotzero of Int.int

type addressing =
| Aindexed of coq_Z
| Aindexed2 of coq_Z
| Ascaled of coq_Z * coq_Z
| Aindexed2scaled of coq_Z * coq_Z
| Aglobal of ident * Ptrofs.int
| Abased of ident * Ptrofs.int
| Abasedscaled of coq_Z * ident * Ptrofs.int
| Ainstack of Ptrofs.int

type operation =
| Omove
| Ointconst of Int.int
| Olongconst of Int64.int
| Ofloatconst of float
| Osingleconst of float32
| Oindirectsymbol of ident
| Ocast8signed
| Ocast8unsigned
| Ocast16signed
| Ocast16unsigned
| Oneg
| Osub
| Omul
| Omulimm of Int.int
| Omulhs
| Omulhu
| Odiv
| Odivu
| Omod
| Omodu
| Oand
| Oandimm of Int.int
| Oor
| Oorimm of Int.int
| Oxor
| Oxorimm of Int.int
| Onot
| Oshl
| Oshlimm of Int.int
| Oshr
| Oshrimm of Int.int
| Oshrximm of Int.int
| Oshru
| Oshruimm of Int.int
| Ororimm of Int.int
| Oshldimm of Int.int
| Olea of addressing
| Omakelong
| Olowlong
| Ohighlong
| Ocast32signed
| Ocast32unsigned
| Onegl
| Oaddlimm of Int64.int
| Osubl
| Omull
| Omullimm of Int64.int
| Omullhs
| Omullhu
| Odivl
| Odivlu
| Omodl
| Omodlu
| Oandl
| Oandlimm of Int64.int
| Oorl
| Oorlimm of Int64.int
| Oxorl
| Oxorlimm of Int64.int
| Onotl
| Oshll
| Oshllimm of Int.int
| Oshrl
| Oshrlimm of Int.int
| Oshrxlimm of Int.int
| Oshrlu
| Oshrluimm of Int.int
| Ororlimm of Int.int
| Oleal of addressing
| Onegf
| Oabsf
| Oaddf
| Osubf
| Omulf
| Odivf
| Onegfs
| Oabsfs
| Oaddfs
| Osubfs
| Omulfs
| Odivfs
| Osingleoffloat
| Ofloatofsingle
| Ointoffloat
| Ofloatofint
| Ointofsingle
| Osingleofint
| Olongoffloat
| Ofloatoflong
| Olongofsingle
| Osingleoflong
| Ocmp of condition
| Osel of condition * typ

(** val eq_condition : condition -> condition -> bool **)

let eq_condition x y =
  let h0 = fun x0 y0 ->
    match x0 with
    | Ceq -> (match y0 with
              | Ceq -> true
              | _ -> false)
    | Cne -> (match y0 with
              | Cne -> true
              | _ -> false)
    | Clt -> (match y0 with
              | Clt -> true
              | _ -> false)
    | Cle -> (match y0 with
              | Cle -> true
              | _ -> false)
    | Cgt -> (match y0 with
              | Cgt -> true
              | _ -> false)
    | Cge -> (match y0 with
              | Cge -> true
              | _ -> false)
  in
  (match x with
   | Ccomp x0 -> (match y with
                  | Ccomp c0 -> h0 x0 c0
                  | _ -> false)
   | Ccompu x0 -> (match y with
                   | Ccompu c0 -> h0 x0 c0
                   | _ -> false)
   | Ccompimm (x0, x1) ->
     (match y with
      | Ccompimm (c0, n0) -> if h0 x0 c0 then Int.eq_dec x1 n0 else false
      | _ -> false)
   | Ccompuimm (x0, x1) ->
     (match y with
      | Ccompuimm (c0, n0) -> if h0 x0 c0 then Int.eq_dec x1 n0 else false
      | _ -> false)
   | Ccompl x0 -> (match y with
                   | Ccompl c0 -> h0 x0 c0
                   | _ -> false)
   | Ccomplu x0 -> (match y with
                    | Ccomplu c0 -> h0 x0 c0
                    | _ -> false)
   | Ccomplimm (x0, x1) ->
     (match y with
      | Ccomplimm (c0, n0) -> if h0 x0 c0 then Int64.eq_dec x1 n0 else false
      | _ -> false)
   | Ccompluimm (x0, x1) ->
     (match y with
      | Ccompluimm (c0, n0) -> if h0 x0 c0 then Int64.eq_dec x1 n0 else false
      | _ -> false)
   | Ccompf x0 -> (match y with
                   | Ccompf c0 -> h0 x0 c0
                   | _ -> false)
   | Cnotcompf x0 -> (match y with
                      | Cnotcompf c0 -> h0 x0 c0
                      | _ -> false)
   | Ccompfs x0 -> (match y with
                    | Ccompfs c0 -> h0 x0 c0
                    | _ -> false)
   | Cnotcompfs x0 -> (match y with
                       | Cnotcompfs c0 -> h0 x0 c0
                       | _ -> false)
   | Cmaskzero x0 ->
     (match y with
      | Cmaskzero n0 -> Int.eq_dec x0 n0
      | _ -> false)
   | Cmasknotzero x0 ->
     (match y with
      | Cmasknotzero n0 -> Int.eq_dec x0 n0
      | _ -> false))

(** val eq_addressing : addressing -> addressing -> bool **)

let eq_addressing x y =
  match x with
  | Aindexed x0 -> (match y with
                    | Aindexed z0 -> zeq x0 z0
                    | _ -> false)
  | Aindexed2 x0 -> (match y with
                     | Aindexed2 z0 -> zeq x0 z0
                     | _ -> false)
  | Ascaled (x0, x1) ->
    (match y with
     | Ascaled (z1, z2) -> if zeq x0 z1 then zeq x1 z2 else false
     | _ -> false)
  | Aindexed2scaled (x0, x1) ->
    (match y with
     | Aindexed2scaled (z1, z2) -> if zeq x0 z1 then zeq x1 z2 else false
     | _ -> false)
  | Aglobal (x0, x1) ->
    (match y with
     | Aglobal (i1, i2) ->
       if ident_eq x0 i1 then Ptrofs.eq_dec x1 i2 else false
     | _ -> false)
  | Abased (x0, x1) ->
    (match y with
     | Abased (i1, i2) ->
       if ident_eq x0 i1 then Ptrofs.eq_dec x1 i2 else false
     | _ -> false)
  | Abasedscaled (x0, x1, x2) ->
    (match y with
     | Abasedscaled (z0, i1, i2) ->
       if zeq x0 z0
       then if ident_eq x1 i1 then Ptrofs.eq_dec x2 i2 else false
       else false
     | _ -> false)
  | Ainstack x0 ->
    (match y with
     | Ainstack i0 -> Ptrofs.eq_dec x0 i0
     | _ -> false)

(** val beq_operation : operation -> operation -> bool **)

let beq_operation x y =
  match x with
  | Omove -> (match y with
              | Omove -> true
              | _ -> false)
  | Ointconst n ->
    (match y with
     | Ointconst n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Olongconst n ->
    (match y with
     | Olongconst n0 -> (fun x -> x) (Int64.eq_dec n n0)
     | _ -> false)
  | Ofloatconst n ->
    (match y with
     | Ofloatconst n0 -> (fun x -> x) (Float.eq_dec n n0)
     | _ -> false)
  | Osingleconst n ->
    (match y with
     | Osingleconst n0 -> (fun x -> x) (Float32.eq_dec n n0)
     | _ -> false)
  | Oindirectsymbol id ->
    (match y with
     | Oindirectsymbol id0 -> (fun x -> x) (ident_eq id id0)
     | _ -> false)
  | Ocast8signed -> (match y with
                     | Ocast8signed -> true
                     | _ -> false)
  | Ocast8unsigned -> (match y with
                       | Ocast8unsigned -> true
                       | _ -> false)
  | Ocast16signed -> (match y with
                      | Ocast16signed -> true
                      | _ -> false)
  | Ocast16unsigned -> (match y with
                        | Ocast16unsigned -> true
                        | _ -> false)
  | Oneg -> (match y with
             | Oneg -> true
             | _ -> false)
  | Osub -> (match y with
             | Osub -> true
             | _ -> false)
  | Omul -> (match y with
             | Omul -> true
             | _ -> false)
  | Omulimm n ->
    (match y with
     | Omulimm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Omulhs -> (match y with
               | Omulhs -> true
               | _ -> false)
  | Omulhu -> (match y with
               | Omulhu -> true
               | _ -> false)
  | Odiv -> (match y with
             | Odiv -> true
             | _ -> false)
  | Odivu -> (match y with
              | Odivu -> true
              | _ -> false)
  | Omod -> (match y with
             | Omod -> true
             | _ -> false)
  | Omodu -> (match y with
              | Omodu -> true
              | _ -> false)
  | Oand -> (match y with
             | Oand -> true
             | _ -> false)
  | Oandimm n ->
    (match y with
     | Oandimm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Oor -> (match y with
            | Oor -> true
            | _ -> false)
  | Oorimm n ->
    (match y with
     | Oorimm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Oxor -> (match y with
             | Oxor -> true
             | _ -> false)
  | Oxorimm n ->
    (match y with
     | Oxorimm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Onot -> (match y with
             | Onot -> true
             | _ -> false)
  | Oshl -> (match y with
             | Oshl -> true
             | _ -> false)
  | Oshlimm n ->
    (match y with
     | Oshlimm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Oshr -> (match y with
             | Oshr -> true
             | _ -> false)
  | Oshrimm n ->
    (match y with
     | Oshrimm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Oshrximm n ->
    (match y with
     | Oshrximm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Oshru -> (match y with
              | Oshru -> true
              | _ -> false)
  | Oshruimm n ->
    (match y with
     | Oshruimm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Ororimm n ->
    (match y with
     | Ororimm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Oshldimm n ->
    (match y with
     | Oshldimm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Olea a ->
    (match y with
     | Olea a0 -> (fun x -> x) (eq_addressing a a0)
     | _ -> false)
  | Omakelong -> (match y with
                  | Omakelong -> true
                  | _ -> false)
  | Olowlong -> (match y with
                 | Olowlong -> true
                 | _ -> false)
  | Ohighlong -> (match y with
                  | Ohighlong -> true
                  | _ -> false)
  | Ocast32signed -> (match y with
                      | Ocast32signed -> true
                      | _ -> false)
  | Ocast32unsigned -> (match y with
                        | Ocast32unsigned -> true
                        | _ -> false)
  | Onegl -> (match y with
              | Onegl -> true
              | _ -> false)
  | Oaddlimm n ->
    (match y with
     | Oaddlimm n0 -> (fun x -> x) (Int64.eq_dec n n0)
     | _ -> false)
  | Osubl -> (match y with
              | Osubl -> true
              | _ -> false)
  | Omull -> (match y with
              | Omull -> true
              | _ -> false)
  | Omullimm n ->
    (match y with
     | Omullimm n0 -> (fun x -> x) (Int64.eq_dec n n0)
     | _ -> false)
  | Omullhs -> (match y with
                | Omullhs -> true
                | _ -> false)
  | Omullhu -> (match y with
                | Omullhu -> true
                | _ -> false)
  | Odivl -> (match y with
              | Odivl -> true
              | _ -> false)
  | Odivlu -> (match y with
               | Odivlu -> true
               | _ -> false)
  | Omodl -> (match y with
              | Omodl -> true
              | _ -> false)
  | Omodlu -> (match y with
               | Omodlu -> true
               | _ -> false)
  | Oandl -> (match y with
              | Oandl -> true
              | _ -> false)
  | Oandlimm n ->
    (match y with
     | Oandlimm n0 -> (fun x -> x) (Int64.eq_dec n n0)
     | _ -> false)
  | Oorl -> (match y with
             | Oorl -> true
             | _ -> false)
  | Oorlimm n ->
    (match y with
     | Oorlimm n0 -> (fun x -> x) (Int64.eq_dec n n0)
     | _ -> false)
  | Oxorl -> (match y with
              | Oxorl -> true
              | _ -> false)
  | Oxorlimm n ->
    (match y with
     | Oxorlimm n0 -> (fun x -> x) (Int64.eq_dec n n0)
     | _ -> false)
  | Onotl -> (match y with
              | Onotl -> true
              | _ -> false)
  | Oshll -> (match y with
              | Oshll -> true
              | _ -> false)
  | Oshllimm n ->
    (match y with
     | Oshllimm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Oshrl -> (match y with
              | Oshrl -> true
              | _ -> false)
  | Oshrlimm n ->
    (match y with
     | Oshrlimm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Oshrxlimm n ->
    (match y with
     | Oshrxlimm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Oshrlu -> (match y with
               | Oshrlu -> true
               | _ -> false)
  | Oshrluimm n ->
    (match y with
     | Oshrluimm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Ororlimm n ->
    (match y with
     | Ororlimm n0 -> (fun x -> x) (Int.eq_dec n n0)
     | _ -> false)
  | Oleal a ->
    (match y with
     | Oleal a0 -> (fun x -> x) (eq_addressing a a0)
     | _ -> false)
  | Onegf -> (match y with
              | Onegf -> true
              | _ -> false)
  | Oabsf -> (match y with
              | Oabsf -> true
              | _ -> false)
  | Oaddf -> (match y with
              | Oaddf -> true
              | _ -> false)
  | Osubf -> (match y with
              | Osubf -> true
              | _ -> false)
  | Omulf -> (match y with
              | Omulf -> true
              | _ -> false)
  | Odivf -> (match y with
              | Odivf -> true
              | _ -> false)
  | Onegfs -> (match y with
               | Onegfs -> true
               | _ -> false)
  | Oabsfs -> (match y with
               | Oabsfs -> true
               | _ -> false)
  | Oaddfs -> (match y with
               | Oaddfs -> true
               | _ -> false)
  | Osubfs -> (match y with
               | Osubfs -> true
               | _ -> false)
  | Omulfs -> (match y with
               | Omulfs -> true
               | _ -> false)
  | Odivfs -> (match y with
               | Odivfs -> true
               | _ -> false)
  | Osingleoffloat -> (match y with
                       | Osingleoffloat -> true
                       | _ -> false)
  | Ofloatofsingle -> (match y with
                       | Ofloatofsingle -> true
                       | _ -> false)
  | Ointoffloat -> (match y with
                    | Ointoffloat -> true
                    | _ -> false)
  | Ofloatofint -> (match y with
                    | Ofloatofint -> true
                    | _ -> false)
  | Ointofsingle -> (match y with
                     | Ointofsingle -> true
                     | _ -> false)
  | Osingleofint -> (match y with
                     | Osingleofint -> true
                     | _ -> false)
  | Olongoffloat -> (match y with
                     | Olongoffloat -> true
                     | _ -> false)
  | Ofloatoflong -> (match y with
                     | Ofloatoflong -> true
                     | _ -> false)
  | Olongofsingle -> (match y with
                      | Olongofsingle -> true
                      | _ -> false)
  | Osingleoflong -> (match y with
                      | Osingleoflong -> true
                      | _ -> false)
  | Ocmp cond ->
    (match y with
     | Ocmp cond0 -> (fun x -> x) (eq_condition cond cond0)
     | _ -> false)
  | Osel (c, t) ->
    (match y with
     | Osel (c0, t0) ->
       (&&) ((fun x -> x) (eq_condition c c0)) ((fun x -> x) (typ_eq t t0))
     | _ -> false)

(** val eq_operation : operation -> operation -> bool **)

let eq_operation =
  BE.dec_eq_from_bool_eq beq_operation

(** val offset_in_range : coq_Z -> bool **)

let offset_in_range n =
  (&&) ((fun x -> x) (zle Int.min_signed n))
    ((fun x -> x) (zle n Int.max_signed))

(** val ptroffset_min : coq_Z **)

let ptroffset_min =
  Zneg (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH))))))))))))))))))))))))

(** val ptroffset_max : coq_Z **)

let ptroffset_max =
  Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    Coq_xH)))))))))))))))))))))))

(** val ptroffset_in_range : Ptrofs.int -> bool **)

let ptroffset_in_range n =
  let n0 = Ptrofs.signed n in
  (&&) ((fun x -> x) (zle ptroffset_min n0))
    ((fun x -> x) (zle n0 ptroffset_max))

(** val addressing_valid : addressing -> bool **)

let addressing_valid a =
  if ptr64
  then (match a with
        | Aindexed n -> offset_in_range n
        | Aindexed2 n -> offset_in_range n
        | Ascaled (_, ofs) -> offset_in_range ofs
        | Aindexed2scaled (_, ofs) -> offset_in_range ofs
        | Aglobal (_, ofs) -> ptroffset_in_range ofs
        | Abased (_, ofs) -> ptroffset_in_range ofs
        | Abasedscaled (_, _, ofs) -> ptroffset_in_range ofs
        | Ainstack ofs -> offset_in_range (Ptrofs.signed ofs))
  else true

(** val type_of_condition : condition -> typ list **)

let type_of_condition = function
| Ccomp _ -> Tint :: (Tint :: [])
| Ccompu _ -> Tint :: (Tint :: [])
| Ccompl _ -> Tlong :: (Tlong :: [])
| Ccomplu _ -> Tlong :: (Tlong :: [])
| Ccomplimm (_, _) -> Tlong :: []
| Ccompluimm (_, _) -> Tlong :: []
| Ccompf _ -> Tfloat :: (Tfloat :: [])
| Cnotcompf _ -> Tfloat :: (Tfloat :: [])
| Ccompfs _ -> Tsingle :: (Tsingle :: [])
| Cnotcompfs _ -> Tsingle :: (Tsingle :: [])
| _ -> Tint :: []

(** val type_of_addressing_gen : typ -> addressing -> typ list **)

let type_of_addressing_gen tyA = function
| Aindexed2 _ -> tyA :: (tyA :: [])
| Aindexed2scaled (_, _) -> tyA :: (tyA :: [])
| Aglobal (_, _) -> []
| Ainstack _ -> []
| _ -> tyA :: []

(** val type_of_addressing : addressing -> typ list **)

let type_of_addressing =
  type_of_addressing_gen coq_Tptr

(** val type_of_addressing32 : addressing -> typ list **)

let type_of_addressing32 =
  type_of_addressing_gen Tint

(** val type_of_addressing64 : addressing -> typ list **)

let type_of_addressing64 =
  type_of_addressing_gen Tlong

(** val type_of_operation : operation -> typ list * typ **)

let type_of_operation = function
| Omove -> ([], Tint)
| Ointconst _ -> ([], Tint)
| Olongconst _ -> ([], Tlong)
| Ofloatconst _ -> ([], Tfloat)
| Osingleconst _ -> ([], Tsingle)
| Oindirectsymbol _ -> ([], coq_Tptr)
| Ocast8signed -> ((Tint :: []), Tint)
| Ocast8unsigned -> ((Tint :: []), Tint)
| Ocast16signed -> ((Tint :: []), Tint)
| Ocast16unsigned -> ((Tint :: []), Tint)
| Oneg -> ((Tint :: []), Tint)
| Omulimm _ -> ((Tint :: []), Tint)
| Oandimm _ -> ((Tint :: []), Tint)
| Oorimm _ -> ((Tint :: []), Tint)
| Oxorimm _ -> ((Tint :: []), Tint)
| Onot -> ((Tint :: []), Tint)
| Oshlimm _ -> ((Tint :: []), Tint)
| Oshrimm _ -> ((Tint :: []), Tint)
| Oshrximm _ -> ((Tint :: []), Tint)
| Oshruimm _ -> ((Tint :: []), Tint)
| Ororimm _ -> ((Tint :: []), Tint)
| Olea addr -> ((type_of_addressing32 addr), Tint)
| Omakelong -> ((Tint :: (Tint :: [])), Tlong)
| Olowlong -> ((Tlong :: []), Tint)
| Ohighlong -> ((Tlong :: []), Tint)
| Ocast32signed -> ((Tint :: []), Tlong)
| Ocast32unsigned -> ((Tint :: []), Tlong)
| Onegl -> ((Tlong :: []), Tlong)
| Oaddlimm _ -> ((Tlong :: []), Tlong)
| Osubl -> ((Tlong :: (Tlong :: [])), Tlong)
| Omull -> ((Tlong :: (Tlong :: [])), Tlong)
| Omullimm _ -> ((Tlong :: []), Tlong)
| Omullhs -> ((Tlong :: (Tlong :: [])), Tlong)
| Omullhu -> ((Tlong :: (Tlong :: [])), Tlong)
| Odivl -> ((Tlong :: (Tlong :: [])), Tlong)
| Odivlu -> ((Tlong :: (Tlong :: [])), Tlong)
| Omodl -> ((Tlong :: (Tlong :: [])), Tlong)
| Omodlu -> ((Tlong :: (Tlong :: [])), Tlong)
| Oandl -> ((Tlong :: (Tlong :: [])), Tlong)
| Oandlimm _ -> ((Tlong :: []), Tlong)
| Oorl -> ((Tlong :: (Tlong :: [])), Tlong)
| Oorlimm _ -> ((Tlong :: []), Tlong)
| Oxorl -> ((Tlong :: (Tlong :: [])), Tlong)
| Oxorlimm _ -> ((Tlong :: []), Tlong)
| Onotl -> ((Tlong :: []), Tlong)
| Oshll -> ((Tlong :: (Tint :: [])), Tlong)
| Oshllimm _ -> ((Tlong :: []), Tlong)
| Oshrl -> ((Tlong :: (Tint :: [])), Tlong)
| Oshrlimm _ -> ((Tlong :: []), Tlong)
| Oshrxlimm _ -> ((Tlong :: []), Tlong)
| Oshrlu -> ((Tlong :: (Tint :: [])), Tlong)
| Oshrluimm _ -> ((Tlong :: []), Tlong)
| Ororlimm _ -> ((Tlong :: []), Tlong)
| Oleal addr -> ((type_of_addressing64 addr), Tlong)
| Onegf -> ((Tfloat :: []), Tfloat)
| Oabsf -> ((Tfloat :: []), Tfloat)
| Oaddf -> ((Tfloat :: (Tfloat :: [])), Tfloat)
| Osubf -> ((Tfloat :: (Tfloat :: [])), Tfloat)
| Omulf -> ((Tfloat :: (Tfloat :: [])), Tfloat)
| Odivf -> ((Tfloat :: (Tfloat :: [])), Tfloat)
| Onegfs -> ((Tsingle :: []), Tsingle)
| Oabsfs -> ((Tsingle :: []), Tsingle)
| Oaddfs -> ((Tsingle :: (Tsingle :: [])), Tsingle)
| Osubfs -> ((Tsingle :: (Tsingle :: [])), Tsingle)
| Omulfs -> ((Tsingle :: (Tsingle :: [])), Tsingle)
| Odivfs -> ((Tsingle :: (Tsingle :: [])), Tsingle)
| Osingleoffloat -> ((Tfloat :: []), Tsingle)
| Ofloatofsingle -> ((Tsingle :: []), Tfloat)
| Ointoffloat -> ((Tfloat :: []), Tint)
| Ofloatofint -> ((Tint :: []), Tfloat)
| Ointofsingle -> ((Tsingle :: []), Tint)
| Osingleofint -> ((Tint :: []), Tsingle)
| Olongoffloat -> ((Tfloat :: []), Tlong)
| Ofloatoflong -> ((Tlong :: []), Tfloat)
| Olongofsingle -> ((Tsingle :: []), Tlong)
| Osingleoflong -> ((Tlong :: []), Tsingle)
| Ocmp c -> ((type_of_condition c), Tint)
| Osel (c, ty) -> ((ty :: (ty :: (type_of_condition c))), ty)
| _ -> ((Tint :: (Tint :: [])), Tint)

(** val is_move_operation : operation -> 'a1 list -> 'a1 option **)

let is_move_operation op args =
  match op with
  | Omove ->
    (match args with
     | [] -> None
     | arg :: l -> (match l with
                    | [] -> Some arg
                    | _ :: _ -> None))
  | _ -> None

(** val negate_condition : condition -> condition **)

let negate_condition = function
| Ccomp c -> Ccomp (negate_comparison c)
| Ccompu c -> Ccompu (negate_comparison c)
| Ccompimm (c, n) -> Ccompimm ((negate_comparison c), n)
| Ccompuimm (c, n) -> Ccompuimm ((negate_comparison c), n)
| Ccompl c -> Ccompl (negate_comparison c)
| Ccomplu c -> Ccomplu (negate_comparison c)
| Ccomplimm (c, n) -> Ccomplimm ((negate_comparison c), n)
| Ccompluimm (c, n) -> Ccompluimm ((negate_comparison c), n)
| Ccompf c -> Cnotcompf c
| Cnotcompf c -> Ccompf c
| Ccompfs c -> Cnotcompfs c
| Cnotcompfs c -> Ccompfs c
| Cmaskzero n -> Cmasknotzero n
| Cmasknotzero n -> Cmaskzero n

(** val shift_stack_addressing : coq_Z -> addressing -> addressing **)

let shift_stack_addressing delta addr = match addr with
| Ainstack ofs -> Ainstack (Ptrofs.add ofs (Ptrofs.repr delta))
| _ -> addr

(** val shift_stack_operation : coq_Z -> operation -> operation **)

let shift_stack_operation delta op = match op with
| Olea addr -> Olea (shift_stack_addressing delta addr)
| Oleal addr -> Oleal (shift_stack_addressing delta addr)
| _ -> op

(** val offset_addressing_total : addressing -> coq_Z -> addressing **)

let offset_addressing_total addr delta =
  match addr with
  | Aindexed n -> Aindexed (Z.add n delta)
  | Aindexed2 n -> Aindexed2 (Z.add n delta)
  | Ascaled (sc, n) -> Ascaled (sc, (Z.add n delta))
  | Aindexed2scaled (sc, n) -> Aindexed2scaled (sc, (Z.add n delta))
  | Aglobal (s, n) -> Aglobal (s, (Ptrofs.add n (Ptrofs.repr delta)))
  | Abased (s, n) -> Abased (s, (Ptrofs.add n (Ptrofs.repr delta)))
  | Abasedscaled (sc, s, n) ->
    Abasedscaled (sc, s, (Ptrofs.add n (Ptrofs.repr delta)))
  | Ainstack n -> Ainstack (Ptrofs.add n (Ptrofs.repr delta))

(** val offset_addressing : addressing -> coq_Z -> addressing option **)

let offset_addressing addr delta =
  let addr' = offset_addressing_total addr delta in
  if addressing_valid addr' then Some addr' else None

(** val is_trivial_op : operation -> bool **)

let is_trivial_op = function
| Omove -> true
| Ointconst _ -> true
| Olongconst _ -> true
| Olea a ->
  (match a with
   | Aglobal (_, _) -> true
   | Ainstack _ -> true
   | _ -> false)
| Oleal a ->
  (match a with
   | Aglobal (_, _) -> true
   | Ainstack _ -> true
   | _ -> false)
| _ -> false

(** val condition_depends_on_memory : condition -> bool **)

let condition_depends_on_memory = function
| Ccompu _ -> negb ptr64
| Ccompuimm (_, _) -> negb ptr64
| Ccomplu _ -> ptr64
| Ccompluimm (_, _) -> ptr64
| _ -> false

(** val op_depends_on_memory : operation -> bool **)

let op_depends_on_memory = function
| Ocmp c -> condition_depends_on_memory c
| Osel (c, _) -> condition_depends_on_memory c
| _ -> false

(** val globals_addressing : addressing -> ident list **)

let globals_addressing = function
| Aglobal (s, _) -> s :: []
| Abased (s, _) -> s :: []
| Abasedscaled (_, s, _) -> s :: []
| _ -> []

(** val globals_operation : operation -> ident list **)

let globals_operation = function
| Oindirectsymbol s -> s :: []
| Olea addr -> globals_addressing addr
| Oleal addr -> globals_addressing addr
| _ -> []

(** val builtin_arg_ok_1 :
    'a1 builtin_arg -> builtin_arg_constraint -> bool **)

let builtin_arg_ok_1 ba = function
| OK_default -> false
| OK_const ->
  (match ba with
   | BA_int _ -> true
   | BA_long _ -> true
   | BA_float _ -> true
   | BA_single _ -> true
   | _ -> false)
| OK_addrstack -> (match ba with
                   | BA_addrstack _ -> true
                   | _ -> false)
| OK_addressing ->
  (match ba with
   | BA_addrstack _ -> true
   | BA_addrglobal (_, _) -> true
   | BA_addptr (a1, a2) ->
     (match a1 with
      | BA _ ->
        (match a2 with
         | BA_int _ -> true
         | BA_long _ -> true
         | _ -> false)
      | _ -> false)
   | _ -> false)
| OK_all -> true

(** val builtin_arg_ok : 'a1 builtin_arg -> builtin_arg_constraint -> bool **)

let builtin_arg_ok ba c =
  match ba with
  | BA _ -> true
  | BA_splitlong (hi, lo) ->
    (match hi with
     | BA _ -> (match lo with
                | BA _ -> true
                | _ -> builtin_arg_ok_1 ba c)
     | _ -> builtin_arg_ok_1 ba c)
  | _ -> builtin_arg_ok_1 ba c
