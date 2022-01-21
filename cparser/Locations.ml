open AST
open BinInt
open BinNums
open Coqlib
open Machregs
open Ordered

type slot =
| Local
| Incoming
| Outgoing

(** val slot_eq : slot -> slot -> bool **)

let slot_eq p q =
  match p with
  | Local -> (match q with
              | Local -> true
              | _ -> false)
  | Incoming -> (match q with
                 | Incoming -> true
                 | _ -> false)
  | Outgoing -> (match q with
                 | Outgoing -> true
                 | _ -> false)

(** val typesize : typ -> coq_Z **)

let typesize = function
| Tint -> Zpos Coq_xH
| Tsingle -> Zpos Coq_xH
| Tany32 -> Zpos Coq_xH
| _ -> Zpos (Coq_xO Coq_xH)

(** val typealign : typ -> coq_Z **)

let typealign = function
| Tlong -> Zpos (Coq_xO Coq_xH)
| _ -> Zpos Coq_xH

type loc =
| R of mreg
| S of slot * coq_Z * typ

module Loc =
 struct
  (** val coq_type : loc -> typ **)

  let coq_type = function
  | R r -> mreg_type r
  | S (_, _, ty) -> ty

  (** val eq : loc -> loc -> bool **)

  let eq p q =
    match p with
    | R x -> (match q with
              | R r0 -> mreg_eq x r0
              | S (_, _, _) -> false)
    | S (x, x0, x1) ->
      (match q with
       | R _ -> false
       | S (sl0, pos0, ty0) ->
         if slot_eq x sl0
         then if zeq x0 pos0 then typ_eq x1 ty0 else false
         else false)

  (** val diff_dec : loc -> loc -> bool **)

  let diff_dec l1 l2 =
    match l1 with
    | R r ->
      (match l2 with
       | R r0 -> let s = mreg_eq r r0 in if s then false else true
       | S (_, _, _) -> true)
    | S (sl, pos, ty) ->
      (match l2 with
       | R _ -> true
       | S (sl0, pos0, ty0) ->
         let s = slot_eq sl sl0 in
         if s
         then let s0 = zle (Z.add pos (typesize ty)) pos0 in
              if s0 then true else zle (Z.add pos0 (typesize ty0)) pos
         else true)
 end

module IndexedTyp =
 struct
  type t = typ

  (** val index : t -> positive **)

  let index = function
  | Tint -> Coq_xO Coq_xH
  | Tfloat -> Coq_xI (Coq_xO Coq_xH)
  | Tlong -> Coq_xO (Coq_xI Coq_xH)
  | Tsingle -> Coq_xI Coq_xH
  | Tany32 -> Coq_xH
  | Tany64 -> Coq_xO (Coq_xO Coq_xH)

  (** val eq : typ -> typ -> bool **)

  let eq =
    typ_eq
 end

module OrderedTyp = OrderedIndexed(IndexedTyp)

module IndexedSlot =
 struct
  type t = slot

  (** val index : t -> positive **)

  let index = function
  | Local -> Coq_xH
  | Incoming -> Coq_xO Coq_xH
  | Outgoing -> Coq_xI Coq_xH

  (** val eq : slot -> slot -> bool **)

  let eq =
    slot_eq
 end

module OrderedSlot = OrderedIndexed(IndexedSlot)

module OrderedLoc =
 struct
  type t = loc

  (** val compare : t -> t -> t OrderedType.coq_Compare **)

  let compare x y =
    match x with
    | R r ->
      (match y with
       | R r0 ->
         let c =
           OrderedPositive.compare (IndexedMreg.index r)
             (IndexedMreg.index r0)
         in
         (match c with
          | OrderedType.LT -> OrderedType.LT
          | OrderedType.EQ -> OrderedType.EQ
          | OrderedType.GT -> OrderedType.GT)
       | S (_, _, _) -> OrderedType.LT)
    | S (sl, pos, ty) ->
      (match y with
       | R _ -> OrderedType.GT
       | S (sl0, pos0, ty0) ->
         let c = OrderedSlot.compare sl sl0 in
         (match c with
          | OrderedType.LT -> OrderedType.LT
          | OrderedType.EQ ->
            let c0 = OrderedZ.compare pos pos0 in
            (match c0 with
             | OrderedType.LT -> OrderedType.LT
             | OrderedType.EQ ->
               let c1 = OrderedTyp.compare ty ty0 in
               (match c1 with
                | OrderedType.LT -> OrderedType.LT
                | OrderedType.EQ -> OrderedType.EQ
                | OrderedType.GT -> OrderedType.GT)
             | OrderedType.GT -> OrderedType.GT)
          | OrderedType.GT -> OrderedType.GT))

  (** val diff_low_bound : loc -> loc **)

  let diff_low_bound l = match l with
  | R _ -> l
  | S (sl, ofs, _) -> S (sl, (Z.sub ofs (Zpos Coq_xH)), Tany64)

  (** val diff_high_bound : loc -> loc **)

  let diff_high_bound l = match l with
  | R _ -> l
  | S (sl, ofs, ty) ->
    S (sl, (Z.sub (Z.add ofs (typesize ty)) (Zpos Coq_xH)), Tlong)
 end
