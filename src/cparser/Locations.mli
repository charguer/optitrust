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

val slot_eq : slot -> slot -> bool

val typesize : typ -> coq_Z

val typealign : typ -> coq_Z

type loc =
| R of mreg
| S of slot * coq_Z * typ

module Loc :
 sig
  val coq_type : loc -> typ

  val eq : loc -> loc -> bool

  val diff_dec : loc -> loc -> bool
 end

module IndexedTyp :
 sig
  type t = typ

  val index : t -> positive

  val eq : typ -> typ -> bool
 end

module OrderedTyp :
 sig
  type t = IndexedTyp.t

  val compare : t -> t -> t OrderedType.coq_Compare

  val eq_dec : t -> t -> bool
 end

module IndexedSlot :
 sig
  type t = slot

  val index : t -> positive

  val eq : slot -> slot -> bool
 end

module OrderedSlot :
 sig
  type t = IndexedSlot.t

  val compare : t -> t -> t OrderedType.coq_Compare

  val eq_dec : t -> t -> bool
 end

module OrderedLoc :
 sig
  type t = loc

  val compare : t -> t -> t OrderedType.coq_Compare

  val diff_low_bound : loc -> loc

  val diff_high_bound : loc -> loc
 end
