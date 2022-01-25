open AST
open BinInt
open BinNums
open Datatypes
open Integers
open IntvSets
open Lattice
open Maps
open Registers
open ValueDomain

type nval =
| Nothing
| I of Int.int
| All

val eq_nval : nval -> nval -> bool

val nlub : nval -> nval -> nval

val complete_mask : Int.int -> Int.int

val andimm : nval -> Int.int -> nval

val orimm : nval -> Int.int -> nval

val bitwise : nval -> nval

val shlimm : nval -> Int.int -> nval

val shruimm : nval -> Int.int -> nval

val shrimm : nval -> Int.int -> nval

val ror : nval -> Int.int -> nval

val modarith : nval -> nval

val zero_ext : coq_Z -> nval -> nval

val sign_ext : coq_Z -> nval -> nval

val store_argument : memory_chunk -> nval

val maskzero : Int.int -> nval

val default : nval -> nval

val andimm_redundant : nval -> Int.int -> bool

val orimm_redundant : nval -> Int.int -> bool

val zero_ext_redundant : coq_Z -> nval -> bool

val sign_ext_redundant : coq_Z -> nval -> bool

module NVal :
 sig
  type t = nval

  val beq : t -> t -> bool

  val bot : t

  val lub : nval -> nval -> nval
 end

module NE :
 sig
  type t = NVal.t PTree.t

  val get : positive -> t -> NVal.t

  val set : positive -> NVal.t -> t -> t

  val beq : t -> t -> bool

  val bot : t

  val opt_beq : NVal.t option -> NVal.t option -> bool

  type changed =
  | Same
  | Same1
  | Same2
  | Changed of NVal.t PTree.t

  val xcombine :
    (NVal.t option -> NVal.t option -> NVal.t option) -> NVal.t PTree.tree ->
    NVal.t PTree.tree -> changed

  val combine :
    (NVal.t option -> NVal.t option -> NVal.t option) -> NVal.t PTree.t ->
    NVal.t PTree.t -> NVal.t PTree.t

  val lub : t -> t -> t
 end

type nenv = NE.t

val nreg : nenv -> reg -> NVal.t

type nmem =
| NMemDead
| NMem of ISet.t * ISet.t PTree.t

val nmem_all : nmem

val nmem_add : nmem -> aptr -> coq_Z -> nmem

val nmem_remove : nmem -> aptr -> coq_Z -> nmem

val nmem_contains : nmem -> aptr -> coq_Z -> bool

val nmem_dead_stack : coq_Z -> nmem

val nmem_lub : nmem -> nmem -> nmem

val nmem_beq : nmem -> nmem -> bool

module NA :
 sig
  type t = nenv * nmem

  val beq : t -> t -> bool

  val bot : t

  val lub : t -> t -> t
 end
