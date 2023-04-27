open BinNums
open BinPos
open Coqlib
open Datatypes
open Specif

type __ = Obj.t

module WfIter :
 sig
  val step_info : ('a1 -> ('a2, 'a1) sum) -> 'a1 -> ('a2, 'a1 sig2) sum

  val iterate_F : ('a1 -> ('a2, 'a1) sum) -> 'a1 -> ('a1 -> __ -> 'a2) -> 'a2

  val iterate : ('a1 -> ('a2, 'a1) sum) -> 'a1 -> 'a2
 end

module PrimIter :
 sig
  val num_iterations : positive

  val iter_step :
    ('a1 -> ('a2, 'a1) sum) -> positive -> (positive -> __ -> 'a1 -> 'a2
    option) -> 'a1 -> 'a2 option

  val iter : ('a1 -> ('a2, 'a1) sum) -> positive -> 'a1 -> 'a2 option

  val iterate : ('a1 -> ('a2, 'a1) sum) -> 'a1 -> 'a2 option
 end
