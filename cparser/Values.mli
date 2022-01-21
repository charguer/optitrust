open AST
open Archi
open BinInt
open BinNums
open Coqlib
open Datatypes
open Floats
open Integers

type block = positive

val eq_block : positive -> positive -> bool

type coq_val =
| Vundef
| Vint of Int.int
| Vlong of Int64.int
| Vfloat of float
| Vsingle of float32
| Vptr of block * Ptrofs.int

val coq_Vzero : coq_val

val coq_Vone : coq_val

val coq_Vtrue : coq_val

val coq_Vfalse : coq_val

val coq_Vptrofs : Ptrofs.int -> coq_val

module Val :
 sig
  val eq : coq_val -> coq_val -> bool

  val of_bool : bool -> coq_val

  val add : coq_val -> coq_val -> coq_val

  val addl : coq_val -> coq_val -> coq_val

  val subl : coq_val -> coq_val -> coq_val

  val mull' : coq_val -> coq_val -> coq_val

  val divls : coq_val -> coq_val -> coq_val option

  val modls : coq_val -> coq_val -> coq_val option

  val divlu : coq_val -> coq_val -> coq_val option

  val modlu : coq_val -> coq_val -> coq_val option

  val shll : coq_val -> coq_val -> coq_val

  val shrl : coq_val -> coq_val -> coq_val

  val shrlu : coq_val -> coq_val -> coq_val

  val cmp_different_blocks : comparison -> bool option

  val cmpu_bool :
    (block -> coq_Z -> bool) -> comparison -> coq_val -> coq_val -> bool
    option

  val cmplu_bool :
    (block -> coq_Z -> bool) -> comparison -> coq_val -> coq_val -> bool
    option

  val normalize : coq_val -> typ -> coq_val

  val load_result : memory_chunk -> coq_val -> coq_val
 end
