open AST
open BinInt
open BinNums
open BinPos
open Coqlib
open Datatypes
open Integers
open Maps
open Memdata
open Memtype
open Values
open Znumtheory

module Mem :
 sig
  type mem' = { mem_contents : memval ZMap.t PMap.t;
                mem_access : (coq_Z -> perm_kind -> permission option) PMap.t;
                nextblock : block }

  val mem_contents : mem' -> memval ZMap.t PMap.t

  val mem_access : mem' -> (coq_Z -> perm_kind -> permission option) PMap.t

  val nextblock : mem' -> block

  type mem = mem'

  val perm_order_dec : permission -> permission -> bool

  val perm_order'_dec : permission option -> permission -> bool

  val perm_dec : mem -> block -> coq_Z -> perm_kind -> permission -> bool

  val range_perm_dec :
    mem -> block -> coq_Z -> coq_Z -> perm_kind -> permission -> bool

  val valid_access_dec :
    mem -> memory_chunk -> block -> coq_Z -> permission -> bool

  val valid_pointer : mem -> block -> coq_Z -> bool

  val weak_valid_pointer : mem -> block -> coq_Z -> bool

  val empty : mem

  val alloc : mem -> coq_Z -> coq_Z -> mem' * block

  val unchecked_free : mem -> block -> coq_Z -> coq_Z -> mem

  val free : mem -> block -> coq_Z -> coq_Z -> mem option

  val free_list : mem -> ((block * coq_Z) * coq_Z) list -> mem option

  val getN : nat -> coq_Z -> memval ZMap.t -> memval list

  val load : memory_chunk -> mem -> block -> coq_Z -> coq_val option

  val loadv : memory_chunk -> mem -> coq_val -> coq_val option

  val loadbytes : mem -> block -> coq_Z -> coq_Z -> memval list option

  val setN : memval list -> coq_Z -> memval ZMap.t -> memval ZMap.t

  val store : memory_chunk -> mem -> block -> coq_Z -> coq_val -> mem option

  val storev : memory_chunk -> mem -> coq_val -> coq_val -> mem option

  val storebytes : mem -> block -> coq_Z -> memval list -> mem option

  val drop_perm : mem -> block -> coq_Z -> coq_Z -> permission -> mem option
 end
