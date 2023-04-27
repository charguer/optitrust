open BinNums
open BinPos
open Coqlib
open Datatypes
open Iteration
open Maps
open Mergesort

type node = positive

type graph = node list PTree.t

module PositiveOrd :
 sig
  type t = positive

  val leb : t -> t -> bool
 end

module Sort :
 sig
  val merge : positive list -> positive list -> positive list

  val merge_list_to_stack :
    positive list option list -> positive list -> positive list option list

  val merge_stack : positive list option list -> positive list

  val iter_merge : positive list option list -> positive list -> positive list

  val sort : positive list -> positive list

  val flatten_stack : positive list option list -> positive list
 end

type state = { gr : graph; wrk : (node * node list) list;
               map : positive PTree.t; next : positive }

val init_state : graph -> node -> state

val transition : state -> (positive PTree.t, state) sum

val postorder : graph -> node -> positive PTree.t
