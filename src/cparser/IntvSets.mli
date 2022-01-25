open BinInt
open BinNums
open Coqlib

module ISet :
 sig
  module R :
   sig
    type t =
    | Nil
    | Cons of coq_Z * coq_Z * t

    val contains : coq_Z -> coq_Z -> t -> bool

    val add : coq_Z -> coq_Z -> t -> t

    val remove : coq_Z -> coq_Z -> t -> t

    val inter : t -> t -> t

    val beq : t -> t -> bool
   end

  type t = R.t

  val empty : t

  val interval : coq_Z -> coq_Z -> t

  val add : coq_Z -> coq_Z -> t -> t

  val remove : coq_Z -> coq_Z -> t -> t

  val inter : t -> t -> t

  val contains : coq_Z -> coq_Z -> t -> bool

  val beq : t -> t -> bool
 end
