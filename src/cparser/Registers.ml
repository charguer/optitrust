open BinNums
open Coqlib
open Datatypes
open FSetAVL
open Int0
open Ordered

type reg = positive

module Reg =
 struct
  (** val eq : positive -> positive -> bool **)

  let eq =
    peq
 end

module Regset = Make(OrderedPositive)
