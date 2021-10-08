open Ast
open Target


(* [shift ~neg ~pre_cast ~post_cast u] expects the target [tg] to point 
    to a set operation of a get operation then it will replace trm with an 
    operation which can be an addition or substraction of that trm and [code].
    [neg] is a flag to decide if the new trm is an addition or a substraction
    [pre_cast] is a typ option, by default is None if its given than the targeted
    trm will be first casted before applying the operation.
    [post_cast] similat to [pre_cast] but the casting is done after applying the 
    operation.
*)
let shift ?(neg : bool = false) ?(pre_cast : typ option) ?(post_cast : typ option) (code : trm) (tg : target) : unit =
  Target.apply_on_targets (Arith_core.shift neg pre_cast post_cast code ) tg;
  Trace.reparse()

