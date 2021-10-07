open Ast
open Target

let shift ?(neg : bool = false) ?(pre_cast : typ option) ?(post_cast : typ option) (u : trm) (tg : target) : unit =
  Target.apply_on_targets (Arith_core.shift neg pre_cast post_cast u ) tg;
  Trace.reparse()

