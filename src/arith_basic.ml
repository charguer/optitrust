open Ast
open Target

let shift ?(neg : bool = true) ?(pre_cast : typ = typ_unit ()) ?(post_cast : typ = typ_unit ()) (u : trm) (tg : target) : unit =
  Target.apply_on_targets (Arith_core.shift neg pre_cast post_cast u ) tg;
  Trace.reparse()

