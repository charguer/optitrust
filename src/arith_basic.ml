open Ast
open Target

let data_shift ?(neg : bool = true) ?(pre_cast : typ = typ_unit ()) ?(post_cast : typ = typ_unit ()) (u : trm) (tg : target) : unit =
  Target.apply_on_targets (Arith_core.data_shift neg pre_cast post_cast u ) tg;
  Trace.reparse()

