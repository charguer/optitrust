open Ast
open Target


(* [use_goto_for_return mark]: *)
let use_goto_for_return ?(mark : mark = "") : Transfo.t =
  apply_on_targets (Apac_core.use_goto_for_return mark)

