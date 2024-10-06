open Prelude



(* maybe: flag ~elimoptitrust:true on run script? *)
let%transfo std ?(arith_simpl : (Arith.expr -> Arith.expr) list = [Arith.gather_rec]) (_u : unit) : unit =
  Trace.tag "pre-post-processing";
  Function.use_infix_ops ~indepth:true [];
  Flags.recompute_resources_between_steps := false;
  Matrix.elim_mops [];
  Arith.(simpl_surrounding_expr (compose arith_simpl)) [nbMulti; cAccesses()];
  Resources.delete_annots [];
  Loop.delete_all_void []

