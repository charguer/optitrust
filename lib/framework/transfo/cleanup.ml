open Prelude



(* maybe: flag ~elimoptitrust:true on run script? *)
let%transfo std ?(arith_simpl : (Arith.expr -> Arith.expr) list = [Arith.gather_rec]) (_u : unit) : unit =
  Trace.tag "pre-post-processing";
  Function.use_infix_ops ~indepth:true [];
  Flags.recompute_resources_between_steps := false;
  (* should: this be a transfo instead of trm -> trm ? *)
  (* Matrix.elim_mops ~simpl:(Arith_core.(simplify false Arith_basic.(compose [expand; euclidian; gather_rec; compute]))) []; *)
  Matrix.elim_mops ~simpl:(fun t -> t) [];
  Arith.(simpl_rec expand_rec) [];
  Arith.(simpl_rec (compose [euclidian; compute])) [];
  Arith.(simpl_rec gather_rec) [];
  Arith.(simpl_rec compute) [];
  Resources.delete_annots [];
  Loop.delete_all_void []

