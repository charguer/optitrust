open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

(** Reproducing OpenCV code from:
   https://github.com/opencv/opencv/blob/4.x/modules/imgproc/src/box_filter.simd.hpp

   Remaining differences:
   - [ic/cn*cn + ic%cn] instead of [ic],
     could fix by arith simpl or with [PROJNM(...)] ops and simpls
   - using [s = s + x - y] instead of [s += s; s -= y], could fix with compound op transfos: Function.use_infix_ops_at, check += encode/decode -- Reduce.slide could also do this
   - [1..n] instead [of 0..(n-1)] loops, could fix by Loop.shift StartAtZero
   - [i++; i * 3|4|cn] instead of [i += 3|4|cn; i], could fix by Loop.scale
   - [k++; + k] instead of [k++, S++, D++], we may not want to introduce such pointer arithmetic.
   - no template support yet for S/ST types; OpenCV also casts inputs from uchar
   *)

(* FIXME: should be done by flag ~elimoptitrust:true *)
let%transfo postprocessing (_u: unit) : unit =
  Trace.tag "pre-post-processing";
  Flags.recompute_resources_between_steps := false;
  Matrix.elim_mops [];
  Resources.delete_annots [];
  Loop.delete_all_void []

let _ = Run.script_cpp (fun () ->
  (* !! Resources.ensure_computed (); *)

  (* FIXME: not working on fun body because need to go inside seq. *)
  bigstep "prepare for specialization";
  let mark_then (var, _value) = sprintf "%s" var in
  !! Specialize.variable_multi ~mark_then ~mark_else:"generic"
    ["kn", trm_int 3; "kn", trm_int 5; "cn", trm_int 1; "cn", trm_int 3; "cn", trm_int 4]
    [cFunBody "rowSum"; cFor "c"];

  bigstep "generic + cn";
  let c = cMarks ["generic"; "cn"] in
  !! Reduce.slide ~mark_alloc:"acc" [nbMulti; c; cArrayWrite "D"];
  !! Reduce.elim [nbMulti; cMark "acc"; cFun "reduce_spe1"];
  !! Variable.elim_reuse [nbMulti; cMark "acc"];
  !! Reduce.elim ~inline:true [nbMulti; c; cFor "i"; cFun "reduce_spe1"];

  bigstep "kn";
  !! Reduce.elim ~inline:true [nbMulti; cMark "kn"; cFun "reduce_spe1"];
  !! Loop.swap [nbMulti; cMark "kn"; cFor "c"];
  !! Loop.collapse [nbMulti; cMark "kn"; cFor "i"];

  bigstep "cn";
  (* !! Reduce.slide ~mark_alloc:"acc" [nbMulti; cMark "cn"; cArrayWrite "D"];
  !! Reduce.elim [nbMulti; cMark "acc"; cFun "reduce_spe1"];
  !! Variable.elim_reuse [nbMulti; cMark "acc"];
  !! Reduce.elim ~inline:true [nbMulti; cMark "cn"; cFor "i"; cFun "reduce_spe1"]; *)
  !! Loop.unroll [nbMulti; cMark "cn"; cFor "c"];
  !! foreach_target [nbMulti; cMark "cn"] (fun c ->
    Loop.fusion_targets ~into:FuseIntoLast [nbMulti; c; cFor "i" ~body:[cArrayWrite "D"]];
    Instr.gather_targets [c; cStrict; cArrayWrite "D"];
    Loop.fusion_targets ~into:FuseIntoLast [nbMulti; c; cFor ~stop:[cVar "kn"] "i"];
    Instr.gather_targets [c; cFor "i"; cArrayWrite "D"];
  );

  bigstep "postprocessing and optional style";
  !! postprocessing ();
)


(* cleanup of accesses

  !! Loop.shift (StartAtZero) [nbMulti; cFor "i"];
  !!! Arith_basic.(simpls_rec [expand; euclidian; gather_rec]) [nbMulti; cAccesses()];
  !! Loop_basic.scale_range ~factor:(trm_int 3) [nbMulti; cIf ~cond:[sExpr "cn == 3"] (); dThen; cFor "i"];
  !! Arith_basic.(simpls_rec [expand; gather_rec]) [nbMulti; cAccesses()];
  !!! Arith_basic.simplify [nbMulti; cAccesses()];
  !! Function.use_infix_ops ~indepth:true [];
*)
