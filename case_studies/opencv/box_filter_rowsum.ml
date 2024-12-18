open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true

(** Reproducing OpenCV code from:
   https://github.com/opencv/opencv/blob/4.x/modules/imgproc/src/box_filter.simd.hpp

   Remaining differences:
   - [k++; + k] instead of [k++, S++, D++], we may not want to introduce such pointer arithmetic.
   - no template support yet for S/ST types; OpenCV also casts inputs from uchar
   *)

let int = trm_int

let custom_specialize_simpl tg =
  Trace.without_resource_computation_between_steps (fun () ->
    Arith.default_simpl tg;
    Loop.simplify_all_ghosts_group_scale [];
  )

let only = -1
(* comment next line for doing all versions *)
 (* let only = 2 *)

let fast = if only <> -1 then [occIndex only] else [nbMulti]

let _ = Run.script_cpp (fun () ->
  !! Reduce.intro [cVarDef "s"];

  !! Specialize.variable_multi ~mark_then:fst ~mark_else:"anyw"
    ["w", int 3; "w", int 5] [cFunBody "rowSum"; cFor "i"];
  !! Reduce.elim ~inline:true [nbMulti; cMark "w"; cCall "reduce_spe1"];
  !! Loop.collapse [nbMulti; cMark "w"; cFor "i"];

  !! Loop.swap [nbMulti; cMark "anyw"; cFor "i"];
  !! Reduce.slide ~mark_alloc:"acc" [nbMulti; cMark "anyw"; cArrayWrite "D"];
  !! Reduce.elim [nbMulti; cMark "acc"; cCall "reduce_spe1"];

  !! Variable.elim_reuse [nbMulti; cMark "acc"];
  !! Reduce.elim ~inline:true [nbMulti; cMark "anyw"; cFor "i"; cCall "reduce_spe1"];
  !! Loop.shift_range (StartAtZero) [nbMulti; cMark "anyw"; cFor "i"];
  !! Loop.scale_range ~factor:(trm_find_var "cn" []) [nbMulti; cMark "anyw"; cFor "i"];
  !! Specialize.variable_multi ~mark_then:fst ~mark_else:"anycn" ~simpl:custom_specialize_simpl
    ["cn", int 1; "cn", int 3; "cn", int 4] [cMark "anyw"; cFor "c"];
  !! Loop.unroll [nbMulti; cMark "cn"; cFor "c"];
  !! Target.foreach (fast@[cMark "cn"]) (fun c ->
    Loop.fusion_targets ~into:FuseIntoLast [nbMulti; c; cFor "i" ~body:[cArrayWrite "D"]];
    Instr.gather_targets [c; cStrict; cArrayWrite "D"];
    Loop.fusion_targets ~into:FuseIntoLast [nbMulti; c; cFor ~stop:[cVar "w"] "i"];
    Instr.gather_targets [c; cFor "i"; cArrayWrite "D"];
  );
  !! Loop.shift_range (ShiftBy (trm_find_var "c" [cMark "anycn"])) [cMark "anycn"; cFor ~body:[cArrayWrite "D"] "i"];
  !! Cleanup.std ();

)

