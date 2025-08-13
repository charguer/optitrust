open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true

(* Generated trace is too heavy. Keep only the steps of the transformation script *)
let _ = Flags.save_steps := Some Steps_script
(*
let _ = Flags.save_steps := Some Flags.Steps_effectful
let _ = Flags.save_ast_for_steps := Some Flags.Steps_effectful
*)

(** Reproducing OpenCV code from:
  https://github.com/opencv/opencv/blob/4.10.0/modules/imgproc/src/box_filter.simd.hpp#L75

   Remaining differences:
   - [k++; + k] instead of [k++, S++, D++], we may not want to introduce such pointer arithmetic.
   - no template support yet for S/ST types; OpenCV also casts inputs from uchar

   c.f. README.md
   *)

let int = trm_int

let custom_specialize_simpl tg =
  Trace.without_resource_computation_between_steps (fun () ->
    Arith.default_simpl tg;
    Loop.simplify_all_ghosts_group_scale [];
  )

let no_simpl (tg : target) : unit = ()

let _ = Run.script_cpp (fun () ->
  !! Specialize.variable_multi ~mark_then:fst ~mark_else:"anyw"
    ["w", int 3; "w", int 5] [cFunBody "rowSum"; cFor "i"];
  (* TODO:
    !! Reduce.elim ~inline:true [nbMulti; cMark "w"; cCall "reduce_spe1"];

    MAYBE DO AT VERY END AFTER ERASING ANNOTATIONS ?

    is following unroll + fold adds
  !! Loop.unroll [nbMulti; cMark "w"; cFor "k"];
  *)
  !! Loop.collapse [nbMulti; cMark "w"; cFor "i"];

  !! Loop.swap [nbMulti; cMark "anyw"; cFor "i"];
  (* TODO: would need to make slide less syntax-driven to enable simpl again *)
  !! Loop.unroll_first_iteration ~simpl:no_simpl [nbMulti; cMark "anyw"; cFor "i"];
  !! Reduce_models.slide ~mark_alloc:"acc" [nbMulti; cMark "anyw"; cFor "i"];
  (* TODO: do that in slide combi *)
  !! Resources.make_strict_loop_contracts [nbMulti; cMark "anyw"; cFor "i"];

  !! Variable.elim_reuse [nbMulti; cMark "acc"];
  (* TODO: MAYBE DO AT VERY END AFTER ERASING ANNOTATIONS ?

  !! Loop.shift_range (StartAtZero) [nbMulti; cMark "anyw"; cFor "i"];
  !! Loop.scale_range ~factor:(trm_find_var "cn" []) [nbMulti; cMark "anyw"; cFor "i"]; *)
  !! Specialize.variable_multi ~mark_then:fst ~mark_else:"anycn" ~simpl:custom_specialize_simpl
    ["cn", int 1; "cn", int 3; "cn", int 4] [cMark "anyw"; cFor "c"];

  !! Loop.unroll [nbMulti; cMark "cn"; cFor "c"];
  !! Target.foreach [nbMulti; cMark "cn"] (fun c ->
    Loop.fusion_targets ~into:FuseIntoLast [nbMulti; c; cFor "i"];
    Instr.gather_targets [c; cStrict; cArrayWrite "d"];
    Loop.fusion_targets ~into:FuseIntoLast [nbMulti; c; cFor "k"];
    Instr.gather_targets [c; cFor "i"; cArrayWrite "d"];
  );

  (* TODO: MAYBE DO AT VERY END AFTER ERASING ANNOTATIONS ?
  !! Loop.shift_range (ShiftBy (trm_find_var "c" [cMark "anycn"])) [cMark "anycn"; cFor ~body:[cArrayWrite "D"] "i"];
*)
)

