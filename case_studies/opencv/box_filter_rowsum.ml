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

(* FIXME: removing cFor from specialize targets is not working, because we need to go inside seq. *)

let only = -1
(* comment next line for doing all versions *)
(* let only = 2 *)

let fast = if only <> -1 then [occIndex only] else [nbMulti]

let _ = Run.script_cpp (fun () ->
  !! Reduce.intro [cVarDef "s"];

  let mark_then (var, _value) = sprintf "%s" var in
  !! Specialize.variable_multi ~mark_then ~mark_else:"nokn"
    ["kn", int 3; "kn", int 5] [cFunBody "rowSum"; cFor "i"];
  !! Reduce.elim ~inline:true [nbMulti; cMark "kn"; cCall "reduce_spe1"];
  !! Loop.collapse [nbMulti; cMark "kn"; cFor "i"];

  !! Loop.swap [nbMulti; cMark "nokn"; cFor "i"];
  !! Reduce.slide ~mark_alloc:"acc" [nbMulti; cMark "nokn"; cArrayWrite "D"];
  !! Reduce.elim [nbMulti; cMark "acc"; cCall "reduce_spe1"];

  !! Variable.elim_reuse [nbMulti; cMark "acc"];
  !! Reduce.elim ~inline:true [nbMulti; cMark "nokn"; cFor "i"; cCall "reduce_spe1"];
  !! Loop.shift_range (StartAtZero) [nbMulti; cMark "nokn"; cFor "i"];
  !! Loop.scale_range ~factor:(trm_find_var "cn" []) [nbMulti; cMark "nokn"; cFor "i"];
  (* TODO:
    - insert to_prove instead of warning for / 0.
   *)

  !! Specialize.variable_multi ~mark_then ~simpl:custom_specialize_simpl
    ["cn", int 1; "cn", int 3; "cn", int 4] [cMark "nokn"; cFor "c"];
  !! Loop.unroll [nbMulti; cMark "cn"; cFor "c"];

  !! Target.foreach (fast@[cMark "cn"]) (fun c ->
    Loop.fusion_targets ~into:FuseIntoLast [nbMulti; c; cFor "i" ~body:[cArrayWrite "D"]];
    Instr.gather_targets [c; cStrict; cArrayWrite "D"];
    Loop.fusion_targets ~into:FuseIntoLast [nbMulti; c; cFor ~stop:[cVar "kn"] "i"];
    Instr.gather_targets [c; cFor "i"; cArrayWrite "D"];

  );
    (**
  (* TODO? simpl ~unfold_alias:true *)
  (* TODO: [expand_rec; gather_rec; compute] *)
  !! Cleanup.std ();
  *)
)
