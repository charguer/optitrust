open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true

(** Reproducing OpenCV code from:
   https://github.com/opencv/opencv/blob/4.x/modules/imgproc/src/box_filter.simd.hpp

   Remaining differences:
   - [ic/cn*cn + ic%cn] instead of [ic],
     could fix by arith simpl or with [PROJNM(...)] ops and simpls
   - [1..n] instead [of 0..(n-1)] loops, could fix by Loop.shift StartAtZero
   - [i++; i * 3|4|cn] instead of [i += 3|4|cn; i], could fix by Loop.scale
   - [k++; + k] instead of [k++, S++, D++], we may not want to introduce such pointer arithmetic.
   - no template support yet for S/ST types; OpenCV also casts inputs from uchar
   *)

let int = trm_int

(* FIXME: removing cFor from specialize targets is not working, because we need to go inside seq. *)

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

  !! Specialize.variable_multi ~mark_then
    ["cn", int 1; "cn", int 3; "cn", int 4] [cMark "nokn"; cFor "c"];
  !! Loop.unroll [nbMulti; cMark "cn"; cFor "c"];
  !! Target.foreach [nbMulti; cMark "cn"] (fun c ->
    Loop.fusion_targets ~into:FuseIntoLast [nbMulti; c; cFor "i" ~body:[cArrayWrite "D"]];
    Instr.gather_targets [c; cStrict; cArrayWrite "D"];
    Loop.fusion_targets ~into:FuseIntoLast [nbMulti; c; cFor ~stop:[cVar "kn"] "i"];
    Instr.gather_targets [c; cFor "i"; cArrayWrite "D"];
  );

(* cleanup of accesses

  !! Loop.shift_range (StartAtZero) [nbMulti; cFor "i"];
  !! Loop_basic.scale_range ~factor:(trm_int 3) [nbMulti; cIf ~cond:[sExpr "cn == 3"] (); dThen; cFor "i"];
*)
  (* loop scale / shift /  simpl ~unfold_alias:true *)
  (* Loop.scale_range ~factor:? [];
  Loop.shift_range ~factor:? []; *)

  !! Cleanup.std ();
)
