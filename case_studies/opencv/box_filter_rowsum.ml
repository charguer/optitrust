open Optitrust
open Prelude

let _ = Flags.check_validity := false (* TODO: true *)

(* Reproducing OpenCV code from:
   https://github.com/opencv/opencv/blob/4.x/modules/imgproc/src/box_filter.simd.hpp
   *)

let _ = Run.script_cpp (fun () ->
  !! Resources.ensure_computed ();

  !! Resources.delete_annots []; (* TODO: remove *)

  (* FIXME: why not working on fun body? *)
  bigstep "prepare for specialization";
  !! Marks.add "generic" [cFunBody "rowSum"; cFor "c"];
  let specialize (var, n) = begin
    let mark_then = sprintf "%s%i" var n in
    Specialize.variable ~var ~value:(trm_int n) ~mark_then [cFunBody "rowSum"; cMark "generic"];
    Marks.remove "generic" [cMark mark_then; cFor "c"];
  end in
  !! List.iter specialize ["kn", 3; "kn", 5; "cn", 1; "cn", 3];

  bigstep "kn == 3";
  !! Reduce.elim ~inline:true [cMark "kn3"; cFun "reduce_spe1"];
  !! Loop.swap [cMark "kn3"; cFor "c"];
  !! Loop.collapse [cMark "kn3"; cFor "i"];

  bigstep "kn == 5";
  !! Reduce.elim ~inline:true [cMark "kn5"; cFun "reduce_spe1"];
  !! Loop.swap [cMark "kn5"; cFor "c"];
  !! Loop.collapse [cMark "kn5"; cFor "i"];

  bigstep "cn == 1";
  !! Loop.unroll [cMark "cn1"; cFor "c"];
  !! Reduce.slide [cMark "cn1"; cArrayWrite "D"];
  !! Reduce.elim [occFirst; cMark "cn1"; cFun "reduce_spe1"];
  !! Reduce.elim ~inline:true [nbMulti; cMark "cn1"; cFor "i"; cFun "reduce_spe1"];

  bigstep "cn == 3";
  !! Loop.unroll [cMark "cn3"; cFor "c"];
  !! Reduce.slide [nbMulti; cMark "cn3"; cArrayWrite "D"];
  !! Reduce.elim [nbMulti; cMark "cn3"; cVarDef ""; dBody; cFun "reduce_spe1"];
  !! Reduce.elim ~inline:true [nbMulti; cMark "cn3"; cFor "i"; cFun "reduce_spe1"];

  (* TODO: ?
    !! Loop.fusion_targets ~into:FuseIntoLast [nbMulti; cMark "cn3"; cFor "i" ~body:[cArrayWrite "D"]]; *)
  let for_dwrite = [cMark "cn3"; cFor "i" ~body:[cArrayWrite "D"]] in
  !! Loop.fusion_targets ~into:(occLast :: for_dwrite) (nbMulti :: for_dwrite);

  (* TODO: ?
    !! Instr.gather_targets [nbMulti; cMark "cn3"; cStrict; cArrayWrite "D"]; *)
  let dwrite = [cMark "cn3"; cStrict; cArrayWrite "D"] in
  let last_dwrite = resolve_target_exactly_one (occLast :: dwrite) in
  !! Instr.move ~dest:[tBefore; cPath last_dwrite] (nbMulti :: dwrite);

  (* TODO: ?
    !! Loop.fusion_targets ~into:FuseIntoLast [nbMulti; cMark "cn3"; cFor ~stop:[cVar "kn"] "i"]; *)
  let for_kn = [cMark "cn3"; cFor ~stop:[cVar "kn"] "i"] in
  !! Loop.fusion_targets ~into:(occLast :: for_kn) (nbMulti :: for_kn);

  bigstep "cleanup";
  !! Resources.delete_annots [];
  !! Matrix.elim_mops [];
)
