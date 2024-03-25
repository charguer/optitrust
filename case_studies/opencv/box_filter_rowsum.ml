open Optitrust
open Prelude

let _ = Flags.check_validity := false (* TODO: true *)

let _ = Run.script_cpp (fun () ->
  !! Resources.delete_annots [];

  (* TODO: kn == 1, kn == 3 *)
  (* FIXME: why not working on fun body? *)
  bigstep "prepare for specialization";
  !! Specialize.variable ~var:"cn" ~value:(trm_int 1) ~mark_then:"cn1" ~mark_else:"generic" [cFunBody "rowSum"; cFor "c"];
  !! Specialize.variable ~var:"cn" ~value:(trm_int 3) ~mark_then:"cn3" [cFunBody "rowSum"; cMark "generic"];

  bigstep "cn == 1";
  !! Loop.unroll [cMark "cn1"; cFor "c"];

  bigstep "cn == 3";
  !! Loop.unroll [cMark "cn3"; cFor "c"];

  (* TODO: !! Loop.fusion_targets ~into:FuseIntoLast [nbMulti; cMark "cn3"; cFor "i" ~body:[cArrayWrite "D"]]; *)
  let for_dwrite = [cMark "cn3"; cFor "i" ~body:[cArrayWrite "D"]] in
  !! Loop.fusion_targets ~into:(occLast :: for_dwrite) (nbMulti :: for_dwrite);

  (* TODO: !! Instr.gather_targets [nbMulti; cMark "cn3"; cStrict; cArrayWrite "D"]; *)
  let dwrite = [cMark "cn3"; cStrict; cArrayWrite "D"] in
  let last_dwrite = resolve_target_exactly_one (occLast :: dwrite) in
  !! Instr.move ~dest:[tBefore; cPath last_dwrite] (nbMulti :: dwrite);

  (* TODO: !! Loop.fusion_targets ~into:FuseIntoLast [nbMulti; cMark "cn3"; cFor ~stop:[cVar "kn"] "i"]; *)
  let for_kn = [cMark "cn3"; cFor ~stop:[cVar "kn"] "i"] in
  !! Loop.fusion_targets ~into:(occLast :: for_kn) (nbMulti :: for_kn);

  (* !! Resources.delete_annots []; *)
)
