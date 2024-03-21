open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->
  bigstep "prepare for specialization";
  !! Function.inline_def [cFunDef "rowSum"];

  bigstep "cn == 1";
  let cn1 = resolve_target_exactly_one [cFunBody "rowSum_cn1"] in
  (* let cn1 = resolve_target_exactly_one_with_stringreprs_available [cIf ~cond:[sExpr "cn == 1"] (); dThen] (Trace.ast ()) in *)
  !! Loop.unroll [cPath cn1; cFor "c"];

  bigstep "cn == 3";
  let cn3 = resolve_target_exactly_one [cFunBody "rowSum_cn3"] in
  (* let cn3 = resolve_target_exactly_one_with_stringreprs_available [cIf ~cond:[sExpr "cn == 3"] (); dThen] (Trace.ast ()) in *)
  !! Loop.unroll [cPath cn3; cFor "c"];

  (* TODO: !! Loop.fusion_targets ~into:FuseIntoLast [nbMulti; cPath cn3; cFor "i" ~body:[cArrayWrite "D"]]; *)
  let for_dwrite = [cPath cn3; cFor "i" ~body:[cArrayWrite "D"]] in
  !! Loop.fusion_targets ~into:(occLast :: for_dwrite) (nbMulti :: for_dwrite);

  (* TODO: !! Instr.gather_targets [nbMulti; cPath cn3; cStrict; cArrayWrite "D"]; *)
  let dwrite = [cPath cn3; cStrict; cArrayWrite "D"] in
  let last_dwrite = resolve_target_exactly_one (occLast :: dwrite) in
  !! Instr.move ~dest:[tBefore; cPath last_dwrite] (nbMulti :: dwrite);

  (* TODO: !! Loop.fusion_targets ~into:FuseIntoLast [nbMulti; cPath cn3; cFor ~stop:[cVar "kn"] "i"]; *)
  let for_kn = [cPath cn3; cFor ~stop:[cVar "kn"] "i"] in
  !! Loop.fusion_targets ~into:(occLast :: for_kn) (nbMulti :: for_kn);

  !! Resources.delete_annots [];
)
