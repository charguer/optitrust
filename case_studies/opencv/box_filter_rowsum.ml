open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->
  bigstep "prepare for specialization";
  !! Function.inline_def [cFunDef "rowSum"];

  bigstep "cn == 1";
  let cn1 = resolve_target_exactly_one_with_stringreprs_available [cIf ~cond:[sExpr "cn == 1"] (); dThen] (Trace.ast ()) in
  !! Loop.unroll [cPath cn1; cFor "k"];

  bigstep "cn == 3";
  let cn3 = resolve_target_exactly_one_with_stringreprs_available [cIf ~cond:[sExpr "cn == 3"] (); dThen] (Trace.ast ()) in
  !! Loop.unroll [cPath cn3; cFor "k"];
  let for_dwrite = [cPath cn3; cFor "i" ~body:[cArrayWrite "D"]] in
  !! Loop.fusion_targets ~into:(occLast :: for_dwrite) (nbMulti :: for_dwrite);
  let dwrite = [cPath cn3; cStrict; cArrayWrite "D"] in
  let last_dwrite = Xlist.take_last 1 (resolve_target_exactly_one (occLast :: dwrite)) in
  !! Instr.move_in_seq ~dest:[tBefore; cPath last_dwrite] (nbMulti :: dwrite);
  let for_ksize = [cPath cn3; cFor ~stop:[cVar "ksize"] "i"] in
  !! Loop.fusion_targets ~into:(occLast :: for_ksize) (nbMulti :: for_ksize);
)
