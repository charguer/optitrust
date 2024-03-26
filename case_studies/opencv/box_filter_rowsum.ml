open Optitrust
open Prelude

let _ = Flags.check_validity := false (* TODO: true *)

let _ = Run.script_cpp (fun () ->
  !! Resources.ensure_computed ();

  !! Resources.delete_annots []; (* TODO: remove *)

  (* TODO: kn == 1, kn == 3 *)
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
  !! Reduce.elim ~unroll:true [cMark "kn3"; cArrayWrite "D"];

  bigstep "kn == 5";
  !! Reduce.elim ~unroll:true [cMark "kn5"; cArrayWrite "D"];

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

  !! Resources.delete_annots [];
)
