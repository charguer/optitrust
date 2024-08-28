open Optitrust
open Prelude
let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let int = trm_int


let _ = Run.script_cpp (fun () ->

  !! Specialize.variable_multi ~mark_then:fst ~mark_else:"nokn"
    ["kn", int 3; "kn", int 5] [cFunBody "rowSum"; cFor "i"];
  !! Reduce.elim ~inline:true [nbMulti; cMark "kn"; cFun "reduce_spe1"];
  !! Loop.collapse [nbMulti; cMark "kn"; cFor "i"];

  !! Loop.swap [nbMulti; cMark "nokn"; cFor "i"];
  !! Reduce.slide ~mark_alloc:"acc" [nbMulti; cMark "nokn"; cArrayWrite "D"];
  !! Reduce.elim [nbMulti; cMark "acc"; cFun "reduce_spe1"];
  !! Variable.elim_reuse [nbMulti; cMark "acc"];
  !! Reduce.elim ~inline:true [nbMulti; cMark "nokn"; cFor "i"; cFun "reduce_spe1"];

  !! Specialize.variable_multi ~mark_then:fst
    ["cn", int 1; "cn", int 3; "cn", int 4] [cMark "nokn"; cFor "c"];
  !! Loop.unroll [nbMulti; cMark "cn"; cFor "c"];
  !! Target.foreach [nbMulti; cMark "cn"] (fun c ->
      Loop.fusion_targets ~into:FuseIntoLast [nbMulti; c; cFor "i" ~body:[cArrayWrite "D"]];
      Instr.gather_targets [c; cStrict; cArrayWrite "D"];
      Loop.fusion_targets ~into:FuseIntoLast [nbMulti; c; cFor ~stop:[cVar "kn"] "i"];
      Instr.gather_targets [c; cFor "i"; cArrayWrite "D"];
  );

  !! Cleanup.std ();
)

