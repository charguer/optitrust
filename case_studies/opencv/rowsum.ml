open Optitrust
(*open Prelude_valid*)
open Prelude
let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let int = trm_int

(**
let%transfo reduce_intro (tg : target) : unit =
  Target.iter (fun p ->
    let p, i = Internal.isolate_last_dir_in_seq p in
    let tg_instr = target_of_path (p @ [Path.Dir_seq_nth i]) in
    (* First mark the top level function that contains the target tg *)
    let mark = "reduce_intro_mark" in
    Marks.add mark (target_of_path p);
    let mark_tg = cMark mark in
    Function.uninline ~contains_for_loop:true ~fct:[cTopFunDef it_fun.name] tg_instr;
    Expr.replace_fun ~inline:true loop_fun [mark_tg; cFun it_fun.name];
    Function.beta ~indepth:true [mark_tg];
    Marks.remove mark [cMark mark]
  ) tg

  void reduce_spe1_loop_out(uint8_t* out, int start, int stop, const uint8_t* in, int n, int m, int j) {
  uint16_t s = 0;
  for (int i = start; i < stop; i++) {
    //__smodifies("&s ~> Cell");
    //__sreads("in ~> Matrix2(n, m)");
    s += in[MINDEX2(n, m, i, j)];
  }
  *out = s;
}





*)

let _ = Run.script_cpp (fun () ->

  !! Function.uninline ~contains_for_loop:true ~fct:[cTopFunDef "reduce_spe1_loop_out"] [cVarDef "s"];

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

