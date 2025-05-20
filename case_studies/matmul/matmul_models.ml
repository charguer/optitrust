open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_important

(* let _ = Flags.report_exectime := true *)

(* Reproducing a TVM schedule for matrix multiplication:
   1. improve data locality by blocking the computation of C and preloading B with a packed memory layout
   2. unroll loops and introduce parallelism with vectorization and multi-threading

   c.f. README.md
*)

let int = trm_int

let _ = Run.script_cpp (fun () ->
  !! Function.inline_def [cFunDef "mm"];
  let tile (loop_id, tile_size) = Loop.tile (int tile_size)
    ~index:("b" ^ loop_id) ~bound:TileDivides [cFor loop_id] in
  !! List.iter tile [("i", 32); ("j", 32); ("k", 4)];
  !! Loop.reorder_at ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cPlusEq ~lhs:[cVar "sum"] ()];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "bi"] "bT" ~indep:["bi"; "i"] [cArrayRead "b"];
  !! Matrix.stack_copy ~var:"sum" ~copy_var:"s" ~copy_dims:1
    [cFor ~body:[cPlusEq ~lhs:[cVar "sum"] ()] "k"];
  !! Loop.simd [nbMulti; cFor ~body:[cPlusEq ~lhs:[cVar "s"] ()] "j"];
  !! Loop.parallel [nbMulti; cFunBody ""; cStrict; cFor ""];
  !! Loop.unroll ~simpl:Arith.do_nothing [cFor ~body:[cPlusEq ~lhs:[cVar "s"] ()] "k"];
)
