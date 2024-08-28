open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := true

(* Reproducing a TVM schedule from:
   https://tvm.apache.org/docs/how_to/optimize_operators/opt_gemm.html

   1. improve data locality by blocking the computation of C and preloading B with a packed memory layout
   2. unroll loops and introduce parallelism with vectorization and multi-threading

   c.f. README.md
*)

let int = trm_int

(* FIXME: avoid inlining *)
let _ = Run.script_cpp (fun () ->

  !! Function.inline_def [cFunDef "mm"];
  let tile (loop_id, tile_size) = Loop.tile (int tile_size)
    ~index:("b" ^ loop_id) ~bound:TileDivides [cFor loop_id] in
  !! List.iter tile [("i", 32); ("j", 32); ("k", 4)];
  !! Loop.reorder_at ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cPlusEq ~lhs:[cVar "sum"] ()];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "bi"] "pB" ~indep:["bi"; "i"] [cArrayRead "B"];
  !!! Matrix.stack_copy ~var:"sum" ~copy_var:"s" ~copy_dims:1
    [cFor ~body:[cPlusEq ~lhs:[cVar "sum"] ()] "k"];
  !! Omp.simd [nbMulti; cFor ~body:[cPlusEq ~lhs:[cVar "s"] ()] "j"];
  !! Omp.parallel_for [nbMulti; cFunBody ""; cStrict; cFor ""];
  !! Loop.unroll ~simpl:Arith.do_nothing [cFor ~body:[cPlusEq ~lhs:[cVar "s"] ()] "k"];

  !! Cleanup.std ();
)

(* LATER: are the "~lhs" necessary ? *) *
