open Optitrust
open Target
open Ast

let _ = Flags.pretty_matrix_notation := true

(* Reproducing a TVM schedule from:
   https://tvm.apache.org/docs/how_to/optimize_operators/opt_gemm.html

   1. improve data locality by blocking the computation of C and preloading B with a packed memory layout
   2. unroll loops and introduce parallelism with vectorization and multi-threading

   c.f. README.md
*)

let _ = Run.script_cpp (fun () ->
  let tile (loop_id, tile_size) = Loop.tile (trm_int tile_size) ~index:("b" ^ loop_id) ~bound:TileDivides [cFor loop_id] in
  !! List.iter tile [("i", 32); ("j", 32); ("k", 4)];
  !! Loop.reorder_at ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cPlusEqVar "sum"];
  !!! Loop.hoist_expr ~dest:[tBefore; cFor "bi"] "pB" ~indep:["bi"; "i"] [cArrayRead "B"];
  !! Function.inline_def [cFunDef "mm"];
  !!! Matrix.stack_copy ~var:"sum" ~copy_var:"s" ~copy_dims:1 [cFor ~body:[cPlusEqVar "sum"] "k"];
  !! Matrix.elim_mops [];
  !! Loop.unroll [cFor ~body:[cPlusEqVar "s"] "k"];
  !! Omp.simd [nbMulti; cFor ~body:[cPlusEqVar "s"] "j"];
  !! Omp.parallel_for [nbMulti; cFunBody "mm1024"; cStrict; cFor ""];
)