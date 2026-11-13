open Optitrust
open Prelude

let _ = Flags.typechecking_mode := Flags.ProofPreserving
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_script


(* let _ = Flags.report_exectime := true *)

(* Reproducing a TVM schedule for matrix multiplication:
   1. improve data locality by blocking the computation of C and preloading B with a packed memory layout
   2. unroll loops and introduce parallelism with vectorization and multi-threading

   c.f. README.md
*)

let int = trm_int

let _ = Run.script_cpp (fun () ->
  !! Function.inline_def [cFunDef "mm"];
  let tile (id, tile_size) =
    Loop.tile (int tile_size) ~index:("b" ^ id) ~bound:TileDivides [cFor id] in
  !! List.iter tile [("i", 32); ("j", 32); ("k", 4)];
  !! Loop.reorder_at ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cPlusEq ()];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "bi"] "pB" ~indep:["bi"; "i"] [cArrayRead "b"];
  !! Matrix.stack_copy ~var:"sum" ~copy_var:"s" ~copy_dims:1 [cFor ~body:[cPlusEq ()] "k"];
  !! Loop.simd [cFor ~body:[cPlusEq ()] "j"];
  !! Loop.parallel [nbMulti; cFunBody "mm1024"; cStrict; cFor ""];
  !! Loop.unroll ~simpl:Arith.no_simpl [cFor ~body:[cPlusEq ()] "k"];
  !! Cleanup.std ()
)
