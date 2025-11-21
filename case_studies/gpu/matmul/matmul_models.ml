open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.pretty_matrix_notation := true
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

let bm = 32

let current_stage = 1

let _ = if current_stage = 1 then begin
Run.script_cpp (fun () ->
  (* !! Matrix.local_name ~type_and_dims:(~var:"c" ~local_var:"c_gmem" [cFor "i"]; *)
  !! Matrix.local_name_tile ~uninit_pre:true ~var:"c" ~local_var:"c_gmem" [cFor "i"];
  !! Matrix.local_name_tile ~uninit_post:true ~var:"a" ~local_var:"a_gmem" [cFor "i"];
  !! Matrix.local_name_tile ~uninit_post:true ~var:"b" ~local_var:"b_gmem" [cFor "i"];

  let rec tiles (loop_id, tile_name_sizes) =
    match tile_name_sizes with
    | (tile_name, tile_size) :: rest ->
      Loop.tile (int tile_size) ~index:tile_name ~bound:TileDivides [cFor loop_id];
      tiles (loop_id, rest)
    | [] -> ()
    in
  !! List.iter tiles [
    "i", ["bi", bm; "ti", 8];
    "j", ["bj", 32; "tj", 4];
    "k", ["bkIdx", 4]
  ];
  !! Loop.reorder_at ~order:["bi"; "bj"; "bkIdx"; "ti"; "tj"; "k"; "i"; "j"] [cPlusEq ~lhs:[cVar "sum"] ()];

  !! Cleanup.std ()
  (* !! Matrix.local_name_tile ~uninit_post:true ~var:"a_gmem" ~local_var:"a_smem" [cFor "bkIdx"; cFor "ti"]; *)
  (* !! Matrix.local_name_tile ~uninit_post:true ~var:"b_gmem" ~local_var:"b_smem" [cFor "bkIdx"; cFor "ti"]; *)
)
  end

let _ =  if current_stage = 2 then begin
Run.script_cpp ~filename:"matmul_models_out.cpp" (fun () ->
  !! Loop.hoist_expr ~dest:[tBefore; cFor "bkIdx"; cFor "ti"] "a_smem" ~indep:["j"; "tj"] [cArrayRead "a_gmem"];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "bkIdx"; cFor "ti"] "b_smem" ~indep:["i"; "ti"] [cArrayRead "b_gmem"];

  !! Loop.hoist_expr ~dest:[tBefore; cFor "bkIdx"; cFor "i" ~body:[cPlusEq ~lhs:[cVar "sum"] ()]]
    "a_regs" ~indep:["j"] [cArrayRead "a_smem"];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "bkIdx"; cFor "i" ~body:[cPlusEq ~lhs:[cVar "sum"] ()]]
    "b_regs" ~indep:["i"] [cArrayRead "b_smem"];

  !! Cleanup.std ();
  (*
  !! Loop.hoist_expr ~dest:[tBefore; cFor "bi"] "bT" ~indep:["bi"; "i"] [cArrayRead "b"];
  !! Matrix.stack_copy ~var:"sum" ~copy_var:"s" ~copy_dims:1
    [cFor ~body:[cPlusEq ~lhs:[cVar "sum"] ()] "k"];
  !! Loop.simd [nbMulti; cFor ~body:[cPlusEq ~lhs:[cVar "s"] ()] "j"];
  !! Loop.parallel [nbMulti; cFunBody ""; cStrict; cFor ""];
  !! Loop.unroll ~simpl:Arith.do_nothing [cFor ~body:[cPlusEq ~lhs:[cVar "s"] ()] "k"];
  *)
)
end
