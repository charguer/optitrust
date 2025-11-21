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
    "i", ["bi", 32];
    "j", ["bj", 32];
    "k", ["bk", 4]
  ];

  (*
  !! Function.inline_def [cFunDef "mm"];
  !! Loop.reorder_at ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cPlusEq ~lhs:[cVar "sum"] ()];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "bi"] "bT" ~indep:["bi"; "i"] [cArrayRead "b"];
  !! Matrix.stack_copy ~var:"sum" ~copy_var:"s" ~copy_dims:1
    [cFor ~body:[cPlusEq ~lhs:[cVar "sum"] ()] "k"];
  !! Loop.simd [nbMulti; cFor ~body:[cPlusEq ~lhs:[cVar "s"] ()] "j"];
  !! Loop.parallel [nbMulti; cFunBody ""; cStrict; cFor ""];
  !! Loop.unroll ~simpl:Arith.do_nothing [cFor ~body:[cPlusEq ~lhs:[cVar "s"] ()] "k"];
  *)
)
