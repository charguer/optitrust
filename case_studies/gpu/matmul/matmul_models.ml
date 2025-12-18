open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_script

(* let _ = Flags.report_exectime := true *)

let bm = 32
let bn  = 32
let bk = 4
let tm = 8
let tn = 4

let current_stage = 1

let _ = if current_stage = 1 then begin
Run.script_cpp (fun () ->
  let def_tile_sizes (tile_dim_name, tile_dim_size) =
    Variable.insert ~name:tile_dim_name ~typ:typ_int ~value:(trm_int tile_dim_size) [tFirst]
  in
    !! List.iter def_tile_sizes [
      "tm", tm;
      "tn", tn;
      "bk", bk;
      "bn", bn;
      "bm", bm
    ];

  !! Matrix.local_name_tile ~uninit_pre:true ~var:"c" ~local_var:"c_gmem" [cFor "i"];
  !! Matrix.local_name_tile ~uninit_post:true ~var:"a" ~local_var:"a_gmem" [cFor "i"];
  !! Matrix.local_name_tile ~uninit_post:true ~var:"b" ~local_var:"b_gmem" [cFor "i"];

  let rec tiles (loop_id, tile_name_sizes) =
    match tile_name_sizes with
    | (tile_name, tile_size) :: rest ->
      Loop.tile tile_size ~index:tile_name ~bound:TileDivides [cFor loop_id];
      tiles (loop_id, rest)
    | [] -> ()
    in
  !! List.iter tiles [
    "i", ["bi", (trm_find_var "bm" []); "ti", (trm_find_var "tm" [])]; (*trm_int tm)]; *)
    "j", ["bj", (trm_find_var "bn" []); "tj", (trm_find_var "tn" [])];
    "k", ["bkIdx", (trm_find_var "bk" [])]
  ];
  (* FIXME using vars for tm and tn breaks reordering. *)
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
