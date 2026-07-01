open Optitrust
open Prelude

let _ = Flags.check_validity := true (* FIXME: this flag behaviour needs to be cleaned up. *)
let _ = Flags.pretty_matrix_notation := false
let _ = Flags.recompute_resources_between_steps := false
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_script

(* let _ = Flags.report_exectime := true *)
let stage_ok = fun i -> i = 6

let bm = 32
let bn  = 32
let bk = 4
let tm = 8
let tn = 4

let _ = Run.script_cpp_stage stage_ok (fun () ->
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
  !! Matrix.memcpy [nbMulti; cFor "i1"];

  let rec tiles (loop_id, tile_name_sizes) =
    match tile_name_sizes with
    | (tile_name, tile_size) :: rest ->
      Loop.tile (trm_int tile_size) ~index:tile_name ~bound:TileDivides [cFor loop_id];
      tiles (loop_id, rest)
    | [] -> ()
    in
  !! List.iter tiles [
    "i", ["bi", bm; "ti", tm];
    "j", ["bj", bn; "tj", tn];
    "k", ["bkIdx", bk]
  ];
  (* FIXME using vars for tm and tn breaks reordering. *)
  !! Loop.reorder_at ~order:["bi"; "bj"; "bkIdx"; "ti"; "tj"; "k"; "i"; "j"] [cPlusEq ~lhs:[cVar "sum"] ()];
)

let _ = Run.script_cpp_stage stage_ok (fun () ->
  !! Loop.hoist_expr ~dest:[tBefore; cFor "bkIdx"; cFor ~body:[cPlusEq ()] "ti"] "a_smem" ~indep:["j"; "tj"] [cArrayRead "a_gmem"];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "bkIdx"; cFor ~body:[cPlusEq ()] "ti"] "b_smem" ~indep:["i"; "ti"] [cArrayRead "b_gmem"];

  !! Loop.hoist_alloc ~dest:[tBefore; cFor ~body:[cPlusEq ()] "bi"] ~indep:["bkIdx"] [cVarDef "a_smem"];
  !! Loop.hoist_alloc ~dest:[tBefore; cFor ~body:[cPlusEq ()] "bi"] ~indep:["bkIdx"] [cVarDef "b_smem"];
)

let _ =  Run.script_cpp_stage stage_ok (fun () ->
  !! Loop.hoist_expr ~dest:[tBefore; cFor "bkIdx"; cFor "i" ~body:[cPlusEq ~lhs:[cVar "sum"] ()]]
    "a_regs" ~indep:["j"] [cArrayRead "a_smem"];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "bkIdx"; cFor "i" ~body:[cPlusEq ~lhs:[cVar "sum"] ()]]
    "b_regs" ~indep:["i"] [cArrayRead "b_smem"];

  (* !! Cleanup.std (); *)
  (* NOTE (a_smem / b_smem loads):
  - The first loop nest should still be reordered to move k inside (ti; i; k), which would basically be the vectorized dimension. The i index would become tj. One can also see this as collapsing ti; i as the thread flat dimension.
  - The second loop nest seems to be in the right order to me, where j would basically be the vectorized dimension. The tj; k indices would become ti; tj, in other words collapse into the flat thread dimension.
  *)
)

let _ = Run.script_cpp_stage stage_ok (fun () ->
  (* Construct terms to pass to kernel_launch *)
  (* LATER: cleaner frontend for building terms *)
  let t_m, t_n, t_bm, t_bn, t_bk, t_tm, t_tn = (
    let v name = trm_find_var name [cFunDef "mm"] in
    let i v = trm_int v in
    (v "m", v "n", i bm, i bn, i bk, i tm, i tn)
  ) in

  let tpb = [trm_exact_div_int t_bm t_tm; trm_exact_div_int t_bn t_tn] in
  let bpg = [trm_exact_div_int t_m t_bm; trm_exact_div_int t_n t_bn] in
  (* sizeof(float) * 32 * 32 *)
  let smem_szs = [
    trm_mul_int (trm_sizeof typ_f32)
      (trm_mul_int t_bk (trm_mul_int (trm_int (bm/tm)) t_tm));
    trm_mul_int (trm_sizeof typ_f32)
      (trm_mul_int t_bk (trm_mul_int (trm_int (bn/tn)) t_tn))
  ] in

  (* Wrap kernel body in launch and kill calls *)
  !! Gpu.create_kernel_launch bpg tpb smem_szs
    ~setup_end:[tBefore; cFor "bi"] ~teardown_begin:[tAfter; cFor "bi"]
    [tBefore; cVarDef "a_smem"] [tAfter; cPrimCall Prim_delete ~args:[[cVar "a_smem"]]];

  (* !! Gpu.convert_tail_thread_for [1] [occFirst; cFor "bi"; cFor "ti"];
  !! Gpu.convert_tail_thread_for [1;1] [occFirst; cFor "bi"; cFor "bj"; cFor "ti"]; *)

  !! Gpu.convert_tail_thread_for [1] [occFirst; cFor "ti"; cFor ~body:[cWrite ~lhs:[cVar "sum"] ()] "tj"];
  !! Gpu.convert_tail_thread_for [0;1] [cFor "ti"; cFor "k"; cFor ~body:[cWrite ~lhs:[cVar "a_smem"] ()] "i"];
  !! Gpu.convert_tail_thread_for [1] [cFor "tj"; cFor ~body:[cWrite ~lhs:[cVar "b_smem"] ()] "k"];
)

let _ = Run.script_cpp_stage stage_ok (fun () ->
  !! Gpu.convert_tail_thread_for [1] [cFor "ti"; cFor ~body:[cPlusEq ~lhs:[cVar "sum"] ()] "tj"]; (* occLast; cWrite *)
  !! Gpu.convert_tail_thread_for [1] [cFor "ti"; cFor ~body:[cWrite ~lhs:[cVar "c_gmem"] ()] "tj"];
  !! Gpu.convert_tail_thread_for [1] [cFor "bi"; cFor "bj"];
)

let _ = Run.script_cpp_stage stage_ok (fun () ->
  !! Gpu.convert_magic_thread_fors ~patch_steps:(fun () ->
    Gpu.insert_threadsctx_rewrite
      (Matrix_trm.msize [(trm_exact_div_int (trm_int 32) (trm_int 8));
        (trm_exact_div_int (trm_int 32) (trm_int 4))])
      (Matrix_trm.msize [(trm_int 4); (trm_int 8)])
      [tBefore; occFirst; cFor ~body:[cWrite ~lhs:[cVar "sum"] ()] "ti"];
    Gpu.insert_threadsctx_rewrite
      (Matrix_trm.msize [(trm_int 4); (trm_int 8)])
      (Matrix_trm.msize [(trm_int 8); (trm_int 4)])
      [tBefore; cFor ~body:[cWrite ~lhs:[cVar "b_smem"] ()] "tj"];
    Gpu.insert_threadsctx_rewrite
      (Matrix_trm.msize [(trm_int 8); (trm_int 4)])
      (Matrix_trm.msize [(trm_int 4); (trm_int 8)])
      [tBefore; cFor ~body:[cPlusEq ~lhs:[cVar "sum"] ()] "ti"];
    Gpu.insert_threadsctx_rewrite
      (Matrix_trm.msize [(trm_int 4); (trm_int 8)])
      (Matrix_trm.msize [(trm_exact_div_int (trm_int 32) (trm_int 8));
        (trm_exact_div_int (trm_int 32) (trm_int 4))])
      [tLast; cForBody "bj"];
  ) [nbAny; cFunBody "mm"; cFor ""];

  !! Gpu.convert_to_global_mem [nbMulti; cVarDefs ["a_gmem"; "b_gmem"; "c_gmem"]];
  !! Gpu.convert_to_shared_mem ~chop_dims:2 [nbMulti; cVarDefs ["a_smem"; "b_smem"]];

  let kernel_mark = "kernel_body" in
  !! Marks.add_fake_instr kernel_mark [tAfter; cCall "kernel_launch"];

  !! Instr.delete [occFirst; cFor "bkIdx"; cCall "magic_barrier"];
  !! Gpu.insert_barrier [tFirst; cForBody "bkIdx"];
  !! Gpu.magic_barrier_to_blocksync [cMark kernel_mark] [occFirst; cFor "bkIdx"; cCall "magic_barrier"];
)
