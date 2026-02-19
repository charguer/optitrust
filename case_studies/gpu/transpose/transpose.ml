open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_important
let _ = Flags.pretty_matrix_notation := false

let stage_ok = fun i -> true

let _ = Run.script_cpp_stage (stage_ok) (fun () ->
  !! Matrix.local_name_tile ~uninit_post:true ~var:"a" ~local_var:"d_a" [cFor "x"];
  !! Matrix.local_name_tile ~uninit_pre:true ~var:"b" ~local_var:"d_b" [cFor "x"];
  !! Matrix.memcpy [nbMulti; cFor "i1"]; (* TODO: integrate memcpy into local_name_tile as an option? *)

  let tile (loop_id, tile_size) = Loop.tile (trm_int tile_size)
    ~index:("b" ^ loop_id) ~bound:TileDivides [cFor loop_id] in
  !! List.iter tile [("x", 32); ("y", 32)];
  !! Loop.reorder_at ~order:["by"; "bx"; "y"; "x"] [cArrayWrite "d_b"];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "y"] "tile" [cArrayRead "d_a"];
  !! Loop.hoist_alloc ~dest:[tBefore; cFor "by"] [cVarDef "tile"]; (* TODO: SHOULD NOT GET RID OF __STRICT *)
  )

let _ = Run.script_cpp_stage (stage_ok) (fun () ->
    (* this only works after reparsing for some reason; when doing it in stage 1
    it does not add the right loop contracts to the j and y loops *)
  !! Loop.tile (trm_int 16) ~index:"j" ~bound:TileDivides [ occFirst; cFor "y" ~body:[cFor "x"]];
  !! Loop.reorder_at ~order:["x"; "y"] [cArrayWrite "d_b"];
  !! Loop.tile (trm_int 16) ~index:"j" ~bound:TileDivides [ occFirst; cFor "x" ~body:[cFor "y"]];
  (* !! Cleanup.std () *)
)

let _ = Run.script_cpp_stage (stage_ok) (fun () ->
  Flags.recompute_resources_between_steps := false;
  let w,h = (trm_find_var "W" [],trm_find_var "H" []) in
  (* TODO: clean this up and share the constants with other parts of the script *)
  let tile_size = (trm_int 32) in
  let tpb = [(trm_int 16);tile_size] in
  let bpg = [trm_exact_div_int h tile_size;trm_exact_div_int w tile_size] in
  let smem_szs = [trm_mul_int (trm_sizeof typ_f32) (trm_mul_int tile_size tile_size)] in
  !! Gpu.create_kernel_launch bpg tpb smem_szs
    ~setup_end:[tBefore; cFor "by"] ~teardown_begin:[tAfter; cFor "by"]
    [occFirst; tBefore; cFor "bx"] [occLast; tAfter; cFor "bx"];
  !! Resources.ensure_computed ();
  (* Phase 2 *)
  !! Gpu.convert_tail_thread_for [1;0;1;1] [occLast; cFor "x"; cFor "y"];
  !! Gpu.convert_tail_thread_for [1;0] [occFirst; cFor "y"; cFor "x"];
  !! Gpu.convert_magic_thread_fors [nbAny; cFor "by"; cFor "" ];
)

let _ = Run.script_cpp_stage (stage_ok) (fun () ->
  let kernel_mark = "kernel_body" in
  !! Marks.add kernel_mark [occFirst; cFor "by"];
  !! Gpu.convert_to_global_mem [nbAny; cOr [[cVarDef "d_a"];[cVarDef "d_b"]]];
  !! Gpu.convert_to_shared_mem 2 [cMark kernel_mark] [cVarDef "tile"];
  (* move up teardown, TODO thread for conversion should move this above barriers automatically *)
  !! Instr.move ~dest:[tAfter; cMark kernel_mark] [cCall "kernel_teardown_begin"];
  !! Gpu.magic_barrier_to_blocksync [cMark kernel_mark] [cFor "bx"; cCall "magic_barrier"];
  !! Gpu.magic_barrier_to_teardown_sync [cCall "magic_barrier"];
)

let _ = Run.script_cpp_stage (stage_ok) (fun () ->
  (* TODO: preserve marks between stages *)
  !! Marks.add "kernel_sequence" [cSeq ~instrs_pred:(Target.target_list_one_st [cCall "kernel_launch"]) ()];
  (* TODO: not sure why this is necessary, some transfo appears to be adding things to the
    top of the sequence *)
  Flags.recompute_resources_between_steps := false;
  !! (
    Flags.check_validity := false;
    Instr.move ~dest:[tFirst; cMark "kernel_sequence"] [cCall "kernel_launch"];
    Flags.check_validity := true;
  );
  !! (
    Flags.check_validity := false;
    Trace.generate_cuda ();
    Flags.check_validity := true;
  )
)
