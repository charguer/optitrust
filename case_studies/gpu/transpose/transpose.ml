open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_important
let _ = Flags.cuda_codegen := false
let _ = Flags.pretty_matrix_notation := false

let stage_ok = fun i -> i = 3

let _ = Run.script_cpp_stage (stage_ok) (fun () ->
  let tile (loop_id, tile_size) = Loop.tile (trm_int tile_size)
    ~index:("b" ^ loop_id) ~bound:TileDivides [cFor loop_id] in
  !! List.iter tile [("x", 32); ("y", 32)];
  !! Loop.reorder_at ~order:["by"; "bx"; "y"; "x"] [cArrayWrite "b"];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "y"] "tile" [cArrayRead "a"];
  !! Loop.hoist_alloc ~dest:[tBefore; cFor "by"] [cVarDef "tile"]; (* TODO: SHOULD NOT GET RID OF __STRICT *)
  )

let _ = Run.script_cpp_stage (stage_ok) (fun () ->
    (* this only works after reparsing for some reason; when doing it in stage 1
    it does not add the right loop contracts to the j and y loops *)
  !! Loop.tile (trm_int 16) ~index:"j" ~bound:TileDivides [ occFirst; cFor "y" ~body:[cFor "x"]];
  !! Loop.reorder_at ~order:["x"; "y"] [cArrayWrite "b"];
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
  !! Gpu.create_kernel_launch bpg tpb []
    ~setup_end:[tBefore; cFor "by"] ~teardown_begin:[tAfter; cFor "by"]
    [occFirst; tBefore; cFor "bx"] [occLast; tAfter; cFor "bx"];
  !! Resources.ensure_computed ();

  (* Phase 2 *)
  !! Gpu.convert_tail_thread_for [1;0;1;1] [occLast; cFor "x"; cFor "y"];
  !! Gpu.convert_tail_thread_for [1;0] [occFirst; cFor "y"; cFor "x"];
)

(*let _ = Run.script_cpp_stage ~override_stage:100 (stage_ok) (fun () ->
  !! Resources.ensure_computed();
)*)
