open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_important
let _ = Flags.cuda_codegen := false
let _ = Flags.pretty_matrix_notation := false

let stage_ok = fun i -> i = 2

let _ = Run.script_cpp_stage (stage_ok) (fun () ->
  let tile (loop_id, tile_size) = Loop.tile (trm_int tile_size)
    ~index:("b" ^ loop_id) ~bound:TileDivides [cFor loop_id] in
  !! List.iter tile [("x", 32); ("y", 32)];
  !! Loop.reorder_at ~order:["by"; "bx"; "y"; "x"] [cArrayWrite "b"];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "y"] "tile" [cArrayRead "a"];
  !! Loop.hoist_alloc ~dest:[tBefore; cFor "by"] [cVarDef "tile"];
)

let _ = Run.script_cpp_stage (stage_ok) (fun () ->
  (* this only works after reparsing for some reason *)
  !! Loop.tile (trm_int 16) ~index:"j" ~bound:TileDivides [ occFirst; cFor "y" ~body:[cFor "x"]];
  !! Loop.reorder_at ~order:["x"; "y"] [cArrayWrite "b"];
  !! Loop.tile (trm_int 16) ~index:"j" ~bound:TileDivides [ occFirst; cFor "x" ~body:[cFor "y"]];
  (* !! Cleanup.std () *)
)
