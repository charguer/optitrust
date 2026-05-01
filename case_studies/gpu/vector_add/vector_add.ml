open Optitrust
open Prelude
open Cuda_lowering

let _ = Flags.check_validity := true
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := false
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_important

let _ = Run.script_cpp (fun () ->
  (* Stage 0: prepare *)
  !! Matrix.local_name_tile ~uninit_pre:true ~var:"c" ~local_var:"d_c" [cFor "i"];
  !! Matrix.local_name_tile ~uninit_post:true ~var:"a" ~local_var:"d_a" [cFor "i"];
  !! Matrix.local_name_tile ~uninit_post:true ~var:"b" ~local_var:"d_b" [cFor "i"];
  !! Matrix.memcpy [nbMulti; cFor "i1"];
  !! Resources.ensure_computed ();

  (* Stage 1: make kernel launch *)
  let n = trm_find_var "N" [] in
  let tpb = trm_int 256 in
  let bpg = trm_trunc_div_int n (trm_int 256) in
  !! Gpu.create_kernel_launch ~grid_override:[n] [bpg] [tpb] [] [tBefore; cFor "i"] [tAfter; cFor "i"];
  !! Resources.ensure_computed ();

  (* Stage 2: create thread hierarchy *)
  !! Gpu.convert_tail_thread_for [cFor "i"];
  !! Resources.ensure_computed ();

  (* Stage 3: convert memories *)
  !! Gpu.convert_to_global_mem [nbAny; cVarDefs ["d_a";"d_b";"d_c"]];
  !! Gpu.magic_barrier_to_teardown_sync [cCall "magic_barrier"];
  !! Resources.ensure_computed ();

  (* CUDA OUT *)
  !! Trace.generate_cuda ~check_expected:true ();
)
