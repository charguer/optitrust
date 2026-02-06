open Optitrust
open Prelude
open Cuda_lowering

let _ = Flags.check_validity := true
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := false
let _ = Flags.disable_stringreprs := true
let _ = Flags.save_ast_for_steps := Some Flags.Steps_important
let _ = Flags.cuda_codegen := true

(* Trace.apply (Cuda_lowering.lower_to_cuda)*)
(* script_cuda ? *)
(* Print both for unit tests *)
(* Check Reparse transformation, show transfo *)
let _ = Run.script_cpp (fun () ->
  (* Stage 0: prepare *)
  !! Matrix.local_name_tile ~uninit_pre:true ~var:"c" ~local_var:"d_c" [cFor "i"];
  !! Matrix.local_name_tile ~uninit_post:true ~var:"a" ~local_var:"d_a" [cFor "i"];
  !! Matrix.local_name_tile ~uninit_post:true ~var:"b" ~local_var:"d_b" [cFor "i"];
  !! Matrix.memcpy [nbMulti; cFor "i1"];
  Resources.ensure_computed ();

  (* Stage 1: make kernel launch *)
  let n = trm_find_var "N" [] in
  let tpb = trm_trunc_div_int n (trm_int 256) in
  let bpg = trm_int 256 in
  !! Gpu_basic.create_kernel_launch ~tctx_override:[n] [tpb] [bpg] [] [cFor "i"];
  !! Resources.ensure_computed ();

  (* Stage 2: create thread hierarchy *)
  !! Gpu_basic.convert_thread_for_tail_nest ~stop_tg:[cCall "kernel_teardown_begin"] ~insert_barrier:true [] [cFor "i"];
  !! Resources.ensure_computed ();

  (* Stage 3: convert memories *)
  !! Gpu_basic.convert_to_global_mem ~var:"d_a" [cTopFunDef "vector_add"];
  !! Gpu_basic.convert_to_global_mem ~var:"d_b" [cTopFunDef "vector_add"];
  !! Gpu_basic.convert_to_global_mem ~var:"d_c" [cTopFunDef "vector_add"];
  !! Gpu_basic.convert_magic_sync [cCall "magicsync"];
  !! Resources.ensure_computed ();


  (* CUDA OUT *) )
