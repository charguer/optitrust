open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := false
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.cuda_codegen := false (* TODO, unit tests should be able to check CUDA output as well *)

let _ = Run.script_cpp (fun _ ->
  (* !! Gpu_basic.convert_thread_for_tail_nest ["i"] [cTopFunDef "basic"; cFor "j"];*)
  !! Resources.make_strict_loop_contracts [];
  !! Resources.ensure_computed ();
  !! (
    Flags.check_validity := true;
    Loop.fission [tBefore; cCall "magic_barrier"];
    Flags.check_validity := false;
  );
  !! Resources.ensure_computed ();
)
