open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := false
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.cuda_codegen := false (* TODO, unit tests should be able to check CUDA output as well *)

let _ = Run.script_cpp (fun _ ->
  !! Resources.ensure_computed ();
  !! Trace.resource_error_expected (fun _ -> Instr.delete [occFirst; cTopFunDef "sync_required"; cCall "blocksync"])
  (* TODO: negative unit tests for removing parts of the contract?
  E.g. things should break when we get rid of KernelParams, ThreadsCtx, etc. *)
)
