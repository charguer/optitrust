open Optitrust
open Prelude
open Target

let _ = Flags.typechecking_mode := Flags.ProofRepairing
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun _ ->
  !! Resources.ensure_computed ();
  (* Verify that removing sync causes typing error *)
  !! Trace.resource_error_expected (fun _ ->
    Instr.delete [occFirst; cTopFunDef "sync_required"; cCall "blocksync"];
    Resources.ensure_computed ());
  !! Trace.generate_cuda ~check_expected:true ();
)
