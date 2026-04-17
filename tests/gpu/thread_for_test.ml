open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := false
let _ = Flags.pretty_matrix_notation := true
let _ = Flags.recompute_resources_between_steps := false

let _ = Run.script_cpp (fun _ ->
  !! Resources.ensure_computed ();
  (* Verify that removing sync causes typing error *)
  !! Trace.resource_error_expected (fun _ ->
    Instr.delete [occFirst; cTopFunDef "sync_required"; cCall "blocksync"];
    Resources.ensure_computed ());
  !! Trace.generate_cuda ();
  let prefix = (Trace.get_context ()).prefix in
  if (not ((!Flags.aux_file_compare) (prefix ^ ".cu") (prefix ^ "_exp.cu"))) then begin failwith "Generated CUDA mismatch!" end;
)
