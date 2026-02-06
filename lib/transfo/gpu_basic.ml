open Prelude
open Target
open Flags

let change_to_thread_for_mode (t: trm): trm =
  (* Does not do any checks - only works when models are enabled!! *)
  assert !Flags.use_resources_with_models;
  let error = "Thread for transformation is invalid: it is not applied on a for loop." in
  let range, _, body, contract = trm_inv ~error trm_for_inv t in
  (trm_like ~old:t (trm_for ~contract ~mode:GpuThread range body))

let%transfo thread_for (tg : target) : unit =
  apply_at_target_paths (change_to_thread_for_mode) tg

(* Function 1: Convert a loop at the path p to thread for. Don't change the contracts in that loop, but if
  it is specified to be in a loop, then modify its contract to contain DesyncGroups. (using the procedure described in my notes).
  *)


(* let%transfo *)
