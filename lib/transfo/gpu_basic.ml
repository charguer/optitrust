open Prelude
open Target
open Flags

let add_thread_for_attribute (t: trm): trm =
  (* Does not do any checks - only works when models are enabled!! *)
  assert !Flags.use_resources_with_models;
  let error = "Thread for transformation is invalid: it is not applied on a for loop." in
  let _, _, _ = trm_inv ~error trm_for_inv t in
  trm_add_attribute ThreadFor t

let%transfo thread_for (tg : target) : unit =
  apply_at_target_paths (add_thread_for_attribute) tg
