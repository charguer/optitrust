open Prelude

(* maybe: flag ~elimoptitrust:true on run script? *)
let%transfo std (_u: unit) : unit =
  Trace.tag "pre-post-processing";
  Flags.recompute_resources_between_steps := false;
  Matrix.elim_mops [];
  Resources.delete_annots [];
  Loop.delete_all_void []
