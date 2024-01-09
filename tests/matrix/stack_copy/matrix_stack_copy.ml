open Optitrust
open Prelude

let _ = Flags.check_validity := false (* TODO *)

let _ = Run.script_cpp (fun () ->
  !! Resources.ensure_computed ();
  let s = find_var_in_current_ast "s" in
  !! Matrix_basic.stack_copy ~var:s ~copy_var:"x" ~copy_dims:1 [cFor ~body:[sInstr "+="] "j"];
  !! Trace.set_ast ( Resource_computation.trm_deep_copy (Trace.ast()));
  !! Resources.ensure_computed ();
  !!! ();
)
