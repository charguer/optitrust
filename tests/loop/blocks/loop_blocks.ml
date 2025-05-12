open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  bigstep "x";

  !! Resources.ensure_computed ();
  !! Loop_basic.color (trm_int 2) [cFor "k"];
  !! Resources.ensure_computed ();

  !!! (); (* TODO: Find how to eliminate this reparse *)

)
