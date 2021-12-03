open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  (* TODO: move to target_one
  show [cFieldRead "x" ()];
  show [cFieldWrite "x" ()];
  *)
  show [cFieldAccess "x" ()]; (* TODO: missing the reads *)

  (*
  show [cFieldWrite "x" ];
  show [cFieldRead "y" ];
  show [cFieldWrite "y" ];
  show [cFieldRead "pos" ];
  show [cFieldWrite "pos" ];*)


)
