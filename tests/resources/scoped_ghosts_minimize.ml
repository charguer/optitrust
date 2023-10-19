open Optitrust
open Target

(*let _ = Flags.resource_errors_as_warnings := true*)
(* let _ = Flags.display_includes := true *)

let _ = Run.script_cpp (fun () ->
  !! ();
  !! Resources.minimize_ghost_scopes [cFunBody ""];
  (* FIXME: some usage does not commute if we don't recompute resources. *)
  !! Resources.minimize_ghost_scopes [cFunBody ""];
  !! Resources.recompute_all_resources ();
)
