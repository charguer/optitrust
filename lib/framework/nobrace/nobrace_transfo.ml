open Ast


(** [remove_after ~remove f]: wrapper for creating and deleting a nobrace sequence.
  Internally it applies the function clean_nobrace over the trm t.
  By default, scope is checked: inlining nobrace sequences should not change variable usage.
  LATER: ideally, we may want to remove all cstyle carrying the identifier ID when remove is false. *)
let remove_after ?(check_scoping = true) ?(remove : bool = true) (f : unit -> unit) : unit =
  Nobrace.enter ();
  f ();
  let id = Nobrace.exit () in
  if remove then begin
    Trace.apply (Nobrace.clean_all_seq id);
    if check_scoping then Scope.infer_var_ids ()
  end

