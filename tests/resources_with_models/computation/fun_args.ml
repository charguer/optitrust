open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
(*let _ = Flags.resource_errors_as_warnings := true*)

let _ = Run.script_cpp (fun () ->
  (* Conflicting effects must not typecheck *)
  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Sequence.insert ~reparse:true (instr "y = incr(&x) + incr(&x)") [cFunBody "write_in_args"; tLast];
    Trace.recompute_resources ();
  );
  !! Trace.failure_expected (fun _ -> true) (fun () ->
    Sequence.insert ~reparse:true (instr "y = (incr(&x) - incr(&y)) + incr(&x)") [cFunBody "write_in_args"; tLast];
    Trace.recompute_resources ();
  );
)
