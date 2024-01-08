open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  (*!! Label.add "lab1" [cVarDef "a"];*)

  (* The next block of code debugs printing of errors *)
  !! let t = Trace.ast() in
  let t = Resource_computation.trm_deep_copy t in
  t.errors <- ["foo"];
  Trace.set_ast t;
  (*Show.(trm ~style:InternalAst) t;*)
  (*Show.trm t;*)
  !! ();

  (* The next line sets an invalid contract *)
  !! Resources.(set_loop_contract [__modifies("&m[MINDEX1(sz, i)] ~> Cell")]) [cFor "i"];
  (* The line [ensure_computed] triggers a resource typing error.
     If [resource_errors_as_warnings] is [true], the error is
     simply displayed. *)
     (*
  !! Flags.resource_errors_as_warnings := true;
  !! Resources.ensure_computed ();
  !! Flags.resource_errors_as_warnings := false;
  *)
  (* Otherwise if [resource_errors_as_warnings] is [false],
     the error is raised. *)
  !! Resources.ensure_computed ();

)
