open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.invariant [cVarDef "x"];
  (* TODO: invoke cleanup_no_brace on the specific id introduced
     at the end of the transformation

     the cleanup can be called on the entire ast,
     or on the immediate prefix of the explicit path that goes to the for-loop *)
  !! Loop.invariant [cVarDef "x"];
)
