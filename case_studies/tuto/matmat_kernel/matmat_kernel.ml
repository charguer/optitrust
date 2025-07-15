open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  !! Loop.collapse [cFor "i"];
  !! Loop.fission  [cWrite ~lhs:[cVar "C"] ~rhs:[cDouble 0.] ();tAfter];

  ;)
