open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Loop.fission [tAfter; sInstr "t[i] += a"];
  (* TODO: add the function to find the for loop
    surrounding a target between

  *)
  (* FIXED *)
)