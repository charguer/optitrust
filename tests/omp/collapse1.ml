open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.for_ [Collapse 2; Private ["i"; "k"; "j"]] [tBefore;cFor "k"];
)
