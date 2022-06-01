open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Omp.for_ ~clause:[Collapse 2; Private ["i"; "k"; "j"]] [cFor "k"];

)
