open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.for_ [Ordered_c 2; Private ["i";"j";"k"]] [tBefore; cFor_c "i"];
  !! Omp.ordered [Depend (Sink ["i-1";"j"]);Depend (Sink ["i+1";"j"]);Depend (Sink ["i";"j-1"]);Depend (Sink ["i";"j+1"])] [tBefore; cFor_c "k"];
)
