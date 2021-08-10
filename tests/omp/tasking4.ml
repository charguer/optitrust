open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.task [Shared_c ["i"]] [tBefore;sInstrRegexp ~substr:true "i = ."];
  !! Omp.task [Shared_c ["j"]] [tBefore;sInstrRegexp ~substr:true "j = ."];
  !! Omp.taskwait [tBefore; tIndex ~nb:2 1;cReturn];
)
