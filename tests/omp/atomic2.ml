open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.atomic (Some Read) [tBefore; sInstrRegexp ~substr:true "value = ."];
  !! Omp.atomic (Some Write) [tBefore; sInstrRegexp ~substr:true ". = value"];
)
