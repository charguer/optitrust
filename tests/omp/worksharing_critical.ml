open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.parallel_sections [] [tBefore; cSeq ~args:[cSeq ~args:[cSeq ~args:[cSeq ~args:[cSeq ~args:[sInstr "i++"] ()] ()] ()] ()]()];
  !! Omp.section [tBefore; cSeq ~args:[cSeq ~args:[cSeq ~args:[cSeq ~args:[sInstr "i++"] ()] ()] ()] ()];
  !! Omp.critical "name" [tBefore; cSeq ~args:[cSeq ~args:[cSeq ~args:[sInstr "i++"] ()] ()] ()];
  !! Omp.parallel [] [tBefore; cSeq ~args:[cSeq ~args:[sInstr "i++"] ()] ()];
  !! Omp.single [] [tBefore; cSeq ~args:[sInstr "i++"] ()];
)
