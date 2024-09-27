open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence.intro_on_instr ~mark:"seq" [sInstr "x++"];
  !! Omp.task ~clause:[Mergeable] [cMark "seq"];
  !! Omp.taskwait [cCall "printf"];
  !! Marks.remove "seq" [cMark "seq"];

)
