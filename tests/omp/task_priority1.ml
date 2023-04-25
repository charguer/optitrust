open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Sequence_basic.intro_on_instr ~mark:"seq_ins" [cFor_c ""];
  !! Omp.task ~clause:[Priority "i"] [cFun "compute_array"];
  !! Omp.single [cMark "seq_ins"];
  !! Omp.parallel ~clause:[Private ["i"]] [cMark "seq_ins"];
  !! Marks.remove "seq_ins" [cMark "seq_ins"];
)
