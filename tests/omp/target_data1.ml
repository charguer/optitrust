open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence.intro_on_instr ~mark:"seq_ins" [cFor_c ""];
  !! Omp.parallel_for [cFor_c ""];
  !! Omp.target [cFor_c ""];
  !! Omp.target ~clause:[Map_c (To, ["v1[0:N]"; "v2[:N]"]); Map_c (From, ["p[0:N]"])] [cMark "seq_ins"];
  !! Marks.clean [cMark "seq_ins"];

)
