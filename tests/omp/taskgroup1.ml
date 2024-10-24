open Optitrust
open Target



(* TODO: Fix the issue with typedef parsing. *)

let _ = Run.script_cpp (fun _ ->


  !! Sequence.intro_on_instr ~mark:"call" [cCall "compute_tree"];
  !! Sequence_basic.intro ~mark:"loop" 2 [cCall "start_backgroud_work"];

  !! Omp.task [cCall "start_background_work"];
  !! Omp.taskgroup [cMark "call"];
  !! Omp.task [cCall "compute_tree"];
  !! Omp.single [cMark "loop"];
     Marks.remove "loop" [cMark "loop"];
     Marks.remove "call" [cMark "call"];

)
