open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence.intro ~mark:"seq_ins" ~start:[tBefore; cVarDef "i"] ~stop:[tBefore; cReturn] ();
  
  !! Omp.teams ~clause:[Num_teams "2"] [cMark "seq_ins"];
  !! Omp.target ~clause:[Map_c (To, ["B[:N]";"C[:N]"]); Map_c (ToFrom, ["sum0";"sum1"])] [cMark "seq_ins"];
  
  !! Omp.parallel_for ~clause:[Reduction(Plus, ["sum0"])] [occIndex 0;cFor_c ""];
  !! Omp.parallel_for ~clause:[Reduction(Plus, ["sum1"])] [occIndex 1;cFor_c ""];

  !! Marks.clean [cMark "seq_ins"];
)
