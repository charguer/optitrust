open Optitrust
open Target 


(* TODO: Fix the missing struct and class  *)

let _ = Run.script_cpp (fun _ -> 


  !! Sequence_basic.intro ~mark:"seq_ins" 2[sInstr "= 100"];
  !! Omp.target [cMark "seq_ins"];

  !! Omp.end_declare_target [cTopFunDef "foo"];
  !! Omp.declare_target [cVarDef "varX"];

  !! Marks.clean [cMark "seq_ins"];

)
