open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  
  !! Sequence.intro_on_instr ~mark:"seq_ins" [cFun "fib"];
  !! Omp.declare_target [cTopFunDefAndDecl "fib"];
  !! Omp.end_declare_target [cVarDef "THRESHOLD"];
  !! Omp.target ~clause:[If "n > THRESHOLD"] [cMark "seq_ins"];
  !! Marks.clean [cMark "seq_ins"];

)
