open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence.intro_on_instr ~mark:"seq_ins" [cFun "vec_mult"];
  !! Omp.target_data ~clause:[Map_c (To, ["v1[0:N]";"v2[:N]"]); Map_c (From, ["p0[0:N]"])] [cMark "seq_ins"];
  
     let tg_loop = [cFor_c ""] in 
  !! Omp.parallel_for tg_loop;
  !! Omp.target ~clause:[Map_c (To, ["v3[0:N]";"v4[:N]"]); Map_c (From, ["p1[0:N]"])] tg_loop;
  !! Marks.clean [cMark "seq_ins"];
  
)
