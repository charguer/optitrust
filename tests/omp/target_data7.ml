open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    let tg_loop = [cFor_c ""] in  
  
  !! Sequence.intro_on_instr ~mark:"seq_ins" tg_loop;
  !! Omp.target_data ~clause:[Map_c (From, ["p0[0:N]"])] [cMark "seq_ins"];
  
  !! Omp.parallel_for tg_loop;
  !! Omp.target ~clause:[If "N>THRESHOLD";Map_c (To, ["v1[:N]";"v2[:N]"])] tg_loop;
  
  !! Marks.clean [cMark "seq_ins"];

)
