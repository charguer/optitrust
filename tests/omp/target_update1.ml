open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

     let tg_loop1 = [occIndex 0; cFor_c ""] in 
  !! Sequence_basic.intro ~mark:"seq_ins"  3 tg_loop1;
  
  !! Omp.target_data ~clause:[Map_c (To, ["v1[:N]";"v2[:N]"]);Map_c (From, ["p0[0:N]"])] [cMark "seq_ins"];
  
  !! Omp.parallel_for tg_loop1;
  !! Omp.target tg_loop1;
   
     let tg_loop2 = [occIndex 1; cFor_c ""] in 
  !! Omp.parallel_for tg_loop2;
  !! Omp.target tg_loop2;
  !! Omp.target_update ~clause:[To_c ["v1[:N]";"v2[:N]"]] tg_loop2;

  !! Marks.clean [cMark "seq_ins"];

)
