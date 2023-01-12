open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence.intro ~mark:"seq_ins" ~start:[tBefore; cVarDef "changed"] ~stop:[tBefore;cFun "output"] ();
  
  
  !! Omp.target_data ~clause:[Map_c (To, ["v1[:N]";"v2[:N]"]);Map_c (From, ["p0[0:N]"])] [cMark "seq_ins"];
  
     let tg_loop1 = [occIndex 0; cFor_c ""] in 
  !! Omp.parallel_for tg_loop1;
  !! Omp.target tg_loop1;
 
  !! Omp.target_update ~clause:[If "changed"; To_c ["v1[:N]"]] [occIndex 1; cWriteVar "changed"];
     let tg_loop2 = [occIndex 1; cFor_c ""] in 
  
  !! Omp.parallel_for tg_loop2;
  !! Omp.target tg_loop2;
  !! Omp.target_update ~clause:[If "changed"; To_c ["v2[:N]"]] tg_loop2;

  !! Marks.clean [cMark "seq_ins"];
  
)
