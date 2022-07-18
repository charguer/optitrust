open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  
  !! Sequence_basic. intro 3 ~mark:"seq_ins" [occIndex 0; cFor_c ""];
  let tg_loop1 = [occIndex 0; cFor_c ""] in 
  !! Omp.parallel_for tg_loop1;
  !! Omp.target ~clause:[Map_c (To, ["v1[:N]"; "v2[:N]"])] tg_loop1;

  let tg_loop2 = [occIndex 1; cFor_c ""] in 
  !! Omp.parallel_for tg_loop2;
  !! Omp.target ~clause:[Map_c (To, ["v1[:N]"; "v2[:N]"])] tg_loop2;

  !! Omp.target_data ~clause:[Map_c (From, ["p[0:N]"])] [cMark "seq_ins"];
  !! Marks.clean [cMark "seq_ins"];
  
)
