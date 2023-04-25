open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  (* !! Omp.parallel_for ~clause:[If "N > THRESHOLD2"] [cFor_c ""]; *)
  
  !! Omp.target ~clause:[Parallel_c; For_c; If "target: N > THRESHOLD1"; If "parallel : N > THRESHOLD2";Map_c (To, ["v1[0:N]"; "v2[:N]"]); Map_c (From, ["p[0:N]"])] [cFor_c ""];

)
