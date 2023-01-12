open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.for_ ~clause:[Linear (["j"],1)] [occIndex ~nb:2 1; cFor "i"];
  !! Omp.parallel [occIndex ~nb:2 1; cFor "i"];

)
