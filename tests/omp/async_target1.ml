open Optitrust
open Target


let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

  
  !! Omp.declare_target [cFunDef "F"];
  !! Omp.end_declare_target [cFunDef "F"];
  !! Omp.task ~clause:[Shared ["z"]] [cFor "i"];
  !! Omp.target_teams ~clause:[Map_c (From, ["Z[C:CHUNKSZ]"])] [cFor "i"];
  !! Omp.parallel_for [cFor "i"];
  !! Omp.taskwait [cFor "i"];
)
