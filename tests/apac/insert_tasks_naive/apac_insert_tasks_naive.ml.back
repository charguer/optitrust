open Optitrust
open Target 

let _ = Run.script_cpp (fun _ -> 

  let fad = Apac.get_functions_args_deps [] in

  !! Apac.insert_tasks_naive fad [cFunDef "h"];
  
)