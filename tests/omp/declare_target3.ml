open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Omp.declare_target [cVarDef "p"];
  !! Omp.end_declare_target [cTopFunDefAndDecl "init"];
     let tg_loop = [cFor_c ""] in 
  
  !! Omp.parallel_for tg_loop;
  !! Omp.target tg_loop;
  !! Omp.target_update ~clause:[To_c ["v1";"v2"]] tg_loop;

  !! Omp.target_update ~clause:[From_c ["p"]] [cFun "output"];

)
