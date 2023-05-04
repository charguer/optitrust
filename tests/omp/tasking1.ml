open Optitrust
open Target


(* let _ = Flags.set_dump_clang_ast() *)

let _ = Run.script_cpp (fun _ ->


 !! Omp.task [nbMulti; cFun "traverse"];

)
