open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Variable_basic.rename ~list:[("y","y1");("z","z1")] [cFunDef "f"; dBody];
  !! Variable_basic.rename ~func: (fun s -> s ^ "2") [cTopFun "main"; dBody]; 
)
