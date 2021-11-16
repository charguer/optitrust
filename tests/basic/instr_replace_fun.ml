open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  (* replace the function call to "f" with a function call to "f1" *)
  !! Instr_basic.replace_fun "f1" [cFun "f"];
  !! Instr_basic.replace_fun "f" [occIndex ~nb:2 1; cFun "f1"];
  !! Instr_basic.replace_fun "f3" [cFun "f2"];
  !! Instr_basic.replace_fun "f2" [occIndex ~nb:2 1; cFun "f3"];
)
