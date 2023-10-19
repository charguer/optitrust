open Optitrust
open Target



let _ = Run.script_cpp (fun _ ->

  !! Instr.copy [cWriteVar "x"];
  !! Instr.copy [cWriteVar "y"];
  !! Instr.copy [cFun "f"];
)