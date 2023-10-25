open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Instr.move_out_of_fun [cVarDef "x"];
  !! Instr.move_out_of_fun [cVarDefs ["y"; "z"]];

)
