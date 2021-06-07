open Optitrust
open Run
let _ = 
  run
  ( fun _ -> 
    set_init_source"remove_instructions.cpp";
    Generic.remove_instructions [[cVarDef "a"];[cVarDef "v"]];
    Generic.remove_instruction [cVarDef "a"];
    Generic.remove_instruction [cVarDef "v"];

    dump();
  )