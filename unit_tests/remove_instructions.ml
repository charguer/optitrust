open Optitrust

let _ = 
  run
  ( fun _ -> 
    set_init_source"remove_instructions.cpp";
    remove_instructions [[cVarDef "a"];[cVarDef "v"]];
    remove_instruction [cVarDef "a"];
    remove_instruction [cVarDef "v"];

    dump();
  )