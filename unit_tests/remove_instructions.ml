open Optitrust

let _ = 
  run
  ( fun _ -> 
    set_init_source"remove_instructions.cpp";
    remove_instructions [[cVarDef ~name:"a"()];[cVarDef ~name: "v"()]];
    remove_instruction [cVarDef ~name:"a"()];
    remove_instruction [cVarDef ~name: "v"()];

    dump();
  )