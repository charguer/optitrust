open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
   !! Struct.to_variables [cVarDef "speed2"];
   !! Loop.extract_variable [cVarDef "speed2_x"];
   !! Loop.extract_variable [cVarDef "speed2_y"];
   !! Loop.extract_variable [cVarDef "speed2_z"];

   !! Struct.to_variables [cVarDef "pos2"];
   !! Loop.extract_variable [cVarDef "pos2_x"];
   !! Loop.extract_variable [cVarDef "pos2_y"];
   !! Loop.extract_variable [cVarDef "pos2_z"];
   
   !! Loop.split [sInstr "pos2_x[idParticle] = "];
   !! Loop.split [cVarDef "idCell2"];

)


