open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  (* Since there are two vect_add function calls the first one need to an explicit bind *)
   show [cVarDef "speed2"];
   !! Struct.to_variables [cVarDef "speed2"];
   !! Struct.to_variables [cVarDef "pos2"];
   !! Loop.hoist "speed2_x_" [cFor "idParticle"];

)


