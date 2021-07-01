open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

   !! Struct.inline "pos" [cTypDef "obj"];
   !!! Struct.inline "speed" [cTypDef "obj"];
   
)