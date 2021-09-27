open Optitrust
open Target


(* TODO: Fix me! *)
let _ = Run.script_cpp (fun _ ->

   !! Struct_basic.inline "pos" [cTypDef "obj"];
   !! Struct_basic.inline "speed" [cTypDef "obj"];
)
