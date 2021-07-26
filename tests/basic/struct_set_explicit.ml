open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Struct_basic.set_explicit [sInstr "b = p"];
  !! Struct_basic.set_explicit [sInstr "u = a.pos"];
  !! Struct_basic.set_explicit [sInstr "t[0] = p2"];
)
