open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Struct_basic.set_explicit [sInstr "b = p"];
  !! Struct_basic.set_explicit [sInstr "u = a.pos"];
  !! Struct_basic.set_explicit [sInstr "t[0] = p2"];
  !! Struct_basic.set_explicit [sInstr "c = a"];
  !! Struct_basic.set_explicit [sInstr "c.pos ="];
     Struct_basic.set_explicit [sInstr "c.speed ="];

  (* TODO: this one should raise an error saying that only struct type can be updated *)
  !!! Struct_basic.set_explicit [sInstr "c.weight ="];


  (* LATER: arthur think about this one, the explicit path are modified in between...
    !! Struct_basic.set_explicit [nbMulti; cOr [[sInstr "c.pos ="]; [sInstr "c.speed ="]]]; *)
)
