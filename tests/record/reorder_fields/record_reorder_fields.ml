
open Optitrust
open Target
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! Record_basic.reorder_fields (Move_before ("x", ["m";"z"])) [cTypDef "obj"];
  !! Record_basic.reorder_fields (Move_after ("y", ["z"])) [cTypDef "obj"];
  !! Record_basic.reorder_fields (Move_after ("z", ["y";"m"])) [cTypDef "obj"];
  !! Record_basic.reorder_fields (Reorder_all ["x";"y";"z"; "m"]) [cTypDef "obj"];

  !! Record_basic.reorder_fields (Move_before ("x", ["f";"g"])) [cTypDef "OBJ"];
  !! Record_basic.reorder_fields (Reorder_all ["x";"f";"g"]) [cTypDef "OBJ"];

)
