open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  (* inline at one specific occurence *)
  !! Variable_basic.inline ~at:[sInstr "y = 9"] [cVarDef "y"];
  !! Variable_basic.inline ~at:[sInstr "b = 9"] [cVarDef "b"];
  (* inline at all occurences and delete the reference definition *)
  !! Variable_basic.inline ~delete:true [cVarDef "a"];
  (* inline a reference to a matrix row *)
  !! Variable_basic.inline ~delete:true [cVarDef "v"];
)

