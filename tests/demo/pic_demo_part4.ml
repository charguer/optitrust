open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
   !! Loop.grid_enumerate [("x", "gridSize"); ("y", "gridSize"); ("z", "gridSize")] [cFor "idCell"];
   !! Loop.tile "2" "bx" [cFor "x"];
   !! Loop.tile "2" "by" [cFor "y"];
   !! Loop.tile "2" "bz" [cFor "z"];
   !! Loop.color "2" "cx" [cFor "bx"];
   !! Loop.color "2" "cy" [cFor "by"];
   !! Loop.color "2" "cz" [cFor "bz"];
   !! Loop.move "x" ~after:"bz";
   !! Loop.move "y" ~after:"x";
   !! Loop.move "cy" ~before:"bx";
   !! Loop.move "cz" ~before:"bx";
)


