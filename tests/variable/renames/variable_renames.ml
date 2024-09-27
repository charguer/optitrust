open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable.(renames(AddSuffix "2")) [cTopFunBody "main"];
  !! Variable.renames(ByList [("y","y1");("z","z1")]) [cFunBody "f"];
  !! Variable.(renames(AddSuffix "2")) [cTopFunBody "main"];
)
