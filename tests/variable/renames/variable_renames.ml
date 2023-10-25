open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable.(renames(AddSuffix "2")) [cTopFunDef "main"; dBody];
  !! Variable.renames(ByList [("y","y1");("z","z1")]) [cFunDef "f"; dBody];
  !! Variable.(renames(AddSuffix "2")) [cTopFunDef "main"; dBody];
)
