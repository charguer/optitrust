open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Variable_basic.(rename_on_block (AddSuffix "2")) [cTopFunDef "main"; dBody];
  !! Variable_basic.rename_on_block (Variable.ByList [("y","y1");("z","z1")]) [cFunDef "f"; dBody];
  !! Variable_basic.(rename_on_block (AddSuffix "2")) [cTopFunDef "main"; dBody];
)
