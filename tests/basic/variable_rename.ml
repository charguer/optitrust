open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Variable_basic.rename (Ast.Rename_list [("y","y1");("z","z1")]) [cFunDef "f"; dBody];
  !! Variable_basic.rename (Ast.Postfix "2") [cTopFun "main"; dBody]; 
)
