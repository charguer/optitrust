open Optitrust
open Prelude
open Target

let _ = Run.script_cpp (fun _ ->
  !! Matrix_basic.intro_malloc [cVarDef "p"; cCall "malloc"];
  let (x, _) = find_var "x" [] in
  let (y, _) = find_var "y" [] in
  !! Matrix_basic.intro_malloc0 x [cFunBody "main"];
  !! Matrix_basic.intro_malloc0 y [cLabel "y_seq"];
)
