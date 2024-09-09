open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  let x = find_var "x" [] in
  !! Matrix_basic.intro_malloc0 x [cFor "i"; dBody];
  !! Loop_basic.hoist [cVarDef "x"];
)
