open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun _ ->
  let (a, _) = find_var "a" [] in
  !! Variable_basic.subst ~subst:a ~put:(trm_int 3) [cVarDef "b"];
)
