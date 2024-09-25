open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun _ ->
  let x = trm_find_var "x" [] in
  let (y, _) = find_var "y" [] in
  (* TODO: (expr "2 + x") *)
  !! Variable_basic.subst ~subst:y ~put:(trm_add x (trm_int 2)) [cVarDef "z"];
  !! Variable_basic.subst ~subst:y ~put:(trm_int 5) [cOr [[cVarDef "a"]; [cWriteVar "z"]]];
)
