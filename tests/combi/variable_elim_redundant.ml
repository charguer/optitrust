open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable.elim_redundant [cVarDef "b"];
  !! Variable.elim_redundant [cVarDef "f"];
     Variable.elim_redundant [cVarDef "g"]; (* TODO: why not working? [cOr [[cVarDef "f"]; [cVarDef "g"]]; *)
  !! Variable.elim_redundant [cVarDef "coef_x2"];
  (* TODO: no need for argc/argv in unit test, please remove them everywhere *)
)
