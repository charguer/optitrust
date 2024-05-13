open Optitrust
open Target



let _ = Run.script_cpp (fun _ ->

  !! Variable.elim_redundant [cVarDef "b"];
  !! Variable.elim_redundant [cOr [[cVarDef "f"];[cVarDef "g"]]];

  (* FIXME: Trm_val not handled correctly by unify_trm *)
  (*!! Variable.elim_redundant [cVarDef "coef_x2"];*)

)
