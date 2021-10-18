open Optitrust
open Target

(* TODO: Allow wild cards in patterns *)
let _ = Run.script_cpp (fun _ ->

  Variable.intro_pattern_array "double coef_x; double sign_x; double coef_y; double sign_y; double coef_z; double sign_z; # (coef_x + sign_x * rx) * (coef_y + sign_y * ry) * (coef_z + sign_z * rz);" [nbMulti;cReturn; cCell ()]
)
