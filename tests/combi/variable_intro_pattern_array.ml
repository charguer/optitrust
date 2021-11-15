open Optitrust
open Target

(* TOOD: Fix the issue with aux variables later *)
let _ = Run.script_cpp (fun _ ->

  (* !! Variable.intro_pattern_array "double coef_x, sign_x, coef_y, sign_y, coef_z, sign_z; ==>  double a, b, c; ==> (coef_x + sign_x * a) * (coef_y + sign_y * b) * (coef_z + sign_z * c);" [nbMulti;cReturn; cCell ()]; *)
  !! Variable.intro_pattern_array "double coef_x, sign_x, coef_y, sign_y, coef_z, sign_z; ==>  double rx, ry, rz; ==> (coef_x + sign_x * rx) * (coef_y + sign_y * ry) * (coef_z + sign_z * rz);" [nbMulti;cReturn; cCell ()]
)
