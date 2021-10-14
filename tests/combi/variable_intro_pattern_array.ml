open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  Variable.intro_pattern_array "double coef_x, sign_x, coef_y, sign_y, coef_z, sign z; (coef_x + sign_x * rx) * (coef_y + sign_y * ry) * (coef_z + sign_z * rz);" [cReturn; cCell]
)