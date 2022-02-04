open Optitrust
open Target


(* TODO: the semi-colon in the pattern should be inserted by the transformation,
   not given by the user; likewise for pattern vars. *)

(* let _ = Run.doc_script_cpp (fun _ ->
  !! Variable.intro_pattern_array ~pattern_vars:"int k;" ~pattern:"2 * k;" [nbMulti; cWrite(); dRHS];
  )
"
int main() {
  int a = 7, b = 8, c = 9;
  int t[3];
  t[0] = 2 * a;
  t[1] = 2 * b;
  t[2] = 2 * c;
}
" *)

let _ = Run.script_cpp (fun _ ->
  !! Variable.intro_pattern_array ~pattern_vars:"double coef_x, sign_x, coef_y, sign_y, coef_z, sign_z;" ~pattern_aux_vars:"double rx, ry, rz;" ~pattern:"(coef_x + sign_x * rx) * (coef_y + sign_y * ry) * (coef_z + sign_z * rz);" [nbMulti; cReturn; cCell ()];
)
