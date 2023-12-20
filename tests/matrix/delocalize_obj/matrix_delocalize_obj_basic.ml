open Optitrust
open Prelude

(* TODO: Add a minimal documentation test *)


let _ = Run.script_cpp (fun _ ->
  !! Matrix_basic.delocalize ~acc_in_place:false ~dim:(var "N0") ~index:"i0" ~acc:"sum" ~ops:(Local_obj (toplevel_var "bag_init", toplevel_var "bag_append", toplevel_var "bag_free")) [cLabel "mark"];

)
