open Optitrust
open Target
open Prelude

let _ = Run.script_cpp (fun () ->
  let a = find_var_in_current_ast "a" in
  !! Variable.delocalize a ~into:"x" ~index:"k" ~mark:"A" ~array_size:(var "N") ~ops:(Local_arith (Lit_int 0, Binop_add) ) [cFor "i"];

)
