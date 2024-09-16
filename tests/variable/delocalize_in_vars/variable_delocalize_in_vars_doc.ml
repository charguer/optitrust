open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->
  let fv n = find_var n [] in
  !! Variable.delocalize_in_vars "a" ~into:"x" ~index:"k" ~mark:"section_of_interest" ~array_size:(fv "N") ~ops:(Local_arith (Lit_int 0, Binop_add) ) ~local_vars:["xa";"xb"] [cFor "i"];

)
