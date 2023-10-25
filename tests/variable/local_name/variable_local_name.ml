open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  let a = find_var_in_current_ast "a" in
  !! Variable_basic.local_name ~mark:"mymark" a  ~into:"x" [cFor "i"];

)
