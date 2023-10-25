open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  let x = find_var_in_current_ast "x" in
  !! Variable_basic.local_name x ~into:"y" [cLabel "sec"];

)
