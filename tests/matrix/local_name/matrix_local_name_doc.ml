open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  let a = find_var "a" [] in
  !! Matrix_basic.local_name a ~into:"b" [cFor "i"];

)
