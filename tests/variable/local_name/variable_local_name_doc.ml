open Optitrust
open Prelude

let _ = Flags.typechecking_mode := ProofPreserving

let _ = Run.script_cpp (fun _ ->
  !! Variable.local_name ~var:"x" ~local_var:"y" [cLabel "sec"];
)
