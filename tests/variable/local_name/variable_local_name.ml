open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Variable.local_name ~mark:"mymark" ~var:"a" ~local_var:"x" [cFunBody "ok"; cFor "i"];
  !! Variable.local_name ~var:"a" ~local_var:"x" [cFunBody "ko"; cFor "i"];
)
