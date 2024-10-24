open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Variable.(renames (AddSuffix "1")) [cFunBody "main"];
  )
