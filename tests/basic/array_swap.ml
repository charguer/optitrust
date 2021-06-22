open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    Arrays.swap [cTypDef "T"];
)