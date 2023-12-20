open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Typedef_basic.insert "uchar" (Typdef_alias (ty "unsigned char" )) [tAfter; cVarDef "a"];

)
