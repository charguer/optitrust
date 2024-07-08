open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Typedef_basic.insert "uchar" (Typedef_alias (ty "uint8_t")) [tAfter; cVarDef "a"];
  !!! ();

)
