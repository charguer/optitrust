open Optitrust
open Target

let _ = Run.script_cpp(fun _ ->

  !! Arrays.tile "B" [cTypDef "T"];
  !! Arrays.tile "B" ~block_type:"U_BLOCK" [cTypDef "U"];
  !! Arrays.tile "B" [cTypDef "V"];
  (* TODO: change the label arg *)
  )