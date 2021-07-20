open Optitrust
open Target

let _ = Run.script_cpp(fun _ ->

  !! Arrays.tile "X" ~block_type:"B" [cTypDef "T"];
  !! Arrays.tile "Y" [cTypDef "U"];

  (* LATER (or in the summer): Most convenient for the user:
  !! Arrays.tile "B" ~blocktype:"X" [cTypDef "T"];
  !! Arrays.tile ~blocktype:"X" "B" [cTypDef "T"];
  !! Arrays.tile "B" [cTypDef "T"];  => use "T_block" as default for "X"
  *)
)