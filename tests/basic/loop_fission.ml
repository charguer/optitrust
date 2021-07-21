open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Loop.fission [sInstr "int b = i"];
  (* TODO: add the function to find the for loop
    surrounding a target between
  *)
)