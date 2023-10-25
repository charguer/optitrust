open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Matrix_basic.read_last_write ~write:[cArrayWrite "t"] [cArrayRead "t"];
)
