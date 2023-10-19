open Optitrust
open Target

let _ = Run.script_cpp (fun _->
  show [cArrayWrite "t"];
  !! Matrix_basic.read_last_write ~write:[cArrayWrite "t"] [cArrayRead "t"];
  !! Matrix_basic.read_last_write ~write:[cArrayWrite "t2"] [nbMulti; cArrayRead "t2"];
  !! Matrix_basic.read_last_write ~write:[cArrayWrite "t3"] [nbMulti; cArrayRead "t3"];
  !! Matrix_basic.read_last_write ~write:[cArrayWrite "img"] [cArrayRead "img"];
)
