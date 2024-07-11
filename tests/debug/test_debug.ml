open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->

  !! Arith_basic.(simpl euclidian) [nbMulti; cWriteVar "eu"; dRHS];
   !! ()
)
