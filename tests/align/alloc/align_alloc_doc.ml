open Optitrust
open Prelude


let _ = Run.script_cpp (fun () ->

  !! Align.alloc (lit "64") [cCall "MALLOC1"];
  !!! (); (* TODO: Find how to eliminate this reparse *)

)
