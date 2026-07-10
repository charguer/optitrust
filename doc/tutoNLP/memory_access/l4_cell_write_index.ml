open Optitrust
open Prelude

(* Difficulty: Level 4
   Request: target writes to t indexed by i
   Expected target: [cCellWrite ~base:[cVar "t"] ~index:[cVar "i"] ()]
   Accepted broader variant: [cArrayWrite "t"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cCellWrite ~base:[cVar "t"] ~index:[cVar "i"] ()];
)
