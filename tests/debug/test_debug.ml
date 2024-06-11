open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->

   !! Show.trm_text (Trace.ast())
)
