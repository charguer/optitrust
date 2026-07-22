open Optitrust
open Prelude

(* Difficulty: Level 5
   Request: target the instruction span around the first write to d_partial_sums
   Expected target: [tSpanAround [occFirst; cArrayWrite "d_partial_sums"]]
   Accepted variant: [tSpan [tBefore; occFirst; cArrayWrite "d_partial_sums"] [tAfter; occFirst; cArrayWrite "d_partial_sums"]] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [tSpanAround [occFirst; cArrayWrite "d_partial_sums"]];
)
