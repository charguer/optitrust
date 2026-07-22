open Optitrust
open Prelude

(* Difficulty: Level 5
   Request: target the write to d_partial_sums inside the bi and ti loops of reduce
   Expected target: [cFunDef "reduce"; cFor "bi"; cFor "ti"; cArrayWrite "d_partial_sums"]
   Accepted variant: [cTopFunDef "reduce"; cFor "bi"; cFor "ti"; cArrayWrite "d_partial_sums"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cFunDef "reduce"; cFor "bi"; cFor "ti"; cArrayWrite "d_partial_sums"];
)
