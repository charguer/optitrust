open Optitrust
open Prelude
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.extend_range ~start:ExtendToZero ~stop:(ExtendTo (expr "r")) [cFor "i"];
)
