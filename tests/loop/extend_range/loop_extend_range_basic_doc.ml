open Optitrust
open Prelude
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.extend_range ~start:ExtendToZero ~stop:(ExtendTo (expr "r")) [cFor "i"];

  !!! (); (* TODO: Find how to eliminate this reparse *)
)
