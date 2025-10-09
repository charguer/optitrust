open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  (* !!Matrix.insert_alloc_dim (expr "N2") [ cVarInit "p" ];*)
  !! Loop.extend_range_array_size [ cFunBody "main"; cVarInit "p" ] [ cFunBody "main"; cFor "i" ];
  !! Loop.extend_range_array_size ~dim:1 [ cFunBody "main2"; cVarInit "p" ] [ cFunBody "main2"; cFor "i" ];
)
