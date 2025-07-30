open Optitrust
open Prelude

let _ =
  Flags.pretty_matrix_notation := true;
  Flags.print_optitrust_syntax := true

let _ =
  Run.script_cpp (fun () ->
      !!Loop.tile (trm_find_var "GS" []) ~index:"j" ~bound:TileBoundMin ~iter:TileIterLocal
        [ cFunBody "matvec"; cFor "k" ])
