open Optitrust
open Prelude

let _ = Flags.pretty_matrix_notation := true
let _ = Flags.clang_format_nb_columns := 50

let _ = Run.script_cpp (fun () ->

   !! Loop.tile (trm_int 4) ~index:"kj" ~iter:TileIterGlobal [cFor "k"];
   !! Loop.tile (trm_int 32) ~index:"bi" ~bound:TileDivides [cFor "i"];

   !! Loop.swap [cFor "bi"];
   !! Loop.swap [cFor "bi"];
   !! Loop.reorder ~order:["j"; "bi"; "i"] [cFor "i"];

   !! Loop.hoist [cVarDef "sum"];
   !! Loop.fission [cForBody "i"; tBetweenAll];
  )
