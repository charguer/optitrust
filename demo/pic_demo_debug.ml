open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
   !! Sequence.insert "const int nbColors = 8" [tBefore; cVarDef "charge"];
   !! Sequence.insert "const int nbCellsPerTile = 8" [tBefore; cVarDef "charge"];
   !! Sequence.insert "const int nbCellsPerColor = nbCells / nbColors" [tBefore; cVarDef "charge"];
   !! Sequence.insert "const int nbTiles = nbCells / nbCellsPerTile" [tBefore; cVarDef "charge"];
   !! Sequence.insert "const int nbTilesPerColor = nbTiles / nbColors"[tBefore; cVarDef "charge"];
   !! Sequence.insert "int idCellsOfTile[nbTiles][nbCellsPerTile]" [tBefore; cVarDef "charge"];
   !! Loop.color "nbColors" "color" [cFor "step";cFor "idCell"] ;
   !! Loop.tile "nbTilesPerColor" "idTileInColor" [cFor "idCell"];
   !! Loop.tile "idCellOfTile" "nbCellsPerTile" [cFor "idCell"];



)


