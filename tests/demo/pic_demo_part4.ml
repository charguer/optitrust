open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  (*
   !! Sequence.insert "const int nbColors = 8" [tBefore; cVarDef "charge"];
   !! Sequence.insert "const int nbCellsPerTile = 8" [tBefore; cVarDef "charge"];
   !! Sequence.insert "const int nbCellsPerColor = nbCells / nbColors" [tBefore; cVarDef "charge"];
   !! Sequence.insert "const int nbTiles = nbCells / nbCellsPerTile" [tBefore; cVarDef "charge"];
   !! Sequence.insert "const int nbTilesPerColor = nbTiles / nbColors"[tBefore; cVarDef "charge"];
   !! Sequence.insert "int idCellsOfTile[nbTiles][nbCellsPerTile]" [tBefore; cVarDef "charge"];
   *)
   !! let p = [tBefore; cVarDef "charge"] in
   let lines = [
     "const int nbColors = 8";
     "const int nbCellsPerTile = 8"; ] in
   List.iter (fun s -> Sequence.insert s p) lines;
   (* LATER: in the long term, we can have Sequence.inserts which takes a list of lines *)

   !! Loop.color "nbColors" "color" [cFor "step";cFor "idCell"] ;
   !! Loop.tile "nbTilesPerColor" "idTileInColor" [cFor "idCell"];
   !! Loop.tile "idCellOfTile" "nbCellsPerTile" [cFor "idCell"];



)


