open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  (* Or constraints *)
  show [nbMulti; cOr [[cFor "i"];[cFor "j"]]];
  show [nbMulti; cOr [[sInstr "i++"];[sInstr "j++"]]];

  (* And constraints *)
  show [nbMulti; cAnd [[cFor "i"];[cFor ""]]];
  show [nbMulti; cAnd []];
  show [nbExact 0; cAnd [[cFor "i"];[cFor "j"];[cFor "k"]]];
  show [nbMulti; cAnd [[cSet()]; [sInstr "k + j"]]];
)
  (* TODO: -- not the most urgent
  show [nbMulti; sInstr "k++"];
  show [nbMulti; sInstr "k < 13"];
  show [nbMulti; cFor ~step:[sInstr "k++"]] "" ; *)
