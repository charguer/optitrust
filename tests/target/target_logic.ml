open Optitrust
open Target

let _ = Flags.execute_show_even_in_batch_mode := true

let _ = Run.script_cpp (fun () ->

  (* Or constraints *)
  show [ cOr [[cFor "i"];[cFor "j"]]];
  show [ cOr [[sInstr "i++"];[sInstr "j++"]]];

  (* And constraints *)
  show [ cAnd [[cFor "i"];[cFor ""]]];
  show [nbExact 0; cAnd []];
  show [nbExact 0; cAnd [[cFor "i"];[cFor "j"];[cFor "k"]]];
  show [ cAnd [[cWrite()]; [sInstr "k + j"]]];

  (* Diff constraints *)
  show [cDiff [[cFor ""]] [[cFor "j"]]];
  show [cDiff [[cFor ""]] [[cFor "j"];[cFor "k"]]];
)

  (*
   LATER: -- not the most urgent
  show [ sInstr "k++"];
  show [ sInstr "k < 13"];
  show [ cFor ~step:[sInstr "k++"]] "" ; *)
