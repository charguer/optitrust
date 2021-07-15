open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  (* show [cVarDef "bagsNext"]; *)
  show  [cFunDef "main";cFun "bag_push"];
  (* show [cTopFun "main";cFun "bag_push"]; *)
  !! Generic.from_one_to_many ["bagsNextPrivate"; "begsNextShared"] [cVarDef "bagsNext"];
  !!! Generic.arbitrary_if "isNeighbor(idCell2, idCell)" [cFunDef "main";cFun "bag_push"];
)