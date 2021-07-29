open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Generic_basic.from_one_to_many ["bagsNextPrivate"; "begsNextShared"] [cVarDef "bagsNext"];
  !! Generic_basic.arbitrary_if "isNeighbor(idCell2, idCell)" [cFunDef "main";cFun "bag_push" ~args_pred:(Target.target_list_one_st (cVar "bagsNextPrivate"))];
  !! Generic_basic.replace "bag_push_atomic" [cIf(); dElse; cFun "bag_push"; cVar "bag_push"];
  !! Sequence_basic.insert "bag_transfer(bagsCur[idCell], bagsNext[idCell])" [tAfter; cFun "bag_transfer" ~args_pred:(Target.target_list_one_st (cVar "bagsNextPrivate"))];
)