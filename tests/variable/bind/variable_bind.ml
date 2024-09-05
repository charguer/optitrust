open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Variable_basic.bind "a" ~const:true [cFunDef "test"; cReturn; cArrayInit];
  !! Variable_basic.bind "b" [cVarDef "arr"; cArrayInit];

  (* FIXME:
  !! Variable_basic.bind ~const:true "a0" [nbMulti; sExpr "arr[0]"];
     ~===~ [cCellAccess ()]
  *) (* LATER: remove the show *)
  (*show [cCellAccess ~base:[cVar "arr"] ()];
  show [cAccesses ~base:[cStrict; cCellAccess ~base:[cVar "arr"] ()] ()];*)
  !! Variable_basic.bind "a0" [nbMulti; cRead ~addr:[cAccesses ~base:[cStrict; cCellAccess ~base:[cVar "arr"] ()] ()] ()];

  !! Variable_basic.bind "z" [cFun "pointer_arg"; dArg 0];
)
