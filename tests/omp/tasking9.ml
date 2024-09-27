open Optitrust
open Target

(* FIX ME! *)

let _ = Run.script_cpp (fun _ ->

  !! Omp.task [] [tFirst; cFunDef "work";dBody];

  !! Omp.task [] [tBefore;occIndex ~nb:2 0;cSeq ~args:[[cSeq ~args:[[cCall "printf"]] ()]] ()];
  !! Omp.critical "" [tBefore;occIndex ~nb:2 0;cSeq ~args:[[cCall "printf"]] ()];

  !! Omp.task [] [tBefore;cSeq ~args:[[cSeq ~args:[[cCall "printf"]] ()]] ()];
  !! Omp.critical "" [tBefore;occIndex ~nb:2 1;cSeq ~args:[[cCall "printf"]] ()];

)
