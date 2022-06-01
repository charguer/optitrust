open Optitrust
open Target

(* TODO: Fix me *)
let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [occIndex ~nb:3 0; cFun "printf"];
  !! Sequence_basic.intro 1 [cSeq ~args:[[cFun "printf"]] ()];
  !! Sequence_basic.intro 1 [occIndex ~nb:3 1; cFun "printf"];
  !! Sequence_basic.intro 1 [occIndex ~nb:2 1;cSeq ~args:[[cFun "printf"]] ()];
  !! Sequence_basic.intro 1 [occIndex ~nb:3 2; cFun "printf"];
  !! Sequence.intro ~start:[tFirst; cFunDef "main"; dBody] ~stop:[tLast; cFunDef "main"; dBody] ();
  (* !! Omp.parallel [] [tFirst; cFunDef "main"; dBody];
  !! Omp.set_dynamic 0 [tFirst; cFunDef "main"; dBody];
  !! Omp.set_nested 1 [tFirst; cFunDef "main"; dBody];
  !! Omp.parallel [] [tBefore; occIndex ~nb:2 0;cSeq ~args:[[cSeq ~args:[[cFun "printf"]] ()]] ()];
  !! Omp.single [] [tBefore;occIndex ~nb:3 0; cSeq ~args:[[cFun "printf"]] ()];
  !! Omp.parallel [] [tBefore; cSeq ~args:[[cSeq ~args:[[cFun "printf"]] ()]] ()];
  !! Omp.single [] [tBefore;occIndex ~nb:3 1; cSeq ~args:[[cFun "printf"]] ()];
  !! Omp.barrier [tBefore;occIndex ~nb:3 2;cSeq ~args:[[cFun "printf"]] ()];
  !! Omp.single [] [tBefore;occIndex ~nb:3 2;cSeq ~args:[[cFun "printf"]] ()]; *)
)
