open Optitrust
open Target

(* TODO: Fix me *)
let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [occIndex ~nb:3 0; cCall "printf"];
  !! Sequence_basic.intro 1 [cSeq ~args:[[cCall "printf"]] ()];
  !! Sequence_basic.intro 1 [occIndex ~nb:3 1; cCall "printf"];
  !! Sequence_basic.intro 1 [occIndex ~nb:2 1;cSeq ~args:[[cCall "printf"]] ()];
  !! Sequence_basic.intro 1 [occIndex ~nb:3 2; cCall "printf"];
  !! Sequence.intro ~start:[tFirst; cFunBody "main"] ~stop:[tLast; cFunBody "main"] ();
  (* !! Omp.parallel [] [tFirst; cFunBody "main"];
  !! Omp.set_dynamic 0 [tFirst; cFunBody "main"];
  !! Omp.set_nested 1 [tFirst; cFunBody "main"];
  !! Omp.parallel [] [tBefore; occIndex ~nb:2 0;cSeq ~args:[[cSeq ~args:[[cCall "printf"]] ()]] ()];
  !! Omp.single [] [tBefore;occIndex ~nb:3 0; cSeq ~args:[[cCall "printf"]] ()];
  !! Omp.parallel [] [tBefore; cSeq ~args:[[cSeq ~args:[[cCall "printf"]] ()]] ()];
  !! Omp.single [] [tBefore;occIndex ~nb:3 1; cSeq ~args:[[cCall "printf"]] ()];
  !! Omp.barrier [tBefore;occIndex ~nb:3 2;cSeq ~args:[[cCall "printf"]] ()];
  !! Omp.single [] [tBefore;occIndex ~nb:3 2;cSeq ~args:[[cCall "printf"]] ()]; *)
)
