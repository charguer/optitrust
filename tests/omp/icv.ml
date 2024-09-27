open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 1 [occIndex ~nb:2 0; cCall "printf"];
  !! Sequence_basic.intro 1 [occIndex ~nb:2 1; cCall "printf"];
  !! Sequence_basic.intro 1 [occIndex ~nb:2 0; cSeq ~args:[[cCall "printf"]] ()];
  !! Sequence_basic.intro 1 [occIndex ~nb:2 1; cSeq ~args:[[cCall "printf"]] ()];
  !! Sequence.intro ~start:[tFirst; cFunBody "main"] ~stop:[tBefore;cReturn] ();
  !! Omp.parallel [] [tFirst; cFunBody "main"];
  !! Omp.set_num_threads 2 [tFirst; cFunBody "main"];
  !! Omp.set_dynamic 0 [tFirst; cFunBody "main"];
  !! Omp.set_max_active_levels 8 [tFirst; cFunBody "main"];
  !! Omp.set_nested 1 [tFirst; cFunBody "main"];
  !! Omp.set_num_threads 3 [tBefore; occIndex ~nb:2 0;cSeq ~args:[[cSeq ~args:[[cCall "printf"]] ()]] ()];
  !! Omp.parallel [] [tBefore; occIndex ~nb:2 0;cSeq ~args:[[cSeq ~args:[[cCall "printf"]] ()]] ()];
  !! Omp.barrier [tBefore; occIndex ~nb:2 1;cSeq ~args:[[cSeq ~args:[[cCall "printf"]] ()]] ()];
  !! Omp.single [] [tBefore; occIndex ~nb:2 1;cSeq ~args:[[cSeq ~args:[[cCall "printf"]] ()]] ()];
  !! Omp.set_num_threads 4 [tBefore; occIndex ~nb:2 0; cSeq ~args:[[cCall "printf"]] ()];
  !! Omp.single [] [tBefore; occIndex ~nb:2 0;cSeq ~args:[[cCall "printf"]] ()];

)
