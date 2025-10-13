open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->
  let lines_per_thread = 32 in
  let vector_size = 4 in

  bigstep "---- reorder loops ----";

  !! Loop.reorder ~order:["y"; "x"] [nbMulti; cFunDef "blur"; cFor "x"];

  bigstep "---- vector parallelism ----";

  !! Loop.tile (trm_int vector_size) ~index:"bx" ~bound:TileDivides [nbMulti; cFunDef "blur"; cFor "x"];
  !! Omp.simd [nbMulti; cFunDef "blur"; cFor "x"];

  bigstep "---- thread parallelism ----";

  !! Loop.fusion [occFirst; cFunDef "blur"; cFor "y"];
  !! Loop.tile (trm_int lines_per_thread) ~index:"by" ~bound:TileBoundAnd [cFunDef "blur"; cFor "y"];
  !! Omp.parallel_for [cFunDef "blur"; cFor "by"];
)
