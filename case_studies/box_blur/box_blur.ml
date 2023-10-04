open Optitrust
open Target
open Prelude

let by x = (lit (string_of_int x))

let _ = Run.script_cpp (fun () ->
  let lines_per_thread = 32 in
  let vector_size = 4 in

  bigstep "---- reorder loops ----";

  !! Loop.reorder ~order:["y"; "x"] [nbMulti; cFunDef "blur"; cFor "x"];

  bigstep "---- vector parallelism ----";

  !! Loop.tile (by vector_size) ~index:"bx" ~bound:TileDivides [nbMulti; cFunDef "blur"; cFor "x"];
  !! Omp.ensure_header ();
  !! Omp.simd [nbMulti; cFunDef "blur"; cFor "x"];

  bigstep "---- thread parallelism ----";

  !! Loop.fusion [occFirst; cFunDef "blur"; cFor "y"];
  !! Loop.tile (by lines_per_thread) ~index:"by" ~bound:TileBoundAnd [cFunDef "blur"; cFor "y"];
  !! Omp.parallel_for [cFunDef "blur"; cFor "by"];
)