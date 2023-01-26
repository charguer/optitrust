open Optitrust
open Target
open Ast

(* FIXME: seems weird *)
let cArrayRead (x : var) : constr =
  cAccesses ~base:[cStrict; cCellAccess ~base:[cVar x] ()] ()

let foreach (_doc : string) (l : 'a list) (f: 'a -> unit) : unit =
  List.iter f l

let _ = Run.script_cpp (fun () ->
  let lines_per_thread = 32 in
  let vector_size = 8 in

  !! ();

  bigstep "---- reorder loops ----";

  !! Loop.reorder ~order:["y"; "x"] [nbMulti; cFunDef "blur"; cFor "x"];

  bigstep "---- thread parallelism ----";

  !! Loop.fusion [occFirst; cFunDef "blur"; cFor "y"];
  !! Loop.tile (lit (string_of_int lines_per_thread)) ~index:"by" ~bound:TileBoundAnd [nbMulti; cFunDef "blur"; cFor "y"];
  !! Omp.header ();
  !! Omp.parallel_for [cFunDef "blur"; cFor "by"];

  bigstep "---- vector parallelism ----";

  !! Loop.tile (lit (string_of_int vector_size)) ~index:"bx" ~bound:TileBoundDivides [nbMulti; cFunDef "blur"; cFor "x"];
  !! Omp.simd [nbMulti; cFunDef "blur"; cFor "x"];

  !!! ();
)