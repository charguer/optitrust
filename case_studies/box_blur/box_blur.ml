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

  bigstep "---- reorder loops ----";

  !! Loop.reorder ~order:["y"; "x"] [nbMulti; cFunDef "blur"; cFor "x"];

  !! Loop.fusion [occFirst; cFunDef "blur"; cFor "y"];

  !! Loop.tile (lit (string_of_int vector_size)) ~index:"bx" [nbMulti; cFunDef "blur"; cFor "x"];

  !! Loop.tile (lit (string_of_int lines_per_thread)) ~index:"by" [nbMulti; cFunDef "blur"; cFor "y"];

  bigstep "---- introduce vector and thread parallelism ----";

  (* -- Vectorize -- *)
  !! Omp.header ();
  !! Omp.simd [nbMulti; cFunDef "blur"; cFor "x"];

  (* -- Multi-thread -- *)
  !! Omp.parallel_for [cFunDef "blur"; cFor "by"];

  !!! ();
)