open Optitrust
open Target
open Ast

(* FIXME: seems weird *)
let cArrayRead (x : var) : constr =
  cAccesses ~base:[cStrict; cCellAccess ~base:[cVar x] ()] ()

let foreach (_doc : string) (l : 'a list) (f: 'a -> unit) : unit =
  List.iter f l

let _ = Run.script_cpp (fun () ->
  bigstep "---- reorder loops ----";

  !! Loop.reorder ~order:["y"; "x"] [nbMulti; cFunDef "blur"; cFor "x"];

  bigstep "---- introduce vector and thread parallelism ----";

  (* -- Vectorize -- *)
  !! Omp.header ();
  !! Omp.simd [nbMulti; cFunDef "blur"; cFor "x"];

  (* -- Multi-thread -- *)
  !! Omp.parallel_for [cFunDef "blur"; cFor "by"];
)