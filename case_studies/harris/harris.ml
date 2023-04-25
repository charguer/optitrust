open Optitrust
open Target
(* open Ast *)

let _ = Run.script_cpp (fun () ->
  (* TODO *)
  !! Omp.header ();
  show [cFunDef "conv3x3"];
)