open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  (*
  !! Loop.fold "i" [cLabelled "iterations"];

  *) (* NOTE: nbMulti is added by default *)

  !! Loop.fold  ~index:"k"  ~start:0  ~step:1 3 [cCellWrite ~base:[cVar "values"] ~index:[cInt 0]];

)


(* LATER: Loop.fold will compute automatically the difference between the instructions
   in order to guess what should be the index *)