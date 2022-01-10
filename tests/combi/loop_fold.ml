open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Loop.fold  ~index:"k"  3 [cCellWrite ~base:[cVar "values"] ~index:[cInt 0] ()];
)


(* LATER: Loop.fold will compute automatically the difference between the instructions
   in order to guess what should be the index *)