open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

  !! Loop.fold  ~index:"k"  ~start:"0" ~stop:"3" ~step:"1" 3 [cIndexSet ~base:[cVar "values"] [cInt 0]];
)
