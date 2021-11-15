open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  (* NOTE: nbMulti is added by default *)
  !! Loop.fold_instrs  ~index:"k" [cCellWrite ~base:[cVar "values"] ~index:[]];
)

(* LATER: Loop.fold will compute automatically the difference between the instructions
   in order to guess what should be the index *)