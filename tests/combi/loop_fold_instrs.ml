open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  (* NOTE: nbMulti is added by default *)
  !! Loop.fold_instrs  ~index:"k" [cWriteVar "a"];
  !! Loop.fold_instrs  ~index:"k" [cCellWrite ~base:[cVar "values"] ~index:[] ()];
)
