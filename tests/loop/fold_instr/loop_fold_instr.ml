open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Loop.fold_instrs  ~index:"k" [sInstrRegexp "values\\[.\\] ="];
)