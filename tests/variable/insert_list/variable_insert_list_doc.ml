open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  !! Variable.insert_list  ~defs:[("int","c",lit "3"); ("int","d",lit "4")] [tAfter; cVarDef "b"]
  )
