open Optitrust
open Target
open Prelude

let _ = Run.script_cpp (fun _ ->

    !! Variable.insert_list  ~defs:[("const int","x", lit "1");("const int","y",expr "x/2");("const int","z",expr "y/x")] [tBefore; cFunDef "main"]

)