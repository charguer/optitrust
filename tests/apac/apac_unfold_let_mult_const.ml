open Optitrust
open Target 

let _ = Run.script_cpp (fun () ->
            let all : bool list = List.init 10 (Fun.const true) in
            !! Apac_prologue.unfold_let_mult
              ~constify:all [nbAny; cVarsDef "" ];
          )
