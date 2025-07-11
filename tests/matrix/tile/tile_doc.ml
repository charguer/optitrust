open Optitrust
open Target
open Prelude

let _ =
  Run.script_cpp (fun _ ->
      !!Matrix.tile
        ~block_size:(trm_find_var "block_size" [])
        ~index_dim:1
        [ cVarDef "a" ])
