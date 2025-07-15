open Optitrust
open Target
open Prelude

let _ =
  Run.script_cpp (fun _ ->
      !!Matrix.tile ~block_size:(trm_int 2) ~index_dim:0
        [ cFunDef "test"; cVarDef "a" ];
      !!Matrix.tile ~block_size:(trm_int 2) ~index_dim:0
        [ cFunDef "main"; cVarDefs [ "a"; "b" ] ];
      !!Matrix.tile ~block_size:(trm_int 2) ~index_dim:2
        [ cFunDef "main"; cVarDef "c" ];
      !!Matrix.tile ~block_size:(trm_int 2) ~index_dim:0
        [ cFunDef "main2"; cVarDefs [ "a"; "b"; "c" ] ];
      !!Matrix.tile ~block_size:(trm_int 2) ~index_dim:0
        [ cFunDef "main3"; cVarDef "a" ];
      let block_size, _ = find_var "block_size" [ cFunDef "main4" ] in
      !!Matrix.tile ~block_size:(trm_var block_size)
        ~nb_blocks:(trm_find_var "nb_blocks" [ cFunDef "main4" ])
        ~index_dim:0
        [ cFunDef "main4"; cVarDef "a" ];
      !!Matrix.tile
        ~block_size:(trm_find_var "block_size" [ cFunDef "main5" ])
        ~index_dim:0
        [ cFunDef "main5"; cVarDef "a" ])
