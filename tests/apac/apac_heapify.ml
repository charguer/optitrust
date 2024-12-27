open Optitrust
open Target 

let () =
  Run.script_cpp ~check_syntax_at_end:true (fun () ->
      !! Apac_preprocessing.explode_let_mult [
          nbMulti;
          cVarsDef ""
        ];
      !! Apac_parallelization.heapify [
          cFunBody "main"
        ];
    );
  Apac_reset.tnt_blast ()
