open Optitrust
open Target 

let () =
  Run.script_cpp (fun () ->
      !! Apac_preprocessing.explode_let_mult [
          nbMulti;
          cVarsDef ""
        ];
    );
  Apac_reset.tnt_blast ()
