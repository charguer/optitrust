open Optitrust
open Target 

let () =
  Apac_flags.verbose := true;
  Run.script_cpp (fun () ->
      !! Apac_preprocessing.Constification.constify [
          nbAny;
          cFunDefAndDecl ""
        ];
    );
  Apac_reset.tnt_blast ()
