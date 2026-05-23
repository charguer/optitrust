open Optitrust
open Target 

let () =
  Apac_reset.tnt_blast ();
  Run.script_cpp ~check_syntax_at_end:true (fun () ->
      !! Apac_preprocessing.Constification.constify [
          nbAny;
          cFunDefAndDecl ""
        ];
    )
