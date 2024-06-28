open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
            !! Apac_prologue.use_goto_for_return [cFunDef "g"];
            !! Apac_prologue.use_goto_for_return [cFunDef "f"];
            !! Apac_prologue.use_goto_for_return [cFunDef "h"];
            !! Apac_prologue.use_goto_for_return [cFunDef "i"];
            !! Apac_prologue.use_goto_for_return [cFunDef "main"];
          );
        Apac_reset.tnt_blast ()
