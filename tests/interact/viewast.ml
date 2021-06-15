(* Usage:

      F7 on a given line, or shift+F7 to recompile optitrust

   or by hand, in src/.vscode, run:
     ./view_result.sh ../tests/interact interact 11 view_result recompile_optitrust_no

*)

open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
    !!();
    )

