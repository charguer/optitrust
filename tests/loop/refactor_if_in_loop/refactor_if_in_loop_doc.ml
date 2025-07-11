open Optitrust
open Prelude

let _ =
  Run.script_cpp (fun _ ->
    !! Loop.refactor_if_in_loop[ cFunBody "main"; cFor "i" ];
      )
