open Optitrust
open Prelude

let _ =
  Run.script_cpp (fun _ ->
      !!Loop.refactor_if_in_loop [ cFunBody "main"; cFor "i" ];
      !!Loop.refactor_if_in_loop [ cFunBody "main3"; cFor "i" ];
      !!Loop.refactor_if_in_loop [ cFunBody "main4"; cFor "i" ];
      !!Loop.refactor_if_in_loop [ cFunBody "main2"; cFor "i" ])
