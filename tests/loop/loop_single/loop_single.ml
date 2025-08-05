open Optitrust
open Prelude

let _ =
  Run.script_cpp (fun () ->
      !!Loop.loop_single [ cFunBody "main"; cVarDef "k" ];
      !!Loop.loop_single [ cFunBody "main2"; cVarDef "k" ];
      !!Loop.loop_single [ cFunBody "main3"; cVarDef "k" ])
