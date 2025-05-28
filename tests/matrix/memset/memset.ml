open Optitrust
open Prelude

let _ =
  Run.script_cpp (fun () ->
      !!Matrix.memset [ nbMulti; cFor "i" ];
      !!Trace.failure_expected
        (fun _e -> true)
        (fun _ -> Matrix.memset [ nbMulti; cFor "k" ]))
