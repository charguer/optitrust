
open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Loop.reorder ~order:["d";"c";"b"] [cFor "b"];

  !! Trace.alternative (fun () ->
    !! Loop.reorder ~order:["b";"d";"e";"a";"c" ] [cFor "a"];
    !!(););

  !! Trace.alternative (fun () ->
    !! Loop.reorder ~order:["e";"d"] [cFor "d"];
    !! Loop.reorder ~order:["a"] [cFor "a"]; (* identity *)
    !!(););

  !! Trace.alternative (fun () ->
    !! Trace.failure_expected (fun () ->
       Loop.reorder ~order:["e"] [cFor "a"];);
    !! Trace.failure_expected (fun () ->
       Loop.reorder ~order:["e"; "f"] [cFor "e"];)
    );

)
