open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  (* With partitioning *)
  !! Loop.unroll [cFor "i"];
  !! Loop.unroll  [cFor "j"];

  (* Without partitioning *)
  !! Trace.alternative (fun _ ->
    !! Loop.unroll  [cFor "i"];
    !! (););

  (* Hiding braces *)
  !! Trace.alternative (fun () ->
    !! Loop.unroll ~braces:false [cFor "i"];
    !!())
)

