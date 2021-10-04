open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  (* With partitioning *)
  !! Loop.unroll ~braces:true ~shuffle:true ~blocks:[2;3] [cFor "i"];
  !! Loop.unroll ~braces:false  [cFor "j"];
  
  (* Without partitioning *)
  !! Trace.alternative (fun _ -> 
    !! Loop.unroll ~braces:true [cFor "i"];
    !! (););

  (* Hiding braces *)
  !! Trace.alternative (fun () ->
    !! Loop.unroll [cFor "i"];
    !!())
)

