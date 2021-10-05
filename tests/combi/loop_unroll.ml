open Optitrust
open Target



(* TODO: Fix the issue with braces parameter for the second loop *)
let _ = Run.script_cpp (fun _ ->
  
  (* With partitioning *)
  !! Loop.unroll ~braces:false ~shuffle:true ~blocks:[2;3] [cFor "i"];
  !! Loop.unroll  [cFor "j"];
  
  (* Without partitioning *)
  !! Trace.alternative (fun _ -> 
    !! Loop.unroll ~braces:true [cFor "i"];
    !! (););

  (* Hiding braces *)
  !! Trace.alternative (fun () ->
    !! Loop.unroll [cFor "i"];
    !!())
)

