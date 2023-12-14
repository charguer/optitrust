open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  (* With partitioning *)
  !! Loop.unroll ~braces:false ~shuffle:true ~blocks:[2;1;2] [cFor "i"];
  !! Loop.unroll [cFor "j"];
  !! Loop.unroll ~nest_of:2 [cFor "k"];
  (* FIXME: scopes are broken here *)

  (* Without partitioning *)
  !! Trace.alternative (fun _ ->
    !! Loop.unroll  [cFor "i"];
    !! (););

  (* Hiding braces *)
  !! Trace.restore_original();
  !! Loop.unroll ~braces:false [cFor "i"];

)
