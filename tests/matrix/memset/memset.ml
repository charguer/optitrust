open Optitrust
open Prelude

let _ = Run.script_cpp (fun() ->
  !! Matrix.memset [nbMulti; cFor "i"];
  (** Check the size of the array matches the loop size *)
 !! Trace.failure_expected (fun _e -> true) (fun _ ->
     Matrix.memset [cFor "k"];);
)
