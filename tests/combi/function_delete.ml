open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   !! Function.delete [cFunDef "f"];
   (* TODO: why failure is only at the next reparse ?
   !! Trace.failure_expected (fun () ->
       !! Function.delete [cFunDef "g"];
       !! Trace.reparse () );
    *)
)
