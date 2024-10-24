
open Optitrust
open Target

(* Fix this unit test *)
let _ = Run.script_cpp (fun () ->

  !! Function.inline [cCall "f"];

)
