open Optitrust
open Target


let _ = Run.script_cpp (fun () ->

  !! Function.beta [cCall "f"];

)
