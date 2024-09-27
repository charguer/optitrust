open Optitrust
open Target


let _  = Run.script_cpp (fun () ->

  (* !!(); *)
  !! Function.bind_args ["a"] [cTopFunDef "main"; cCall "g"];

)
