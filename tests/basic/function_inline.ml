open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Function_basic.inline ~label:"bodyf" [cFun "f"];
  !! Function_basic.inline ~label:"bodyg" [cFun "g"];
  !! Function_basic.inline ~label:"bodyh" [cFun "h"];
  !! Function_basic.inline ~label:"bodym" [cFun "m"];
)

(* NOTE: this basic transformation assumes a call of the form
     int x = f(args)
    or
      f(arggs)  // with unit return type
*)

