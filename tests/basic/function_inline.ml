open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Function_basic.inline ~body_mark:"bodyf" [cFun "f"];
  !! Function_basic.inline ~body_mark:"bodyg" [cFun "g"];
  !! Function_basic.inline ~body_mark:"bodyh" [cFun "h"];
  !! Function_basic.inline ~body_mark:"bodym" [cFun "m"];
)

(* NOTE: this basic transformation assumes a call of the form
     int x = f(args)
    or
      f(arggs)  // with unit return type
*)

