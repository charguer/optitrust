open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Function_basic.inline ~body_mark:"bodyf" [cFun "f"];
  !! Function_basic.inline ~body_mark:"bodyg" [cFun "g"];
  !! Function_basic.inline ~body_mark:"bodyh" [cFun "h"];
  !! Function_basic.inline ~body_mark:"bodym" [cFun "m"];
  !! Function_basic.inline ~body_mark:"bodyk" [cFun "k"];
)


