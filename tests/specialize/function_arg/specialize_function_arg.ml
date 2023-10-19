open Optitrust
open Target


let _ = Run.script_cpp (fun _ -> 


    let tg = [occFirst; cTopFunDef "main"; cFun "f"] in
  !! Specialize.function_arg "f1" [true; true] tg;
  
  !! Specialize.function_arg "f2" [true; false] tg;

  !! Specialize.function_arg "f3" [false; true] tg;

  !! Specialize.function_arg "f4" [false; false] tg;
)
