open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  let call_tg = [occIndex 0; cTopFunDef "main"; cFun "f"]  in

  let f1 = find_var "f1" [] in
  let f2 = find_var "f2" [] in
  let f3 = find_var "f3" [] in
  let f4 = find_var "f4" [] in
  !! Specialize_basic.funcalls f1 [true; true] call_tg;
  !! Specialize_basic.funcalls f2 [true; false] call_tg;
  !! Specialize_basic.funcalls f3 [false; true] call_tg;
  !! Specialize_basic.funcalls f4 [false; false] call_tg;
)
