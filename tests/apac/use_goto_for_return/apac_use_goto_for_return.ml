open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Apac_basic.use_goto_for_return [cFunDef "g"];
  !! Apac_basic.use_goto_for_return [cFunDef "f"];
  !! Apac_basic.use_goto_for_return [cFunDef "h"];
  !! Apac_basic.use_goto_for_return [cFunDef "i"];
  !! Apac_basic.use_goto_for_return [cFunDef "main"];
)
