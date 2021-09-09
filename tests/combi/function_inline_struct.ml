open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    (* TODO
    ExpectedFailure : !! Function.inline_call  [cFun "f"];
     --> please provide a name for the result, because it is a struct and thus cannot be inlined
    *)

    (* !! Function.inline_call ~name_result:"r" [ cFun "f"]; *)
    !! Function.inline_call ~name_result:"r" [cVarDef "s"; cFun "f"];
    !! Function.inline_call ~name_result:"r" [cVarDef "p"; cFun "f"];

)