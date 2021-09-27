open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    
    (* TODO: FIX ME!
    ExpectedFailure : !! Function.inline  [cFun "f"];
     --> please provide a name for the result, because it is a struct and thus cannot be inlined
    *)

    !! Function.inline ~name_result:"r" [cFun "g"; cFun "f"];
    (* !! Function.inline [cVarDef "s"; cFun "f"]; *)
    !! Function.inline ~name_result:"r" [cVarDef "p"; cFun "f"];

)