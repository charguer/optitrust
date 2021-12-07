open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

   (* for variables *)
   !! Variable_basic.inline_at [cVarDef "b"] [cVarDef "a"];
   !! Variable_basic.inline [cVarDef "c"];

   (* for functions *)
   Tools.failure_expected (fun () ->
    !! Variable_basic.inline ~accept_functions:false [cFunDef "f"];);

   !! Variable_basic.inline [cFunDef "f"];

)
