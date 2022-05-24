open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   !! Variable.inline [cTopFunDef "test_ref"; cVarDef "b"];
   !! Variable.inline [cTopFunDef "test_nonconst"; cVarDef "a"];
   !! Tools.failure_expected (fun () ->
      !! Variable.inline [cTopFunDef "test_nonconst_fail"; cVarDef "a"]);

   (* more complex test *)
   !! Variable.inline [cVarDef "p"];
)
