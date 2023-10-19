open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   !! Variable.unfold [cTopFunDef "test_ref"; cVarDef "b"];
   !! Variable.unfold [cTopFunDef "test_nonconst"; cVarDef "a"];
   !! Trace.failure_expected (fun () ->
      !! Variable.unfold [cTopFunDef "test_nonconst_fail"; cVarDef "a"]);

   (* more complex test *)
   !! Variable.unfold [cVarDef "p"];
)
