open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   !! Variable.inline [cTopFunDef "test_ref"; cVarDef "b"];
   !! Variable.inline ~simpl:(fun tg -> Arith.(simpl_surrounding_expr gather (nbAny :: tg))) [cTopFunDef "test_nonconst"; cVarDef "a"];
   !! Tools.failure_expected (fun () ->
      !! Variable.inline [cTopFunDef "test_nonconst_fail"; cVarDef "a"]);

   (* more complex test *)
   !! Variable.inline [cVarDef "p"];
)
