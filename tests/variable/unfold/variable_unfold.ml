open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  (* TODO: show delete:false *)
  (* TODO: delete:true is inline ? remove option? *)

   !! Variable.unfold ~delete:true [cTopFunDef "test_ref"; cVarDef "b"];
   !! Variable.unfold ~delete:true [cTopFunDef "test_nonconst"; cVarDef "a"];
   !! Trace.failure_expected (fun _e -> true) (fun () ->
      Variable.unfold ~delete:true [cTopFunDef "test_nonconst_fail"; cVarDef "a"]);

   (* more complex test *)
   !! Variable.unfold ~delete:true [cVarDef "p"];
)
