open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Matrix_basic.simpl_access_of_access [cVarInit "q"];
  !! Matrix_basic.simpl_index_add [cVarInit "r0"];
  !! Matrix_basic.simpl_index_add [cVarInit "r1"; dArg 0];
  !! Matrix_basic.simpl_index_add [cVarInit "r1"];
  !! Matrix_basic.simpl_index_add [cVarInit "r2"];
  (* TODO: check that transformation fails:
  !! Matrix_basic.simpl_index [cVarInit "r3"];
  *)
)
