open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Arith_basic.(simpl gather) [cVarInit "a"];
  !! Arith_basic.(simpl gather) [cVarInit "b"];
  !! Arith_basic.(simpl gather) [cVarInit "c"];
  !! Arith_basic.(simpl expand) [cVarInit "d"];

)
