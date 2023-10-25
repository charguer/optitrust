open Optitrust
open Target
open Prelude



let _ = Run.script_cpp (fun () ->

  !! Align_basic.def (lit "16") [nbMulti; cVarDef ~regexp:true "coeff.*"];
  !! Align_basic.def (lit "16") [nbMulti; cVarDef "a"];

)
