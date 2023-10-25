open Optitrust
open Target

(*
  LATER: we could also implement and test
    Variable_basic.rename ~pattern:(fun oldname -> "new_" ^ oldname) [cVarDef ~regexp:true "x."]
*)

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.rename ~into:"z" [cVarDef "x"];

)
