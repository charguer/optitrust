open Optitrust
open Run
(* TODO: Not yet implemented*)
let _ = run_unit_test (
    fun () -> 
    let show = Generic.target_show in
    show [cVarDef "a"] ~debug_ast:true;
  )