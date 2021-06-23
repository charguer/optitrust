open Optitrust
open Target
(* TODO: Not yet implemented*)
let _ = Run.script_cpp (
    fun () -> 
    let show = Generic.target_show in
    show [cVarDef "a"] ~debug_ast:true;
  )