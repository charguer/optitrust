open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    (* Example with detach of initialization *)
    !! Struct.set_explicit [cVarDef "p"];
    (* Another example with more complex initializers *)
    !! Struct.set_explicit [sInstr "obj a = "];
    !! Struct_basic.set_explicit [sInstr "a.speed = "];
    (* Another example with a more complex right-hand side *)
    !! Struct.set_explicit [cVarDef "u"];
    (* Example without detach *)
    !! Struct.set_explicit [sInstr "b = p"];
)
