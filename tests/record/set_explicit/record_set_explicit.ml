open Optitrust
open Target

(* let _ = Flags.check_validity := true *)

let _ = Run.script_cpp (fun _ ->
  (* Example with detach of initialization *)
  !! Record.set_explicit [cVarDef "p"];
  (* Another example with more complex initializers *)
  !! Record.set_explicit [sInstr "obj a = "];
  !! Record.set_explicit [sInstr "a.speed = "];
  (* Another example with a more complex right-hand side *)
  !! Record.set_explicit [cVarDef "u"];
  (* Example without detach *)
  !! Record.set_explicit [sInstr "b = p"];
  (* Example with const *)
  !! Record.set_explicit [sInstr "*v = c"];
)
