open Optitrust
open Prelude

let _ = Flags.check_validity := true (* TODO *)

let _ = Run.script_cpp (fun _ ->
  !! Resources.ensure_computed ();
  let range a b = (trm_int a, trm_int b) in
  !! Matrix.local_name_tile ~alloc_instr:[cVarDef "a"] ~local_var:"y_local"
    [range 0 10; range 2 10; range 0 4] [occLast; cFor ~body:[cArrayWrite "a"] "i"];
  !! Matrix.local_name_tile ~alloc_instr:[cVarDef "b"] ~delete:true ~local_var:"y"
    [range 0 10; range 2 10; range 0 4] [cFor ~body:[cArrayWrite "b"] "j"];
  !! Matrix.local_name_tile ~alloc_instr:[cVarDef "c"] ~delete:true ~local_var:"z"
    [(var "i", trm_int 1); range 2 10; range 0 4] [cFor ~body:[cArrayWrite "c"] "j"];
)
