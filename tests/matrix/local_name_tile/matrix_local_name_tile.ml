open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  let tile offset size = (trm_int offset, trm_int size) in
  !! Matrix.local_name_tile ~alloc_instr:[cVarDef "a"] [tile 0 10; tile 2 8; tile 0 4] [cFor ~body:[cArrayWrite "a"] "i"];
  !! Matrix.local_name_tile ~delete:true ~local_var:"y" [tile 0 10; tile 2 8; tile 0 4] ~alloc_instr:[cVarDef "b"] [cFor ~body:[cArrayWrite "b"] "j"];
  !! Matrix.local_name_tile ~delete:true ~local_var:"z" [(var "i", trm_int 1); tile 2 8; tile 0 4] ~alloc_instr:[cVarDef "c"] [cFor ~body:[cArrayWrite "c"] "j"];
)
