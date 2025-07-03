open Optitrust
open Prelude
let _ = Flags.pretty_matrix_notation := true

let _ = Run.script_cpp (fun () ->
  !! (Function.inline [cFunDef "generate_prompt_proc" ; cCall "forward" ]);
  !! (Loop.hoist_alloc_loop_list [0] [nbMulti;cFunDef "generate_prompt_proc" ;cVarDef ~substr:true ""])

)
