open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.bind_intro  ~fresh_name:"a" ~const:true [cFunDef "test"; cReturn;cArrayInit];
  !! Variable_basic.bind_intro  ~fresh_name:"b"  [cVarDef "x"; cArrayInit];
)
