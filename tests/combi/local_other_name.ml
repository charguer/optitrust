open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 
  
  !! Sequence_basic.intro_on_instr ~label:"section_of_interest" ~visible:false [cFor "i"];
  !! Variable_basic.local_other_name (Ast.typ_constr "T") "a"  "x" [cLabel "section_of_interest";dBody];
)
