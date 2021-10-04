open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 
  
  !! Variable.local_other_name (Ast.typ_constr "T") "a"  "x" ;
  !! Trace.alternative (fun _ ->
    !! Sequence_basic.intro_on_instr ~mark:"section_of_interest" ~visible:false [cFor "i"];
    !! Variable_basic.local_other_name (Ast.typ_constr "T") "a"  "x" [cMark "section_of_interest"];
    !! ());
)
