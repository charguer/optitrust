open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 
  
  let mark = "section_of_interest" in
  !! Variable.local_other_name  ~mark ~var_type:(Ast.typ_constr "T") ~old_var:"a"  ~new_var:"x" ();
  !! Trace.alternative (fun _ ->
    !! Sequence_basic.intro_on_instr ~mark ~visible:false [cFor "i"];
    !! Variable_basic.local_other_name  ~var_type:(Ast.typ_constr "T") ~old_var:"a"  ~new_var:"x" [cMark "section_of_interest"];
    !! ());
)
