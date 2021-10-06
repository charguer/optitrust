open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 
  
  let mark = "section_of_interest" in
  !! Variable.local_other_name  ~mark ~var_type:(Ast.typ_constr "T") ~old_var:"a"  ~new_var:"x" [cFor "i"]; 
)
