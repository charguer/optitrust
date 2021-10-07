open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  Variable.local_other_name ~mark:"mymark" ~var_type:(Ast.typ_constr "T") ~old_var:"a"  ~new_var:"x" [cFor "i"];
)
