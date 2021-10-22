open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  Variable.local_name ~mark:"mymark" ~var_type:(Ast.typ_constr "T") ~var:"a"  ~local_var:"x" [cFor "i"];
)
