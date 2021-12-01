open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  Variable.local_name ~mark:"mymark" ~var_type:(Ast.atyp "T") ~var:"a"  ~local_var:"x" [cFor "i"];
)
