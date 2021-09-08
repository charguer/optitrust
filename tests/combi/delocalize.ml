open Optitrust
open Target
open Ast

let _ =  Run.script_cpp ( fun () ->
  !! Generic.delocalize ~label:"section_of_interest" ~old_var:"a" ~new_var:"x" (typ_constr "T") "N" (Delocalize_arith (Lit_int 0, Binop_add) );
  !! Trace.alternative (fun () ->
    Variable.local_other_name ~label:"section_of_interest" (typ_constr "T") "a" "x";
    Generic_basic.delocalize "N" (Delocalize_arith (Lit_int 0, Binop_add)) [cLabel "section_of_interest";dBody];
    !! ());
)
