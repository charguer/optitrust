open Optitrust
open Target
open Ast

let _ =  Run.script_cpp ( fun () ->
  !! Variable.delocalize ~index:"k" ~mark:"A" ~var:"a" ~local_var:"x" ~var_type:(typ_constr "T") ~array_size:"N" ~ops:(Delocalize_arith (Lit_int 0, Binop_add) ) [cFor "i"];
  !! Trace.alternative (fun () ->
    !! Variable.local_name  ~var_type:(typ_constr "T") ~var:"a" ~local_var:"x" [cFor "i"];
    !! Variable_basic.delocalize  ~array_size:"N" ~ops:(Delocalize_arith (Lit_int 0, Binop_add)) [cMark "section_of_interest"] ;
    !! ());
  !! Variable.delocalize ~index:"k" ~mark:"B" ~var:"a" ~local_var:"y" ~var_type:(typ_constr "T") ~array_size:"N" ~ops:(Delocalize_obj ("clean", "transfer")) [cFor "j"];
  !! Trace.alternative (fun () ->
    !! Variable.local_name  ~var_type:(typ_constr "T") ~var:"a" ~local_var:"x" [cFor "i"];
    !! Variable_basic.delocalize  ~array_size:"N" ~ops:(Delocalize_obj ("clean", "transfer")) [cMark "section_of_interest"] ;
    !! ());
)
