open Optitrust
open Target
open Ast

let _ =  Run.script_cpp ( fun () ->
  !! Variable.delocalize "a" ~into:"x" ~index:"k" ~mark:"A" ~array_size:"N" ~ops:(Local_arith (Lit_int 0, Binop_add) ) [cFor "i"];
  !! Trace.alternative (fun () ->
    !! Variable.local_name "a" ~into:"x" ~mark:"section_of_interest" [cFor "i"];
    !! Variable_basic.delocalize  ~array_size:"N" ~ops:(Local_arith (Lit_int 0, Binop_add)) [cMark "section_of_interest"] ;
    !! ());
  !! Variable.delocalize "a" ~into:"y" ~index:"k" ~mark:"B" ~array_size:"N" ~ops:(Local_obj ("clean", "transfer")) [cFor "j"];
  !! Trace.alternative (fun () ->
    !! Variable.local_name  "a" ~into:"x" ~mark:"section_of_interest" [cFor "i"];
    !! Variable_basic.delocalize  ~array_size:"N" ~ops:(Local_obj ("clean", "transfer")) [cMark "section_of_interest"] ;
    !! ());
)
