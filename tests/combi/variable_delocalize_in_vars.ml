open Optitrust
open Target
open Ast

let _ =  Run.script_cpp ( fun () ->
  !! Variable.delocalize_in_vars ~index:"k" ~mark:"section_of_interest" ~var:"a" ~local_var:"x" ~var_type:(typ_str "T") ~array_size:"N" ~ops:(Delocalize_arith (Lit_int 0, Binop_add) ) ~local_vars:["xa";"xb"] [cFor "i"];
  !! Trace.alternative (fun () ->
    !! Variable.local_name  ~mark:"section_of_interest" ~var_type:(typ_str "T") ~var:"a" ~local_var:"x" [cFor "i"];
    !! Variable_basic.delocalize  ~index:"k" ~array_size:"N" ~ops:(Delocalize_arith (Lit_int 0, Binop_add)) [cMark "section_of_interest"] ;
    !! Variable_basic.inline_at [cFor "k"] [ nbAny; cVarDef "N"];
    !! Loop_basic.unroll ~braces:false [nbMulti; cFor "k"];
    !! Arrays_basic.to_variables ["xa";"xb"] [cVarDef "x"];
    !! Marks_basic.remove "section_of_interest"  [cMark "section_of_interest"];
    !! ());
)
