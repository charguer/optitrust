open Optitrust
open Target
open Ast

let _ =  Run.script_cpp ( fun () ->
  !! Variable.delocalize_in_vars "a" ~into:"x" ~index:"k" ~mark:"section_of_interest" ~array_size:"N" ~ops:(Delocalize_arith (Lit_int 0, Binop_add) ) ~local_vars:["xa";"xb"] [cFor "i"];
  !! Trace.alternative (fun () ->
    !! Variable.local_name  "a" ~into:"x" ~mark:"section_of_interest" [cFor "i"];
    !! Variable_basic.delocalize  ~index:"k" ~array_size:"N" ~ops:(Delocalize_arith (Lit_int 0, Binop_add)) [cMark "section_of_interest"] ;
    !! Variable_basic.inline_at [cFor "k"] [ nbAny; cVarDef "N"];
    !! Loop_basic.unroll ~braces:false [nbMulti; cFor "k"];
    !! Arrays_basic.to_variables ["xa";"xb"] [cVarDef "x"];
    !! Marks_basic.remove "section_of_interest"  [cMark "section_of_interest"];
    !! ());
)
