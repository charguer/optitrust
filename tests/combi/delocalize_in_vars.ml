open Optitrust
open Target
open Ast

let _ =  Run.script_cpp ( fun () ->
  !! Variable.delocalize_in_vars ~loop_index:"k" ~mark:"section_of_interest" ~old_var:"a" ~new_var:"x" ~var_type:(typ_constr "T") ~array_size:"N" ~dl_ops:(Delocalize_arith (Lit_int 0, Binop_add) ) ~local_vars:["xa";"xb"] ();
  !! Trace.alternative (fun () ->
    !! Variable.local_other_name  ~var_type:(typ_constr "T") ~old_var:"a" ~new_var:"x" ();
    !! Variable_basic.delocalize  ~loop_index:"k" ~array_size:"N" ~dl_ops:(Delocalize_arith (Lit_int 0, Binop_add)) [cMark "section_of_interest"] ;
    !! Variable_basic.inline_at [cFor "k"] [ nbAny; cVarDef "N"];
    !! Loop_basic.unroll ~braces:false [nbMulti; cFor "k"];
    !! Arrays_basic.to_variables ["xa";"xb"] [cVarDef "x"];
    !! Marks_basic.remove "section_of_interest"  [cMark "section_of_interest"];
    !! ());
)
