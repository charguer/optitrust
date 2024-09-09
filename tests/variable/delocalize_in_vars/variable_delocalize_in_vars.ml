open Optitrust
open Prelude

let _ =  Run.script_cpp ( fun () ->
  let fv n = find_var n [] in
  !! Variable.delocalize_in_vars "a" ~into:"x" ~index:"k" ~mark:"section_of_interest" ~array_size:(fv "N") ~ops:(Local_arith (Lit_int 0, Binop_add) ) ~local_vars:["xa";"xb"] [cFor "i"];

  !! Trace.restore_original();
  !! Variable.local_name ~var:"a" ~local_var:"x" ~mark:"section_of_interest" [cFor "i"];
  !! Variable_basic.delocalize  ~index:"k" ~array_size:(var "N") ~ops:(Local_arith (Lit_int 0, Binop_add)) [cMark "section_of_interest"] ;
  !! Variable_basic.unfold ~at:[cFor "k"] [ nbAny; cVarDef "N"];
  !! Loop_basic.unroll ~inner_braces:false [nbMulti; cFor "k"];
  !! Arrays_basic.to_variables ["xa";"xb"] [cVarDef "x"];
  !! Marks_basic.remove "section_of_interest"  [cMark "section_of_interest"];

)
