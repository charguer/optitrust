open Ast

(* TODO: Add the docs for this transformation *)
let delocalize ?(var_type : typvar = "T") ?(old_var : var = "a") ?(new_var : var = "x")  ?(label : var = "section_of_interest") 
  ?(arr_size : string = "N") ?(neutral_element : int = 0) (fold_operation : string ) 
  (tg : Target.target) : unit =
    Sequence_basic.intro_on_instr ~label ~visible:false tg;
    Generic_basic.local_other_name var_type old_var new_var [Target.cLabel label; Target.dBody];
    Generic_basic.delocalize arr_size neutral_element fold_operation [Target.cLabel label; Target.dBody]