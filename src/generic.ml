open Ast
include Generic_basic

(* [delocalize ~var_type ~old_var ~new_var ~label ~arr_size ~neutral_element fold_operation tg] 
    expects the target [tg] to point to a for loop. Then it will surround this loop with a @nobrace
    sequence. After that it will apply another transformation called local other name. Which as the name
    suggests it will declare a new variable inside the targeted block and replace the current one with t he new one.
    Finally a last instruction is added to save all the changes to the old variable. Now the stage is 
    ready to apply delocalize which basically declares a new array oof size [array_size]. Then it will
    transform the old loop into a parallel loop. And finally a reduction is done to save the result into
    the old variable.
*)

let delocalize ?(old_var : var = "") ?(new_var : var = "") ?(label : var = "section_of_interest")
  (var_type : typ) (arr_size : string) (dl_ops : delocalize_ops): unit = 
    Variable.local_other_name ~label var_type old_var new_var ;
    Generic_basic.delocalize arr_size dl_ops [Target.cLabel label; Target.dBody]
    
