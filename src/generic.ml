(* open Ast *)

(* [delocalize ~var_type ~old_var ~new_var ~label ~arr_size ~neutral_element fold_operation tg] 
    expects the target [tg] to point to a for loop. Then it will surround this loop with a @nobrace
    sequence. After that it will apply another transformation called local other name. Which as the name
    suggests it will declare a new variable inside the targeted block and replace the current one with t he new one.
    Finally a last instruction is added to save all the changes to the old variable. Now the stage is 
    ready to apply delocalize which basically declares a new array oof size [array_size]. Then it will
    transform the old loop into a parallel loop. And finally a reduction is done to save the result into
    the old variable.
*)
(* let delocalize ?(var_type : typvar = "T") ?(old_var : var = "a") ?(new_var : var = "x")  ?(label : var = "section_of_interest") 
  ?(arr_size : string = "N") ?(neutral_element : int = 0) (fold_operation : string ) 
  (tg : Target.target) : unit =
    Sequence_basic.intro_on_instr ~label ~visible:false tg;
    Variable_basic.local_other_name var_type old_var new_var [Target.cLabel label; Target.dBody];
    Generic_basic.delocalize arr_size neutral_element fold_operation [Target.cLabel label; Target.dBody] *)