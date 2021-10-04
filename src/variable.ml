open Ast
include Variable_core.Rename
include Variable_basic

(* [fold ~as_reference ~at ~nonconst tg] expects [tg] to point to a variable declaration
    [as_reference] - denotes a flag whether the declaration initialization contains a
      variable reference or not.
    [at] - denotes a list of targets where the folding is done. If empty the
      folding operation is performed on all the ast nodes in the same level as the
      declaration or deeper, by default [at] = [].
    [nonconst] - denotes a flag to decide if folding should be done for variable which are
        not mutable, in general is not safe to fold variables which are not declared as const.
        But if the user knows what he/she is doing than it can use this flag to use folding
        also for mutable variables.
    This transformation
*)
let fold ?(as_reference : bool = false) ?(at : Target.target = []) ?(nonconst : bool = false) (tg : Target.target) : unit =
  Target.iter_on_targets (fun t p ->
    let (tg_trm, _) = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_let (vk, (_, tx), _) ->
      let ty = get_inner_ptr_type tx in
      begin match ty.typ_desc with 
      (* If the declared variable has a refernce type checking its mutability is not needed*)
      | Typ_ptr {ptr_kind = Ptr_kind_ref;_} ->
        Variable_basic.fold ~as_reference ~at (Target.target_of_path p)
      (* In other cases we need to check the mutability of the variable *)
      | _ -> begin match vk with
            | Var_immutable -> Variable_basic.fold ~as_reference ~at (Target.target_of_path p)
            | _ -> if nonconst = true
                then Variable_basic.fold ~as_reference ~at (Target.target_of_path p)
                else
                  fail tg_trm.loc "fold: if you want to use folding for mutable variables you should set
                            ~nonconst to true when calling this transformation"
            end
      end
      
    | _ -> fail tg_trm.loc "fold: expected a variable declaration"
) tg



(* [local_other_name var_type old_name new_name] similar to the basic version of local_other_name but with the intermediate
      done autmatically
*)
let local_other_name ?(label : var = "_SECTION_MARK") (var_type : typ) (old_name : var) (new_name : var) : unit =
  Sequence_basic.intro_on_instr ~label:"section_of_interest" ~visible:false [Target.tIndex 0; Target.cFor ~body:[Target.cVar old_name]""];  
  Variable_basic.local_other_name var_type old_name new_name [Target.cLabel label]

(* [insert_and_fold] expects [tg] to point to relative location, then it inserts a new variable declaration at that location. 
    The new declared variable is [name] with typ [typ] and value [value]. This variable will be folded everywhere on the ast nodes
    which come after the declared variable.
*)
let insert_and_fold (name : string) (typ : string) (value : string) (tg : Target.target) : unit = 
  Variable_basic.insert name typ value tg;
  Variable_basic.fold [Target.cVarDef name]



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
    local_other_name ~label var_type old_var new_var ;
    Variable_basic.delocalize arr_size dl_ops [Target.cLabel label; Target.dBody]
    
