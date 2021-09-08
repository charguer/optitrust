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
  let t = Trace.get_ast() in
  let exp =  Constr.resolve_target_exactly_one tg t in
  let (tg_trm, _) = Path.resolve_path exp t in
  match tg_trm.desc with 
  | Trm_let (vk, _, _) ->
    begin match vk with 
    | Var_immutable -> Variable_basic.fold ~as_reference ~at tg
    | _ -> if nonconst = true 
            then Variable_basic.fold ~as_reference ~at tg
            else
              fail tg_trm.loc "fold: if you want to use folding for mutable variables you should set
                          ~nonconst to true when calling this transformation"
    end
  | _ -> fail tg_trm.loc "fold: expected a variable declaration"


(* [local_other_name var_type old_name new_name] similar to the basic version of local_other_name but with the intermediate 
      done autmatically
*)
let local_other_name (var_type : typ) (old_name : var) (new_name : var) : unit =
  Sequence_basic.intro_on_instr ~label:"section_of_interest" ~visible:false [Target.tIndex 0; Target.cFor ~body:[Target.cVar old_name]""];
  Variable_basic.local_other_name var_type old_name new_name [Target.cLabel "section_of_interest";Target.dBody];  