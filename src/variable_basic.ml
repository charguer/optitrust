open Target

(* [fold ~as_reference ~at tg] expects [tg] to point to a variable declaration 
    [as_reference] - denotes a flag whether the declaration initialization contains a 
      variable reference or not.
    [at] - denotes a target where the folding is done. If empty the 
      folding operation is performed on all the ast nodes in the same level as the 
      declaration or deeper, by default [at] = []
*)
let fold ?(as_reference : bool = false) ?(at : target = []) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.fold as_reference at i t p) 

(* [inline ~delete ~at tg] expects [tg] to point to a variable declaration 
    it then find all the occurrences of the variable and replaces them with it's assigned value.
   [delete] ~ denotes a falg whether the declaration should be kept or not
   [at] - denotes a target where inlining is done. If empty the
    inlining operation is performed on all the ast nodes in the same level as the declaration 
    or deeper, by default [at] = []
*)
let inline ?(delete : bool = false) ?(at : target = []) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.inline delete at i t p)

(* [reanme ~list ~func tg] expects [tg] to point to a sequence.
    [list] - denotes a list of pairs where each pair has the 
      current variable and the one which is going to replace it.
      By default this list is empty.
    [func] - a function which is going to replace all the variables 
      inside the targeted sequence. By default this function is the one
      which adds "_1" to each declared variable inside the sequence.
*)
let rename ?(list : (string * string) list = []) ?(func : string -> string = fun x -> x ^ "_1") : Target.Transfo.t =
  Target.apply_on_target (Variable_core.rename list func) 
  
(* [init_detach tg] expects the target to point to a variable initialization.
   It then splits the instruction into a variable declaration and a set operation.
*)
let init_detach : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Variable_core.init_detach i t p)

(* [init_attach const tg] expects the target to point to a variable declaration, 
    Then it will search inside the sequence which contains the variable declaration. 
    For an unique assigment. The it will replace that assignment with a new initialized
    variable declaration.
    [const] -denotes a booleean to decide if the new declaration is constant or not.
*)
let init_attach ?(const : bool = false) : Target.Transfo.t = 
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.init_attach const i t p )
(* [const_non_const tg] expects the target to point to an initialized variable declaration.
    It then checks the variable mutability. It it is mutable then make it un-mutable by adding 
    a const in front. Otherwise make it mutable by removing the const.
*)
let const_non_const : Target.Transfo.t =
  Target.apply_on_target (Variable_core.const_non_const)
