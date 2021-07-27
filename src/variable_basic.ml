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
  

