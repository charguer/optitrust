open Ast
open Target

(* [fold ~as_reference ~fold_at tg] expects [tg] to point to a variable declaration 
    [as_reference] - denotes a flag whether the declaration initialization contains a 
      variable reference or not.
    [fold_at] - denotes a list of targets where the folding is done. If empty the 
      folding operation is performed on all the ast nodes in the same level as the 
      declaration or deeper, by default [fold_at] = []
*)
let fold ?(as_reference : bool = false) ?(fold_at : target list = [[]]) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.fold as_reference fold_at i t p) 

(* [inline ~delete_decl ~inline_at tg] eexpects [tg] to point to a variable declaration 
    it then find all the occurrences of the variable and replaces them with it's assigned value.
   [delete_decl] ~ denotes a falg whether the declaration should be kept or not
   [inline_at] - denotes a list of targets where inlining is done. If empty the
    inlining operation is performed on all the ast nodes in the same level as the declaration 
    or deeper, by default [inline_at] = []
*)
let inline ?(delete_decl : bool = false) ?(inline_at : target list = [[]]) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.inline delete_decl inline_at i t p)

(* [reanme new_name tg] expects [tg] to point to a variable declaration
     then it will change the name inside theat declaration and all occurrences
     of the same variable ar going to be change too.
    [new_name] denotes the new name for the targeted declaration
*)
(* Rename a variablea and all its occurrences *)
let rename (new_name : var) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Variable_core.rename new_name i t p)


