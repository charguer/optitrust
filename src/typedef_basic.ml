open Ast
open Target

(* [fold ~at tg] expects [tg] to point at a typedef declaration.
    [at] - denotes a target where fold_lefting is done. If empty
      the fold_lefting operation is performed on all the ast nodes in the same 
      level as the declaration or deeper
*)
let fold ?(at : target = []) (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Typedef_core.fold at i t p) tg
  
(* [unfold ~delete ~at tg] expects [tg] to point at a typedef declaration
    [delete] - denotes a flag for telling if the declaration should be kept or no
    [at] - denotes a target where inlining is done, 
    if empty the inlining operation is performed on all the ast nodes in the 
    same level as the declaration or deeper, by default [at] = []
*)
let unfold ?(delete : bool = false) ?(at : target = []) (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) ->
      Typedef_core.unfold delete at i t p) tg


(* [insert_copy name tg] expects [tg] to point at a typedef declaration it then copies the content 
      of the body of typedef at gives to it the name [name]
*)
let insert_copy (name : string) (tg : Target.target) : unit =
  Internal.nobrace_remove_after( fun _ -> Target.apply_on_targets (Typedef_core.insert_copy name) tg)

(* [insert name td_body] expects target [tg] to a relative location inside a sequence
    then it will insert a typedef declaration at that location. [name] is the new type
    name while [td_body] is the kind of typedef we are going to declare. It can be an alias
    a product(for struct declarations), a sum type or an enum.
*)
let insert (name : string) (td_body : typdef_body) : Target.Transfo.t =
  Target.apply_on_targets_between(fun t (p, i) ->
    Typedef_core.insert name td_body i t p)