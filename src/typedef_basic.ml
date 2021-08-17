open Ast
open Target

(* [fold ~at tg] expects [tg] to point to a typedef declaration.
    [at] - denotes a target where folding is done. If empty
      the folding operation is performed on all the ast nodes in the same 
      level as the declaration or deeper
*)
let fold ?(at : target = []) (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Typedef_core.fold at i t p) tg
  
(* [inline ~delete ~at tg] expects [tg] to point to a typedef declaration
    [delete] - denotes a flag for telling if the declaration should be kept or no
    [at] - denotes a target where inlining is done, if empty 
      the inlining operation is performed on all the ast nodes in the same level as 
      the declaration or deeper, by default [at] = []
*)
let inline ?(delete : bool = false) ?(at : target = []) (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t ->
      Typedef_core.inline delete at i t p) tg


(* [alias name tg] expects [tg] to point to a typedef declaration in then copies the content 
      of the body of typedef at gives to it the name [name]
*)
let alias (name : string) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Typedef_core.alias name i t p)

(* [insert name td_body] expects target [tg] to a relative location inside a sequence
    then it will insert a typedef declaration at that location. [name] is the new type
    name while [td_body] is the kind of typedef we are going to declare. It can be an alias
    a product(for struct declarations), a sum type or an enum.
*)
let insert (name : string) (td_body : typdef_body) : Target.Transfo.t =
  Target.apply_on_target_between(fun t (p, i) ->
    Typedef_core.insert name td_body i t p)