(* open Ast *)
open Target

(* [fold ~fold_at tg] expects [tg] to point to a typedef declaration.
    [fold_at] - denotes a list of targets where folding is done. If empty
      the folding operation is performed on all the ast nodes in the same 
      level as the declaration or deeper
*)
let fold ?(fold_at : target list = [[]]) (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Typedef_core.fold fold_at i t p) tg
  
(* [inline ~delete_decl ~inline_at tg] expects [tg] to point to a typedef declaration
    [delete_decl] - denotes a flag for telling if the declaration should be kept or no
    [inline_at] - denotes a list of targets where inlining is done, if empty 
      the inlining operation is performed on all the ast nodes in the same level as 
      the declaration or deeper, by default [inline_at] = []
*)
let inline ?(delete_decl : bool = false) ?(inline_at : target list = [[]]) (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t ->
      Typedef_core.inline delete_decl inline_at i t p) tg


(* [alias name tg] expects [tg] to point to a typedef declaration in then copies the content 
      of the body of typedef at gives to it the name [name]
*)
let alias (name : string) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Typedef_core.alias name i t p)

