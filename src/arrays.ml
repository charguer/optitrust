open Ast
open Target

(* [to_variables new_vars tg] *)
let to_variables (new_vars : var list) (tg : target) : unit = 
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Arrays_core.to_variables new_vars i t p 
  ) tg

(*[tile name block_name b x tg]*)
let tile ?(name : var -> var = fun x -> x ^ "_tiled") (block_name : typvar) (b : var) (x : typvar) (tg : target) : unit =
  Target.apply_on_transformed_targets(Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Arrays_core.tile name block_name b x i t p) tg

(* [swap name x tg] *)
let swap ?(name : var -> var = fun x -> x ^ "_swapped") (x : typvar) (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Arrays_core.swap name x i t p) tg



(* [aos_to_soa name x tg]*)
let aos_to_soa ?(name : var -> var = fun x -> x ^ "_swapped") (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t ->  Arrays_core.aos_to_soa name i t p) tg



