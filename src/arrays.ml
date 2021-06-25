open Ast
open Target

(* [to_variables new_vars tg] *)
let to_variables (new_vars : var list) (tg : target) : unit = 
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Arrays_core.to_variables new_vars i t p 
  ) tg

(*[tile name block_name b x tg]*)
let tile (block_name : typvar) (b : var) (tg : target) : unit =
  Target.apply_on_transformed_targets(Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Arrays_core.tile block_name b i t p) tg

(* [swap name x tg] *)
let swap (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Arrays_core.swap i t p) tg


(* [aos_to_soa name x tg]*)
let aos_to_soa (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t ->  Arrays_core.aos_to_soa i t p) tg



