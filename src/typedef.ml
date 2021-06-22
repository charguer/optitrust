open Ast
open Target


(* [fold ~fold_at tg] *)
let fold ?(fold_at : target list = [[]]) (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Typedef_core.fold fold_at i t p) tg

(* [inert x dx tg] *)
let insert (x : typvar) (dx : typ) (tg : target) : unit =
  Target.apply_on_target_between
    (fun t (p,i) -> Typedef_core.insert x dx i t p) tg

(* [remove tg] *)
let remove : Transfo.t =
  Generic.remove_instruction

(* [insert_and_fold ~fold_at x dx tg] *)
let insert_and_fold ?(fold_at : target list = [[]]) (x : var) (dx : typ) (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Typedef_core.insert_and_fold x dx i fold_at t p) tg

(* [inline ~delete_decl ~inline_at tg]
  TODO: document the fact that inline_at contains target that are relative to the sequence containing the typedef *)
let inline ?(delete_decl : bool = false) ?(inline_at : target list = []) (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t ->
      (* TODO: here do the resolution of the inline_at  using List.map *)
      Typedef_core.inline delete_decl inline_at i t p) tg




