(* open Ast *)
open Target


(* [fold ~fold_at tg] *)
let fold ?(fold_at : target list = [[]]) (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Typedef_core.fold fold_at i t p) tg

(* [inert x dx tg] *)
let insert (s : string) (tg : target) : unit =
  Trace.apply( fun ctx t ->
    let ps = resolve_target_between tg t in
    List.fold_left (fun t (p, i) -> Typedef_core.insert ctx s i t p) t ps
  ) 
  
(* [remove tg] *)
let remove : Transfo.t =
  Generic.remove_instruction

(* [insert_and_fold ~fold_at x dx tg] *)
let insert_and_fold ?(fold_at : target list = [[]]) (td : string) (tg : target) : unit =
  Trace.apply (fun ctx t ->
    let ps = resolve_target_between tg t in
    List.fold_left (fun t (p, i) -> Typedef_core.insert_and_fold ctx td i fold_at t p) t ps
  )
  

(* [insert_and_fold ~fold_at x dx tg] *)
(* let insert_and_fold ?(fold_at : target list = [[]]) (x : var) (dx : typ) (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Typedef_core.insert_and_fold x dx i fold_at t p) tg *)

(* [inline ~delete_decl ~inline_at tg]*)
let inline ?(delete_decl : bool = false) ?(inline_at : target list = [[]]) (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t ->
      Typedef_core.inline delete_decl inline_at i t p) tg




