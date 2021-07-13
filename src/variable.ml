open Ast
open Target

(* [fold ~as_reference ~fold_at tg] *)
let fold ?(as_reference : bool = false) ?(fold_at : target list = [[]]) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.fold as_reference fold_at i t p)


(* [insert ~const ~as_reference x dx tg] *)
let insert ?(const : bool = false) ?(as_reference : bool = false) (x : var) (dx : var) (tg : Target.target) : unit =
  Trace.apply (fun ctx t ->
   let ps = resolve_target_between tg t in
   List.fold_left (fun t (p,i) -> Variable_core.insert ctx const as_reference x dx i t p) t ps
  )

(* [insert ~const ~as_reference x dx tg] *)
(* let insert ?(const : bool = false) ?(as_reference : bool = false) (x : var) (dx : var) : Target.Transfo.t =
  Target.apply_on_target_between
    (fun t (p,i) -> Variable_core.insert const as_reference x dx i t p) tg *)

(* [remove tg] *)
let remove : Transfo.t =
  Generic.remove_instruction

(* [insert_and_fold ~const ~as_reference ~fold_at x dx tg] *)
let insert_and_fold ?(const : bool = false) ?(as_reference : bool = false) ?(fold_at : target list = [[]]) (x : var) (dx : string) (tg : Target.target) : unit =
  Trace.apply (fun ctx t ->
   let ps = resolve_target_between tg t in
   List.fold_left (fun t (p, i) -> Variable_core.insert_and_fold ctx const as_reference x dx i fold_at t p) t ps
  )
  

(* [insert_and_fold ~const ~as_reference ~fold_at x dx tg] *)
(* let insert_and_fold ?(const : bool = false) ?(as_reference : bool = false) ?(fold_at : target list = [[]]) (x : var) (dx : trm) : Target.Transfo.t =
  (* TODO: apply_on_target_between *)
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.insert_and_fold const as_reference x dx i fold_at t p) tg *)

(* [inline ~delete_decl ~inline_at tg] *)
let inline ?(delete_decl : bool = false) ?(inline_at : target list = [[]]) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.inline delete_decl inline_at i t p)

let rename (new_name : var) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p, i) t -> Variable_core.rename new_name i t p)