open Ast
open Target

(* [fold ~as_reference ~fold_at tg] *)
let fold ?(as_reference : bool = false) ?(fold_at : target list = [[]]) (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Declaration_core.fold as_reference fold_at i t p) tg

(* [insert ~const ~as_reference x dx tg] *)
let insert ?(const : bool = false) ?(as_reference : bool = false)  (x : var) (dx : trm) (tg : target) : unit =
  Target.apply_on_target_between 
    (fun (p,i) t -> Declaration_core.insert const as_reference x dx i t p) tg

(* [insert_const x dx tg] *)
let insert_const (x : var) (dx : trm) (tg : target) : unit =
  insert ~const:true x dx tg

(* [inert_typedef x dx tg] *)
let insert_typedef (x : typvar) (dx : typ) (tg : target) : unit =
  Target.apply_on_target_between 
    (fun (p,i) t -> Declaration_core.insert_typedef x dx i t p) tg

(* [remove tg] *)
let remove : Transfo.t =
  Target.apply_on_target(Declaration_core.remove)


(* [insert_and_fold ~const ~as_reference ~fold_at x dx tg] *)
let insert_and_fold ?(const : bool = false) ?(as_reference : bool = false) ?(fold_at : target list = [[]]) (x : var) (dx : trm) (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Declaration_core.insert_and_fold const as_reference x dx i fold_at t p) tg

(* [insert_and_fold_typedef ~fold_at x dx tg] *)
let insert_and_fold_typedef ?(fold_at : target list = [[]]) (x : var) (dx : typ) (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Declaration_core.insert_and_fold_typedef x dx i fold_at t p) tg

(* [inline ~delete_decl ~inline_at tg] *)
let inline ?(delete_decl : bool = false) ?(inline_at : target list = []) (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Declaration_core.inline delete_decl inline_at i t p) tg

(* [inline_typedef ~delete_decl ~inline_at tg] *)
let inline_typedef ?(delete_decl : bool = false) ?(inline_at : target list = []) (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Declaration_core.inline_typedef delete_decl inline_at i t p) tg


