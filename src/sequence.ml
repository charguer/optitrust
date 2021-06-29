open Ast
open Target

(* [insert tg ts] *)
(* TODO: swap args *)
let insert (tg : target) (s : string) : unit =
  Target.apply_on_target_between (fun t (p,i) ->
    Sequence_core.insert i s p t) tg;
  Trace.reparse()

(* [delete index nb tg] *)
let delete ?(nb : int = 1) (tg : target) : unit =
  Target.apply_on_transformed_targets ~rev:true (Generic_core.isolate_last_dir_in_seq)
    (fun (p, i) t -> Sequence_core.delete i nb t p) tg
(* Alternative trick to shift the indices on the way:
   Target.applyi_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun id_target (p, i) t -> Sequence_core.delete (i-id_target) nb t p) tg *)

(* [iter_delete tgl] *)
let iter_delete (tgl : target list) : unit =
 List.fold_left (fun () x ->
    delete x ) () tgl

(* [sub i nb tg] *)
let sub (nb : int) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
  (fun (p, i) t -> Sequence_core.sub i nb t p)

(* [sub_between tg_beg tg_end] *)
let sub_between (tg_beg : target) (tg_end : target) : unit =
  Trace.apply (fun _ t ->
    let ps_beg : (path * int) list = resolve_target_between tg_beg t in
    let ps_end : (path * int) list = resolve_target_between tg_end t in
    if List.length ps_beg <> List.length ps_end
      then fail t.loc "sub_between: not the same number of targets";
    let pis : (path * int * int) list = List.map2 (fun (p1,i1) (p2,i2) ->
      if p1 <> p2
        then fail t.loc "sub_between: targets for begin and end don't match the same sequences";
      if i2 <= i1
        then fail t.loc "sub_between: target for end should be past the target for start";
      (p1, i1, i2 - i1)) ps_beg ps_end in
    List.fold_left (fun t (p,i,nb) -> Sequence_core.sub i nb t p) t pis)


(* [inline tg] expects the target [tg] to point at a sequence that appears
   nested inside another sequence, e.g., points at [{t2;t3}] inside
   [{ t1; { t2; t3 }; t4 }]. It "inlines" the contents of the inner sequence,
   producing e.g., [{ t1; t2; t3; t3}]. *)
let inline : Target.Transfo.t =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Sequence_core.inline i t p)

(* [wrao visible tg] *)
let wrap ?(visible : bool = true) : Target.Transfo.t =
  Target.apply_on_target (Sequence_core.wrap visible)

(* [unwrap tg] *)
let unwrap : Target.Transfo.t =
  Target.apply_on_target (Sequence_core.unwrap)
