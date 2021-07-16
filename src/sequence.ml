open Ast
open Target

(* [insert tg ts] expects the a relative target [tg] pointing before or after an instruction
  Code to be inserted is entered as a string. This code is added inside the sequence as arbitrary trm 
  ast-node. Which is then transformed into a proper ast after a reparse.    
*)
let insert (s : string) (tg : target): unit =
  Target.apply_on_target_between (fun t (p,i) ->
    Sequence_core.insert i s t p) tg;
  Trace.reparse()

(* [delete index nb tg] expects the target [tg] to point to an instruction.
  It then removes this instruction from the ast.
*)
let delete ?(nb : int = 1) : Target.Transfo.t =
  Target.apply_on_transformed_targets ~rev:true (Generic_core.isolate_last_dir_in_seq)
    (fun (p, i) t -> Sequence_core.delete i nb t p) 

(* Alternative trick to shift the indices on the way:
   Target.applyi_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun id_target (p, i) t -> Sequence_core.delete (i-id_target) nb t p) tg *)

(* [iter_delete tgl]: just iterate over the list of targeted trm to be deleted*)
let iter_delete (tgl : target list) : unit =
 List.fold_left (fun () x ->
    delete x ) () tgl

(* [sub i nb tg] expects the target to point to an instruction inside a sequence.
  A label [label] in case the user want's a labelled sub-sequence and the number of instructions 
  to be moved inside the sub-sequence. If [nb] = 1 means then this transformation is basically the same as
  wrap. If [nb] is greater than one then it means that the instructions which come righ after
  the target instruction will be included in the sub-sequence too.
*)
let sub ?(label : string = "") (nb : int) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
  (fun (p, i) t -> Sequence_core.sub label i nb t p)

(* [sub_between tg_beg tg_end]: this transformation is an advanced version of sub.
   The difference is that instead of giving the number of instructions one want's to put
   inside a sub-sequence, the first and the last trm of the on-coming sub-sequence are given.
   All the intermediate trms are also included inside the sub-sequence.
*)
let sub_between ?(label : string = "") (tg_beg : target) (tg_end : target) : unit =
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
    List.fold_left (fun t (p,i,nb) -> Sequence_core.sub label i nb t p) t pis)


(* [inline tg] expects the target [tg] to point at a sequence that appears
   nested inside another sequence, e.g., points at [{t2;t3}] inside
   [{ t1; { t2; t3 }; t4 }]. It "inlines" the contents of the inner sequence,
   producing e.g., [{ t1; t2; t3; t3}]. *)
let inline : Target.Transfo.t =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Sequence_core.inline i t p)

(* [wrao visible tg] expecets the target [tg] to point at any arbitrary trm,
  it will take this trm and put it inside a sequence*)
let wrap ?(visible : bool = true) ?(label : string = "") : Target.Transfo.t =
  Target.apply_on_target (Sequence_core.wrap visible label )


(* [unwrap tg] expects the target [tg] to point to a sequence with only one trm inside.
 It moves this trm to the outer sequence*)
let unwrap : Target.Transfo.t =
  Target.apply_on_target (Sequence_core.unwrap)
