open Ast
open Target

(* [insert tg ts] expects the relative target [tg] pointing before or after an instruction.
  [s] denotes the trm_to be inserted inside the sequence
*)
let insert (s : string) (tg : target): unit =
  Target.apply_on_target_between (fun t (p,i) ->
    Sequence_core.insert i s t p) tg;
  Trace.reparse()


(* [delete index nb tg] expects the target [tg] to point to an instruction.
     [nb] denotes the number of instructions to delete starting from the targeted trm.
*)
let delete ?(nb : int = 1) : Target.Transfo.t =
  Target.apply_on_transformed_targets ~rev:true (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Sequence_core.delete i nb t p) 


(* [iter_delete tgl]: just iterate over the list of targeted trm to be deleted*)
let iter_delete (tgl : target list) : unit =
 List.fold_left (fun () x ->
    delete x ) () tgl

(* [sub i nb tg] expects the target to point to an instruction inside a sequence.
    [label] denotes a label which the generated sub-sequence is going to have, in case the user decides to have one.
                        }
   [nb] is the number of instructions to be moved inside the sub-sequence. 
      If [nb] = 1 means then this transformation is basically the same as wrap. 
      If [nb] is greater than one then it means that the instructions which come right after
      the target instruction will be included in the sub-sequence too.
   Ex: int main(){     int main(){
        int x = 5;      { int x = 5}
        iny y = 6;      int y = 6;
        return 0;       return 0;
      }                }
*)
let sub ?(label : string = "") (nb : int) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
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
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Sequence_core.inline i t p)

(* [wrap visible tg] expecets the target [tg] to point at any arbitrary trm,
    it will wrap a sequence around the targeted  trm.
    [visible] denotes the visibility of a sequence. This means the that the the sequence is
        used only for internal purposes.
    [label] denotes the label of the sub-sequence. Targeting sequences can be challanging hence having 
          them laballed before can make the apllication of the transformations easier.
*)
let wrap ?(visible : bool = true) ?(label : string = "") : Target.Transfo.t =
  Target.apply_on_target (Sequence_core.wrap visible label )


(* [unwrap tg] expects the target [tg] to point to a sequence with only one trm inside.
 It moves this trm to the outer sequence*)
let unwrap : Target.Transfo.t =
  Target.apply_on_target (Sequence_core.unwrap)
