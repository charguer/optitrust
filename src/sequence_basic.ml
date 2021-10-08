open Ast
open Target

(* [insert tg ts] expects the relative target [tg] pointing before or after an instruction.
  [s] denotes the trm_to be inserted inside the sequence
*)
let insert (s : string) (tg : target): unit =
  Target.apply_on_targets_between (fun t (p,i) ->
    Sequence_core.insert i s t p) tg;
  Trace.reparse()


(* [delete index nb tg] expects the target [tg] to point to an instruction.
     [nb] denotes the number of instructions to delete starting from the targeted trm.
*)
let delete ?(nb : int = 1) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Sequence_core.delete i nb t p)


(* [iter_delete tgl]: just iterate over the list of targeted trm to be deleted*)
let iter_delete (tgl : target list) : unit =
 List.fold_left (fun () x ->
    delete x ) () tgl

(* [intro i nb tg] expects the target to point to an instruction inside a sequence.
    [mark] denotes a mark which the generated sub-sequence is going to have, in case the user decides to have one.
   [visible] denotes the visibility of a sequence. This means the that the the sequence is
        used only for internal purposes.                     }
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
let intro ?(mark : string = "") (nb : int) (tg : Target.target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
  (fun (p, i) t -> Sequence_core.intro mark i nb t p) tg
(* [intro_between tg_beg tg_end]: this transformation is an advanced version of intro.
   The difference is that instead of giving the number of instructions one want's to put
   inside a sub-sequence, the first and the last trm of the on-coming sub-sequence are given.
   All the intermediate trms are also included inside the sub-sequence.
*)
let intro_between ?(mark : string = "") (tg_beg : target) (tg_end : target) : unit =
  Internal.nobrace_remove_after ( fun  _ ->
  Trace.apply (fun t ->
    let ps_beg : (path * int) list = resolve_target_between tg_beg t in
    let ps_end : (path * int) list = resolve_target_between tg_end t in
    if List.length ps_beg <> List.length ps_end
      then fail t.loc "intro_between: not the same number of targets";
    let pis : (path * int * int) list = List.map2 (fun (p1,i1) (p2,i2) ->
      if p1 <> p2
        then fail t.loc "intro_between: targets for begin and end don't match the same sequences";
      if i2 <= i1
        then fail t.loc "intro_between: target for end should be past the target for start";
      (p1, i1, i2 - i1)) ps_beg ps_end in
    List.fold_left (fun t (p,i,nb) -> Sequence_core.intro mark i nb t p) t pis))


(* [elim tg] expects the target [tg] to point at a sequence that appears
   nested inside another sequence, e.g., points at [{t2;t3}] inside
   [{ t1; { t2; t3 }; t4 }]. It "elims" the contents of the inner sequence,
   producing e.g., [{ t1; t2; t3; t3}]. *)
let elim (tg : Target.target) : unit =
  Internal.nobrace_remove_after ( fun _ ->
  Target.apply_on_targets (Sequence_core.elim) tg)

(* [intro_on_instr visible tg] expecets the target [tg] to point at any arbitrary trm,
    it will wrap a sequence around the targeted  trm.
    [visible] denotes the visibility of a sequence. This means the that the the sequence is
        used only for internal purposes.
    [mark] denotes the mark of the sub-sequence. Targeting sequences can be challanging hence having
          them laballed before can make the apllication of the transformations easier.
*)
let intro_on_instr ?(mark : mark = "") ?(visible : bool = true) : Target.Transfo.t =
   if not visible then Internal.nobrace_enter();
   Target.apply_on_targets (Sequence_core.intro_on_instr visible mark)
  

(* [unwrap tg] expects the target [tg] to point to a instruction surrounded by a sequence..
 It moves this trm to the outer sequence*)
let elim_around_instr (tg : Target.target) : unit =
   Internal.nobrace_remove_after ( fun _ ->
    Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, _) t -> Sequence_core.elim t p) tg
   )
   

(* [split tg] expects target [tg] to point around another target in a sequence meaning, before or after another target
    It will split the sequence which contains that target into two parts, depending on the fact that the entered target
    is of type before or after the first part will include (exclude) the target.
*)
let split (tg : Target.target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply_on_targets_between (fun t (p, i) -> Sequence_core.split i t p) tg)

(* [partition ~braces blocks tg] expects the target tg to point to a sequence, this transformations will split that sequence
      into blocks where the sizes of the blocks are given by [blocks].
        [blocks] denotes the sizes for each block inside the sequence. By default it should be empty, otherwise the sum of
          integers inside [blocks] should sum up to the number of instructions of the targeted sequence.
        [braces] denotes a flag for the visibility of the blocks meaning that this block partition will be meaningful only for
          other transformations which call explicitly the partition transformation.
*)
let partition ?(braces : bool = false) (blocks : int list) : Target.Transfo.t =
  if not braces then Internal.nobrace_enter();
  Target.apply_on_targets (Sequence_core.partition blocks braces)

(* [shuffle tg] expects the target [tg] to point to a sequence of blocks, this transformation will transpose the block structure
    think about a sequence of blocks as a matrix.
*)
let shuffle ?(braces : bool = false) : Target.Transfo.t =
  if not braces then Internal.nobrace_enter ();
  Target.apply_on_targets (Sequence_core.shuffle braces)
