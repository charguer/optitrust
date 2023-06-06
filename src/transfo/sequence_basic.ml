open Ast
open Target


(* ==========TODO: replace the old one*)


(* [new_insert_on code t index]: inserts trm [code] at index [index] in sequence [t],
    [code] - instruction to be added as an arbitrary trm,
    [t] - ast of the outer sequence where the insertion will be performed,
    [index] - a valid index where the instruction can be added *)
let new_insert_on (code : trm) (t : trm) (index : int) : trm =
  let error = "Sequence_core.insert_aux: expected the sequence on where insertion is performed." in
  let tl = trm_inv ~error trm_seq_inv t in
  let new_tl = Mlist.insert_at index code tl in
  trm_seq ~annot:t.annot new_tl

(* [insert ~reparse code tg]: expects the target [tg] to point at a relative position(in between two instructoins),
     [code] - the instruction that is going to be added, provided by the user as an arbitrary trm. *)
let%transfo new_insert ?(reparse : bool = false) (code : trm) (tg : target) : unit =
  Target.reparse_after ~reparse (
    Target.apply_at_target_paths_before (new_insert_on code)) tg


  (*==========*)



(* [insert ~reparse code tg]: expects the target [tg] to point at a relative position(in between two instructoins),
     [code] - the instruction that is going to be added, provided by the user as an arbitrary trm. *)
let%transfo insert ?(reparse : bool = false) (code : trm) (tg : target) : unit =
  Target.reparse_after ~reparse (Target.apply_on_targets_between (fun t (p,i) ->
    Sequence_core.insert i code t p) ) tg


(* [delete index nb tg]: expects the target [tg] to point at an instruction,
     [nb] - denotes the number of instructions to delete starting from the targeted trm.

   @correctness: correct if nothing modified by the instruction was observed
   later.
   If the next instructions need an invariant H' and { H } del_instr { H'' }
   we need both H ==> H' and H'' ==> H'. *)
let%transfo delete ?(nb : int = 1) (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p, i) -> Sequence_core.delete i nb t p) tg


(* [iter_delete tgl]: just iterates over the list of targeted trms to be deleted. *)
let iter_delete (tgl : target list) : unit =
 List.fold_left (fun () x ->
    delete x ) () tgl

(* [intro i nb tg]: expects the target [tg] to point at an instruction inside a sequence.
    [mark] -  denotes a mark which add into the generated sub-sequence, in case the user decides to have one.
    [visible] - denotes the visibility of a sequence. This means the that the the sequence is
               used only for internal purposes.                     }
    [nb] - is the number of instructions to be moved inside the sub-sequence.
          If [nb] = 1 means then this transformation is basically the same as intro_on_instr.
          If [nb] is greater than one then it means that the instructions which come right after
            the targeted instruction will be included in the sub-sequence too.
          If [nb] is lwoer than one then it means that the instructions which come before
            the targeted instruction will be included in the sub-sequence too.
    Ex: int main(){     int main(){
        int x = 5;      { int x = 5}
        iny y = 6;      int y = 6;
        return 0;       return 0;
      }                } *)
let%transfo intro ?(mark : string = "") ?(label : label = "") (nb : int) (tg : target) : unit =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
  (fun t (p, i) -> Sequence_core.intro mark label i nb t p) tg


(* [intro_after ~mark ~label tg]: same as [intro] but this transformation will include in the sequence all the
    instructions that come after the targeted instruction and belong to the same scope. *)
let%transfo intro_after ?(mark : mark = "") ?(label : label = "") (tg : target) : unit =
  Target.apply_on_targets (fun t p ->
    let path_to_seq, index = Internal.isolate_last_dir_in_seq p in
    let seq_trm = Path.resolve_path path_to_seq t in
    match seq_trm.desc with
    | Trm_seq tl ->
      let seq_len = Mlist.length tl in
      Sequence_core.intro mark label index (seq_len - index) t path_to_seq
    | _ -> fail seq_trm.loc "Sequence_basic.intro_after: the targeted instruction should belong to a sequence"
  ) tg

(* [intro_before ~mark ~label tg]: similar to [intro] but this transformation will include in the sequence all the
    instructions that come before the targeted instruction and belong to the same scope. *)
let%transfo intro_before ?(mark : mark = "") ? (label : label = "") (tg : target) : unit =
  Target.apply_on_targets (fun t p ->
    let path_to_seq, index = Internal.isolate_last_dir_in_seq p in
    let seq_trm = Path.resolve_path path_to_seq t in
    match seq_trm.desc with
    | Trm_seq _tl ->
      Sequence_core.intro mark label index (-index-1) t path_to_seq
    | _ -> fail seq_trm.loc "Sequence_basic.intro_after: the targeted instruction should belong to a sequence"
  ) tg

(* [intro_between ~mark ~label tg_beg tg_end]: this transformation is an advanced version of [intro].
     Here, the user can specify explicitly the targets to the first and the last instructions that
     are going to be isolated into a sequence. *)
let%transfo intro_between ?(mark : string = "") ?(label : label = "") (tg_beg : target) (tg_end : target) : unit =
  Internal.nobrace_remove_after ( fun  _ ->
  Trace.apply (fun t ->
    let ps_beg : (path * int) list = resolve_target_between tg_beg t in
    let ps_end : (path * int) list = resolve_target_between tg_end t in
    if List.length ps_beg <> List.length ps_end
      then fail t.loc "intro_between: not the same number of targets";
    let pis : (path * int * int) list = List.map2 (fun (p1,i1) (p2,i2) ->
      if p1 <> p2
        then fail t.loc "Sequence_basic.intro_between: targets for begin and end don't match the same sequences";
      if i2 <= i1
        then fail t.loc "Sequence_basic.intro_between: target for end should be past the target for start";
      (p1, i1, i2 - i1)) ps_beg ps_end in
    List.fold_left (fun t (p,i,nb) -> Sequence_core.intro mark label i nb t p) t pis))

(* [elim tg]: expects the target [tg] to point at a sequence that appears nested inside another sequence,
    e.g., points at [{t2;t3}] inside [{ t1; { t2; t3 }; t4 }]. It "elims" the contents of the inner sequence,
    producing e.g., [{ t1; t2; t3; t3}]. *)
let%transfo elim (tg : target) : unit =
  Trace.step_justif "correct if scoping is not broken (TODO: check)";
  Internal.nobrace_remove_after ( fun _ ->
  Target.apply_on_targets (Sequence_core.elim) tg)

(* [intro_on_instr ~mark ~visible tg]: expecets the target [tg] to point at an instruction,
    then it will wrap a sequence around that instruction.
    [visible] - denotes the visibility of a sequence. This means the that the the sequence is
        used only for internal purposes.
    [mark] - denotes the mark of the sub-sequence. Targeting sequences can be challanging hence having
          them marked before can make the apllication of the transformations easier. *)
let%transfo intro_on_instr ?(mark : mark = "")
                   ?(label : label = "")
                   ?(visible : bool = true) (tg : target) : unit =
   if not visible then Internal.nobrace_enter();
   Target.apply_on_targets (Sequence_core.intro_on_instr visible mark label) tg

(* [elim_on_instr tg]: expects the target [tg] to point at a sequence that contains a single instruction,
    then it removes that sequence. *)
let%transfo elim_on_instr (tg : target) : unit =
   Internal.nobrace_remove_after ( fun _ ->
    Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p, _) -> Sequence_core.elim t p) tg
   )

(* [split tg]: expects the target [tg] to point in between two instructions, then it will split the sequence
     that contains that location into two sequences. *)
let%transfo split (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply_on_targets_between (fun t (p, i) ->
      let is_fun_body = Internal.is_decl_body p in
      Sequence_core.split i is_fun_body t p) tg)

(* [partition ~braces blocks tg]: expects the target tg to point at a sequence, this transformations will split that sequence
      into blocks where the sizes of the blocks should be provided by the user.
        [blocks] - denotes the sizes for each block inside the sequence. By default it is empty, otherwise the sum of
          integers inside [blocks] should sum up to the number of instructions of the targeted sequence.
        [braces] - denotes a flag for the visibility of the blocks meaning that this block partition will be meaningful only for
          other transformations that call explicitly the partition transformation. *)
let%transfo partition ?(braces : bool = false) (blocks : int list) (tg : target) : unit =
  Trace.step_justif "correct if scoping is respected (TODO: check)";
  if not braces then Internal.nobrace_enter();
  Target.apply_on_targets (Sequence_core.partition blocks braces) tg

(* [shuffle ~braces tg]: expects the target [tg] to point at a sequence of blocks, this transformation will transpose the block structure

    think about a sequence of blocks as a matrix.
    {
      {{t11};{t12};{t13}};
      {{t21};{t22};{t23}};
      {{t31};{t32};{t33}};
    }
    this will be changed to:
    {
      {{t11};{t21};{t31}};
      {{t12};{t22};{t32}};
      {{t13};{t23};{t33}};
    } *)
let%transfo shuffle ?(braces : bool = false) (tg : target) : unit =
  Target.apply_on_targets (Sequence_core.shuffle braces) tg
