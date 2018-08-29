(**

This file describes transformations of the layout of records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export Semantics LibSet LibMap LibList TLCbuffer Typing.

(* ********************************************************************** *)
(* * Definition of the transformation *)

(** This is a special kind of transformation. We need to define new 
    new semantics. Essentially get and set for the concrete pointer.
    It can be included in the general semantics and just check that no
    concrete pointers are used in the other transformations. *)


(* ********************************************************************** *)
(* Contex holding low-level information about structs and their fields. *)

Definition typdefctx_sizes := map typvar size.
Definition typdefctx_fields_offsets := map typvar (map field offset).
Definition typdefctx_fields_order := map typvar (list field).

Record low_level_ctx := make_low_level_ctx {
  sizes : typdefctx_sizes;
  fields_offsets : typdefctx_fields_offsets;
  fields_order : typdefctx_fields_order }.

(* Ensures that the low-level context is correctly defined with respect to
   the type definitions context. *)

Inductive typdefctx_low_level (C:typdefctx) (LLC:low_level_ctx) : Prop :=
  typdefctx_low_level_intros : forall CS CFOff CFOrd,
    LLC = make_low_level_ctx CS CFOff CFOrd ->
    dom C = dom CS ->
    dom C = dom CFOff ->
    dom C = dom CFOrd ->
    (forall Tv Tfs,
      Tv \indom C ->
      C[Tv] = typ_struct Tfs ->
          dom Tfs = dom CFOff[Tv]
      /\  dom Tfs = to_set CFOrd[Tv]) -> 
    typdefctx_low_level C LLC.


(* ********************************************************************** *)
(* Computing the size of a type. Assuming the size of type variables
   are known. Used to transform array accesses. *)

Inductive typ_size (CS:typdefctx_sizes) : typ -> size -> Prop :=
  | typ_size_unit :
      typ_size CS (typ_unit) 1
  | typ_size_int :
      typ_size CS (typ_int) 1
  | typ_size_double :
      typ_size CS (typ_double) 2
  | typ_size_bool :
      typ_size CS (typ_bool) 1
  | typ_size_ptr : forall T',
      typ_size CS (typ_ptr T') 2
  | typ_size_array : forall n T' k,
      typ_size CS T' n ->
      typ_size CS (typ_array T' (Some k)) (n*k)
  | typ_size_struct : forall s n (m:monoid_op int) (g:field->size->size),
      dom s = dom n ->
      (forall f,
        f \indom s ->
        typ_size CS s[f] n[f]) ->
      m = monoid_make (fun a b => a + b) 0 ->
      g = (fun k v => v) ->
      typ_size CS (typ_struct s) (fold m g n)
  | typ_size_var : forall Tv,
      Tv \indom CS ->
      typ_size CS (typ_var Tv) CS[Tv].

(* Coherency between the offsets and the sizes. TODO: Find a better way. *)

Axiom special_map : list size -> map field offset.

Inductive low_level_ctx_ok (C:typdefctx) (LLC:low_level_ctx) : Prop :=
  | low_level_ctx_ok_intros : forall CS CFOrd CFOff,
      LLC = make_low_level_ctx CS CFOff CFOrd ->
      typdefctx_low_level C LLC ->
      (forall Tv Tfs,
        Tv \indom C ->
        C[Tv] = typ_struct Tfs ->
        (exists CFT CFS,
            CFT = List.map (fun f => Tfs[f]) CFOrd[Tv]
        /\  List.Forall2 (typ_size CS) CFT CFS
        /\  CS[Tv] = fold_right Z.add 0 CFS
        /\  CFOff[Tv] = special_map CFS)) ->
      low_level_ctx_ok C LLC.


(* ********************************************************************** *)
(* Given a list of accesses, computes the offset. Used to translate
   pointer values. *)

Inductive tr_accesses (C:typdefctx) (LLC:low_level_ctx) : accesses -> offset -> Prop :=
  | tr_accesses_nil :
      tr_accesses C LLC nil 0
  | tr_accesses_access_array : forall T T' os πs i n o,
      typing_array C T T' os ->
      typ_size (sizes LLC) T' n ->
      tr_accesses C LLC πs o ->
      tr_accesses C LLC ((access_array T i)::πs) ((i * n) + o)
  | tr_accesses_access_field : forall FO πs Tv f o,
      FO = fields_offsets LLC ->
      Tv \indom FO ->
      f \indom FO[Tv] ->
      tr_accesses C LLC πs o ->
      tr_accesses C LLC ((access_field (typ_var Tv) f)::πs) (FO[Tv][f] + o).


(* ********************************************************************** *)
(* Relates values with a list of words. This is how the memory is transformed. *)

Inductive tr_val (C:typdefctx) (LLC:low_level_ctx) : typ -> val -> list word -> Prop :=
  | tr_val_unit :
      tr_val C LLC typ_unit (val_basic val_unit) (word_int 0%Z::nil)
  | tr_val_bool : forall b,
      tr_val C LLC typ_bool (val_basic (val_bool b)) (word_int (if b then 1 else 0)%Z::nil)
  | tr_val_int : forall i,
      tr_val C LLC typ_int (val_basic (val_int i)) (word_int i::nil)
  | tr_val_double : forall d,
      tr_val C LLC typ_double (val_basic (val_double d)) (word_int d::word_int d::nil)
  | tr_val_abstract_ptr : forall T π l o,
      tr_accesses C LLC π o ->
      tr_val C LLC (typ_ptr T) (val_basic (val_abstract_ptr l π)) (word_int l::word_int o::nil)
  | tr_val_array : forall os T a a',
      List.Forall2 (tr_val C LLC T) a a' ->
      tr_val C LLC (typ_array T os) (val_array T a) (List.concat a')
  | tr_val_struct : forall FCOrd Tv Tfs sc st s s',
      FCOrd = fields_order LLC ->
      Tv \indom FCOrd ->
      Tv \indom C ->
      typing_struct C (typ_var Tv) Tfs ->
      sc = List.map (fun f => s[f]) FCOrd[Tv] ->
      st = List.map (fun f => Tfs[f]) FCOrd[Tv] ->
      length s' = length FCOrd[Tv] ->
      (forall i,
        index s' i ->
        tr_val C LLC st[i] sc[i] s'[i]) ->
      tr_val C LLC (typ_var Tv) (val_struct (typ_var Tv) s) (List.concat s').

(** Transformation of stacks: S ~ |S| *)

Inductive tr_stack_item (C:typdefctx) (LLC:low_level_ctx) (φ:phi) : (var * val) -> (var * val) -> Prop :=
  | tr_stack_item_intro : forall x v T lw,
      typing_val C φ v T ->
      tr_val C LLC T v lw -> 
      tr_stack_item C LLC φ (x, v) (x, (val_words lw)).

Inductive tr_stack (C:typdefctx) (LLC:low_level_ctx) (φ:phi) : stack -> stack -> Prop :=
  | tr_stack_intro : forall S S',
      LibList.Forall2 (tr_stack_item C LLC φ) S S' ->
      tr_stack C LLC φ S S'.

(** Transformation of states: m ~ |m| *)

Inductive tr_state (C:typdefctx) (LLC:low_level_ctx) (φ:phi) : state -> state -> Prop :=
  | tr_state_intro : forall m m',
      dom m = dom m' ->
      (forall l lw T,
        l \indom m ->
            typing_val C φ m[l] T
        /\  tr_val C LLC T m[l] lw
        /\  m'[l] = val_words lw) ->
      tr_state C LLC φ m m'.

(* ********************************************************************** *)
(* Transformation of a term from high-level to low-level. This is how the code is transformed. *)

Inductive tr_ptrs (C:typdefctx) (LLC:low_level_ctx) : val -> val -> Prop :=
  | tr_ptrs_unit :
      tr_ptrs C LLC (val_basic val_unit) (val_basic val_unit)
  | tr_ptrs_bool : forall b,
      tr_ptrs C LLC (val_basic (val_bool b)) (val_basic (val_bool b))
  | tr_ptrs_int : forall i,
      tr_ptrs C LLC (val_basic (val_int i)) (val_basic (val_int i))
  | tr_ptrs_double : forall d,
      tr_ptrs C LLC (val_basic (val_double d)) (val_basic (val_double d))
  | tr_ptrs_abstract_ptr : forall π l o,
      tr_accesses C LLC π o ->
      tr_ptrs C LLC (val_basic (val_abstract_ptr l π)) (val_basic (val_concrete_ptr l o))
  | tr_ptrs_array : forall T a a',
      List.Forall2 (tr_ptrs C LLC) a a' ->
      tr_ptrs C LLC (val_array T a) (val_array T a')
  | tr_ptrs_struct : forall Tv s s',
      dom s = dom s' ->
      (forall f,
        index s f ->
        tr_ptrs C LLC s[f] s'[f]) ->
      tr_ptrs C LLC (val_struct (typ_var Tv) s) (val_struct (typ_var Tv) s').

Inductive tr_trm (C:typdefctx) (LLC:low_level_ctx) : trm -> trm -> Prop :=
  | tr_trm_val : forall v v',
      tr_ptrs C LLC v v' ->
      tr_trm C LLC (trm_val v) (trm_val v')
  | tr_trm_var : forall x,
      tr_trm C LLC (trm_var x) (trm_var x)
  | tr_trm_if : forall t0 t1 t2 t0' t1' t2',
      tr_trm C LLC t0 t0' ->
      tr_trm C LLC t1 t1' ->
      tr_trm C LLC t2 t2' ->
      tr_trm C LLC (trm_if t0 t1 t2) (trm_if t0' t1' t2')
  | tr_trm_let : forall t0 t1 z t0' t1',
      tr_trm C LLC t0 t0' ->
      tr_trm C LLC t1 t1' ->
      tr_trm C LLC (trm_let z t0 t1) (trm_let z t0' t1')
  | tr_trm_binop : forall t1 t2 op t1' t2', 
      tr_trm C LLC t1 t1' ->
      tr_trm C LLC t2 t2' ->
      tr_trm C LLC (trm_app (prim_binop op) (t1::t2::nil)) (trm_app (prim_binop op) (t1'::t2'::nil))
  | tr_trm_get : forall t1 T t1',
      tr_trm C LLC t1 t1' ->
      tr_trm C LLC (trm_app (prim_get T) (t1::nil)) (trm_app (prim_ll_get T) (t1'::nil))
  | tr_trm_set : forall t1 t2 T t1' t2',
      tr_trm C LLC t1 t1' ->
      tr_trm C LLC (trm_app (prim_set T) (t1::t2::nil)) (trm_app (prim_ll_set T) (t1'::t2'::nil)).
(*| tr_trm_new : 
  | tr_trm_new_array :
  | tr_trm_struct_access :
  | tr_trm_array_access :
  | tr_trm_struct_get :
  | tr_trm_array_get :*)

Theorem red_tr : forall m2 t m1 S LLC v C S' m1' t',
  red C S m1 t m2 v ->
  low_level_ctx_ok C LLC ->
  tr_trm C LLC t t' ->
  tr_state C LLC m1 m1' ->
  tr_stack C LLC S S' ->
  exists m2' lw,
      tr_val C LLC v lw
  /\  tr_state C LLC m2 m2'
  /\  red C S' m1' t' m2' (val_words lw).
Proof.
Admitted.

(* This goes in Semantics. *)

(* ---------------------------------------------------------------------- *)
(** Semantics of low_level memory accesses *)

(** m(l)[o] = v *)

Axiom list_splice : forall A, list A -> int -> int -> list A -> Prop.

Inductive read_ll_state (m:state) (l:loc) (o:offset) (n:size) (v:val) : Prop :=
  | read_state_intro : forall ws ws',
      l \indom m ->
      m[l] = val_words ws ->
      v = val_words ws' ->
      list_splice ws o n ws' ->
      read_ll_state m l o n v.

(** m[l := m(l)[π := w]] = m' *)

Axiom list_splice_update : forall A, list A -> int -> int -> list A -> list A -> Prop.

Inductive write_ll_state (m:state) (l:loc) (o:offset) (n:size) (w:val) (m':state) : Prop :=
  | write_mem_intro : forall ws ws' ws'',
      l \indom m ->
      m[l] = val_words ws ->
      w = val_words ws' ->
      list_splice_update ws o n ws' ws'' ->
      m' = m[l := (val_words ws'')] ->
      write_ll_state m l o n w m'.

(* New reduction rules *)
Inductive red' (C:typdefctx) (LLC:low_level_ctx) :  stack -> state -> trm -> state -> val -> Prop :=
  | red_ll_get : forall l n o S T v1 m vr,
      v1 = val_concrete_ptr l o ->
      read_ll_state m l o n vr ->
      typ_size (sizes LLC) T n ->
      ~ is_undef vr ->
      red' C LLC S m (trm_app (prim_ll_get T) ((trm_val v1)::nil)) m vr
  | red_ll_set : forall l o n S m1 T v1 v2 m2 vr,
      v1 = val_words (word_int l::word_int o::nil) ->
      vr = val_unit ->
      typ_size (sizes LLC) T n ->
      write_ll_state m1 l o n v2 m2 ->
      red' C LLC S m1 (trm_app (prim_ll_set T) ((trm_val v1)::(trm_val v2)::nil)) m2 vr
  | red_ll_new : forall l n ws S m1 T m2 vr,
      vr = val_words (word_int l::word_int 0::nil) ->
      l <> null ->
      l \notindom m1 ->
      wf_typ C T ->
      typ_size (sizes LLC) T n ->
      ws = LibListZ.make n word_undef ->
      m2 = m1[l := (val_words ws)] ->
      red' C LLC S m1 (trm_app (prim_ll_new T) nil) m2 vr
  | red_ll_access : forall l o o' S m1 T v1 v2 m2 vr,
      vr = val_words (word_int l::word_int (o+o')::nil) ->
      v1 = val_words (word_int l::word_int o::nil) ->
      v2 = val_words (word_int o'::nil) ->
      red' C LLC S m1 (trm_app (prim_ll_access T) ((trm_val v1)::(trm_val v2)::nil)) m2 vr
  | red_ll_val_get : forall ws o n ws' S m1 T v1 v2 m2 vr,
      vr = val_words ws' ->
      v1 = val_words ws ->
      v2 = val_words (word_int o::nil) ->
      typ_size (sizes LLC) T n ->
      list_splice ws o n ws' ->
      red' C LLC S m1 (trm_app (prim_ll_val_get T) ((trm_val v1)::(trm_val v2)::nil)) m2 vr.

















