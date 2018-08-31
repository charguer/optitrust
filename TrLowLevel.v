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


(* ---------------------------------------------------------------------- *)

(** Transformation of states: m ~ |m| *)

Inductive tr_state (C:typdefctx) (LLC:ll_typdefctx) (φ:phi) : state -> state -> Prop :=
  | tr_state_intro : forall m m',
      dom m = dom m' ->
      (forall l lw T,
        l \indom m ->
            typing_val C LLC φ m[l] T
        /\  tr_ll_val C LLC T m[l] lw
        /\  m'[l] = val_words lw) ->
      tr_state C LLC φ m m'.

(* ---------------------------------------------------------------------- *)
(* Transformation of a term from high-level to low-level. This is how the code is transformed. *)

Inductive tr_val (C:typdefctx) (LLC:ll_typdefctx) : val -> val -> Prop :=
  | tr_val_error :
      tr_val C LLC val_error val_error
  | tr_val_unit :
      tr_val C LLC val_unit val_unit
  | tr_val_bool : forall b,
      tr_val C LLC (val_bool b) (val_bool b)
  | tr_val_int : forall i,
      tr_val C LLC (val_int i) (val_int i)
  | tr_val_double : forall d,
      tr_val C LLC (val_double d) (val_double d)
  | tr_val_abstract_ptr : forall π l o,
      tr_ll_accesses C LLC π o ->
      tr_val C LLC (val_abstract_ptr l π) (val_concrete_ptr l o)
  | tr_val_array : forall T a a',
      List.Forall2 (tr_val C LLC) a a' ->
      tr_val C LLC (val_array T a) (val_array T a')
  | tr_val_struct : forall Tv s s',
      dom s = dom s' ->
      (forall f,
        index s f ->
        tr_val C LLC s[f] s'[f]) ->
      tr_val C LLC (val_struct (typ_var Tv) s) (val_struct (typ_var Tv) s').

(** Transformation of stacks: S ~ |S| *)

Inductive tr_stack_item (C:typdefctx) (LLC:ll_typdefctx) : (var * val) -> (var * val) -> Prop :=
  | tr_stack_item_intro : forall x v v',
      tr_val C LLC v v' -> 
      tr_stack_item C LLC (x, v) (x, v').

Inductive tr_stack (C:typdefctx) (LLC:ll_typdefctx) : stack -> stack -> Prop :=
  | tr_stack_intro : forall S S',
      LibList.Forall2 (tr_stack_item C LLC) S S' ->
      tr_stack C LLC S S'.

(** Transformation of terms: t ~ |t| *)

Inductive tr_trm (C:typdefctx) (LLC:ll_typdefctx) : trm -> trm -> Prop :=
  | tr_trm_val : forall v v',
      tr_val C LLC v v' ->
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
      tr_trm C LLC (trm_app (prim_set T) (t1::t2::nil)) (trm_app (prim_ll_set T) (t1'::t2'::nil))
  | tr_trm_new : forall T,
      tr_trm C LLC (trm_app (prim_new T) nil) (trm_app (prim_ll_new T) nil)
 (*| tr_trm_new_array :
      TODO: prim_new_array is needed here. *)
  | tr_trm_struct_access : forall Tfs t1' o Tv f t1 tr,
      Tv \indom C ->
      typing_struct C (typ_var Tv) Tfs ->
      f \indom Tfs ->
      o = (fields_offsets LLC)[Tv][f] ->
      tr_trm C LLC t1 t1' ->
      tr = trm_app (prim_ll_access Tfs[f]) (t1'::(trm_val (val_int o))::nil) ->
      tr_trm C LLC (trm_app (prim_struct_access (typ_var Tv) f) (t1::nil)) tr
  | tr_trm_array_access : forall os t2' n tr T' t1' toff T t1 t2,
      typing_array C T T' os ->
      typ_size (typvar_sizes LLC) T' n ->
      tr_trm C LLC t1 t1' ->
      tr_trm C LLC t2 t2' ->
      toff = trm_app (prim_binop binop_mul) (t2'::(trm_val (val_int n))::nil) ->
      tr = trm_app (prim_ll_access T') (t1'::toff::nil) ->
      tr_trm C LLC (trm_app (prim_array_access T) (t1::t2::nil)) tr
  | tr_trm_struct_get : forall t1 T f t1',
      tr_trm C LLC t1 t1' ->
      tr_trm C LLC (trm_app (prim_struct_get T f) (t1::nil)) (trm_app (prim_struct_get T f) (t1'::nil))
  | tr_trm_array_get : forall t1 t2 T t1' t2',
      tr_trm C LLC t1 t1' ->
      tr_trm C LLC t2 t2' ->
      tr_trm C LLC (trm_app (prim_array_get T) (t1::t2::nil)) (trm_app (prim_array_get T) (t1'::t2'::nil)).

Theorem red_tr : forall m2 t m1 φ S LLC v C S' m1' t',
  red C LLC S m1 t m2 v ->
  ll_typdefctx_ok C LLC ->
  tr_trm C LLC t t' ->
  tr_stack C LLC S S' ->
  tr_state C LLC φ m1 m1' ->
  state_typing C LLC φ m1 ->
  exists m2' v',
      tr_val C LLC v v'
  /\  tr_state C LLC φ m2 m2'
  /\  red C LLC S' m1' t' m2' v'.
Proof.
  introv HR Hok Ht HS Hm1 Hφ. gen φ t' S' m1'. induction HR; intros. 
  20:{ exists m1' val_error. inverts Ht. }
Admitted.
















