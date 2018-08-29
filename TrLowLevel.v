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

Inductive tr_state (C:typdefctx) (LLC:low_level_ctx) (φ:phi) : state -> state -> Prop :=
  | tr_state_intro : forall m m',
      dom m = dom m' ->
      (forall l lw T,
        l \indom m ->
            typing_val C φ m[l] T
        /\  tr_ll_val C LLC T m[l] lw
        /\  m'[l] = val_words lw) ->
      tr_state C LLC φ m m'.

(* ---------------------------------------------------------------------- *)
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
      tr_ll_accesses C LLC π o ->
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

(** Transformation of stacks: S ~ |S| *)

Inductive tr_stack_item (C:typdefctx) (LLC:low_level_ctx) : (var * val) -> (var * val) -> Prop :=
  | tr_stack_item_intro : forall x v v',
      tr_ptrs C LLC v v' -> 
      tr_stack_item C LLC (x, v) (x, v').

Inductive tr_stack (C:typdefctx) (LLC:low_level_ctx) : stack -> stack -> Prop :=
  | tr_stack_intro : forall S S',
      LibList.Forall2 (tr_stack_item C LLC) S S' ->
      tr_stack C LLC S S'.

(** Transformation of terms: t ~ |t| *)

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

Theorem red_tr : forall m2 t m1 φ S LLC v C S' m1' t',
  red C S m1 t m2 v ->
  low_level_ctx_ok C LLC ->
  tr_trm C LLC t t' ->
  tr_stack C LLC S S' ->
  tr_state C LLC φ m1 m1' ->
  state_typing C φ m1 ->
  exists m2' v',
      tr_ptrs C LLC v v'
  /\  tr_state C LLC φ m2 m2'
  /\  red C S' m1' t' m2' v'.
Proof.
Admitted.
















