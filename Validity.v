(**

This file describes transformations of the layout of records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export TLCbuffer Typing Semantics.


(* ---------------------------------------------------------------------- *)
(** Validity of types *)

Inductive valid_typ (C:typdefctx) : typ -> Prop :=
  | valid_typ_unit :
      valid_typ C typ_unit
  | valid_typ_int :
      valid_typ C typ_int
  | valid_typ_double :
      valid_typ C typ_double
  | valid_typ_bool :
      valid_typ C typ_bool
  | valid_typ_ptr : forall T,
      valid_typ C T ->
      valid_typ C (typ_ptr T)
  | valid_typ_array : forall T os,
      valid_typ C T ->
      valid_typ C (typ_array T os)
  | valid_typ_struct : forall Tfs,
      (forall f,
        f \indom Tfs ->
        valid_typ C Tfs[f]) ->
      valid_typ C (typ_struct Tfs)
  | valid_typ_var : forall Tv,
      Tv \indom C ->
      valid_typ C C[Tv] ->
      valid_typ C (typ_var Tv).

Definition valid_phi (C:typdefctx) (φ:phi) : Prop :=
  forall l,
    l \indom φ ->
    valid_typ C φ[l].

Inductive valid_accesses (C:typdefctx) : accesses -> Prop :=
  | valid_accesses_nil :
      valid_accesses C nil 
  | valid_accesses_cons_array : forall T i π,
      valid_typ C T ->
      valid_accesses C π ->
      valid_accesses C ((access_array T i)::π)
  | valid_accesses_cons_field : forall T f π,
      valid_typ C T ->
      valid_accesses C π ->
      valid_accesses C ((access_field T f)::π).


(* Lemmas about the connection of validity and typing. *)

Lemma valid_typing_array : forall T os C Ta,
  typing_array C Ta T os ->
  valid_typ C Ta ->
  valid_typ C T.
Proof.
  introv HTa HT. induction HTa; inverts~ HT.
Qed.

Lemma valid_typing_struct : forall Tfs C Ts,
  typing_struct C Ts Tfs ->
  valid_typ C Ts ->
  (forall f, 
    f \indom Tfs ->
    valid_typ C Tfs[f]).
Proof.
  introv HTs HT. induction HTs; inverts~ HT.
Qed.

Lemma valid_typing_array_inv : forall T os C Ta,
  typing_array C Ta T os ->
  valid_typ C T ->
  valid_typ C Ta.
Proof.
  introv HTa HT. induction HTa; constructors~.
Qed.

Lemma valid_typing_struct_inv : forall Tfs C Ts,
  typing_struct C Ts Tfs ->
  (forall f, 
    f \indom Tfs ->
    valid_typ C Tfs[f]) ->
  valid_typ C Ts.
Proof.
  introv HTs HT. induction HTs; constructors~.
Qed.

Lemma follow_typ_valid_accesses : forall T T' C π,
  valid_typ C T ->
  follow_typ C T π T' ->
  valid_accesses C π .
Proof.
  introv HT HF. induction HF; constructors~.
  { applys IHHF. applys* valid_typing_array. }
  { applys IHHF. applys* valid_typing_struct. }
Qed.

Lemma follow_typ_valid_typ : forall T T' C π,
  valid_typ C T ->
  follow_typ C T π T' ->
  valid_accesses C π  ->
  valid_typ C T'.
Proof.
  introv HT HF Hva. induction HF.   
  { auto. }
  { inverts Hva. applys~ IHHF. applys* valid_typing_array. }
  { inverts Hva. applys~ IHHF. applys* valid_typing_struct. }
Qed.

(* ---------------------------------------------------------------------- *)
(** Validity of values and terms *)

(* A value is valid if all of the types that appear in it are. *)

Inductive valid_val (C:typdefctx) : val -> Prop :=
  | valid_val_unit :
      valid_val C val_unit
  | valid_val_bool : forall b,
      valid_val C (val_bool b)
  | valid_val_int : forall i,
      valid_val C (val_int i)
  | valid_val_double : forall d,
      valid_val C (val_double d)
  | valid_val_abstract_ptr : forall l π,
      valid_accesses C π ->
      valid_val C (val_abstract_ptr l π)
  | valid_val_array : forall Ta a,
      valid_typ C Ta ->
      (forall i,
        index a i ->
        valid_val C a[i]) ->
      valid_val C (val_array Ta a)
  | valid_val_struct : forall Ts s,
      valid_typ C Ts ->
      (forall f,
        f \indom s ->
        valid_val C s[f]) ->
      valid_val C (val_struct Ts s).

(* A term is valid if all of the types that appear in it are. *)

Inductive valid_prim (C:typdefctx) : prim -> Prop :=
  | valid_prim_binop : forall bop,
      valid_prim C (prim_binop bop)
  | valid_prim_get : forall T,
      valid_typ C T ->
      valid_prim C (prim_get T)
  | valid_prim_set : forall T,
      valid_typ C T ->
      valid_prim C (prim_set T)
  | valid_prim_new : forall T,
      valid_typ C T ->
      valid_prim C (prim_new T)
  | valid_prim_new_array : forall T,
      valid_typ C T ->
      valid_prim C (prim_new_array T)
  | valid_prim_struct_access : forall T f,
      valid_typ C T ->
      valid_prim C (prim_struct_access T f)
  | valid_prim_array_access : forall T,
      valid_typ C T ->
      valid_prim C (prim_array_access T)
  | valid_prim_struct_get : forall T f,
      valid_typ C T ->
      valid_prim C (prim_struct_get T f)
  | valid_prim_array_get : forall T,
      valid_typ C T ->
      valid_prim C (prim_array_get T).

Inductive valid_trm (C:typdefctx) : trm -> Prop :=
  | valid_trm_val : forall v,
      valid_val C v ->
      valid_trm C (trm_val v)
  | valid_trm_var : forall x,
      valid_trm C (trm_var x)
  | valid_trm_if : forall t0 t1 t2,
      valid_trm C t0 ->
      valid_trm C t1 ->
      valid_trm C t2 ->
      valid_trm C (trm_if t0 t1 t2)
  | valid_trm_let : forall x t0 t1,
      valid_trm C t0 ->
      valid_trm C t1 ->
      valid_trm C (trm_let x t0 t1)
  | valid_trm_app : forall op ts,
      (forall i,
        index ts i ->
        valid_trm C ts[i]) ->
      valid_prim C op ->
      valid_trm C (trm_app op ts).

(* Valid stack *)

Definition valid_stack (C:typdefctx) (S:stack) : Prop :=
  forall x v,
    Ctx.lookup x S = Some v ->
    valid_val C v.

(* Valid state *)

Definition valid_state (C:typdefctx) (m:state) : Prop :=
  forall l,
    l \indom m ->
    valid_val C m[l].

(* Results *)

Lemma stack_valid_add : forall C x v S,
  valid_stack C S ->
  valid_val C v ->
  valid_stack C (Ctx.add x v S).
Proof.
  introv HS Hv. unfolds Ctx.add. destruct~ x.
  unfolds. introv HCl. unfold Ctx.lookup in HCl.
  case_if in HCl.
  { inverts~ HCl. }
  { applys~ HS x. }
Qed.

Lemma red_valid : forall S m1 t C m2 v,
  red C S m1 t m2 v ->
  valid_stack C S ->
  valid_state C m1 ->
  valid_trm C t ->
  ~ is_error v ->
      valid_state C m2
  /\  valid_val C v.
Proof.
  introv HR HS Hm1 Ht He. induction HR; intros;
  try solve [ inverts* Ht ; forwards*: He ].
  { inverts Ht. forwards* (Hm2&HVb): IHHR1.
    applys~ IHHR2. case_if*. }
  { inverts Ht. forwards* (Hm2&HVb): IHHR1.
    applys~ IHHR2. applys~ stack_valid_add. }
Qed.






























