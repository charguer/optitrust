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
  | valid_trm_app_args_0 : forall op,
      valid_prim C op ->
      valid_trm C (trm_app op nil)
  | valid_trm_app_args_1 : forall op t1,
      valid_prim C op ->
      valid_trm C t1 ->
      valid_trm C (trm_app op (t1::nil))
  | valid_trm_app_args_2 : forall op t1 t2,
      valid_prim C op ->
      valid_trm C t1 ->
      valid_trm C t2 ->
      valid_trm C (trm_app op (t1::t2::nil)).


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

Lemma redbinop_valid : forall op v1 v2 C vr,
  redbinop op v1 v2 vr ->
  valid_val C v1 ->
  valid_val C v2 ->
  valid_val C vr.
Proof.
  introv HR Hv1 Hv2. induction HR; constructors*.
Qed.

Lemma read_accesses_valid : forall v1 π C v2,
  read_accesses v1 π v2 ->
  valid_val C v1 ->
  valid_accesses C π ->
  valid_val C v2.
Proof.
  introv HR Hv1 Hπ. induction HR.
  { auto. }
  { inverts Hπ. inverts Hv1 as Hv1 Hai. 
    applys~ IHHR. }
  { inverts Hπ. inverts Hv1 as Hv1 Hsf.
    applys~ IHHR. }
Qed.

Lemma read_state_valid : forall m l π C v,
  read_state m l π v ->
  valid_state C m ->
  valid_accesses C π ->
  valid_val C v.
Proof.
  introv HR Hm Hπ. unfolds valid_state. inverts HR.
  forwards*: Hm. applys* read_accesses_valid.
Qed.

Lemma write_accesses_valid : forall v1 w π C v2,
  write_accesses v1 π w v2 ->
  valid_val C v1 ->
  valid_val C w ->
  valid_accesses C π ->
  valid_val C v2.
Proof.
  introv HW Hv1 Hw Hπ. induction HW.
  { auto. }
  { subst. inverts Hπ. inverts Hv1 as Hv1 Ha0i.
    constructors*. introv Hi. rew_index~ in Hi. rew_reads*. }
  { subst. inverts Hπ. inverts Hv1 as Hv1 Hs1f.
    constructors*. introv Hf. rew_reads*. }
Qed.

Lemma write_state_valid : forall m1 l π v C m2,
  write_state m1 l π v m2 ->
  valid_state C m1 ->
  valid_accesses C π ->
  valid_val C v ->
  valid_state C m2.
Proof. 
  introv HW Hm1 Hπ Hv. inverts HW. unfolds valid_state.
  forwards*: Hm1. introv Hl0. rew_reads*; intros.
  { applys* write_accesses_valid. }
  { applys~ Hm1. applys~ indom_update_inv_neq l l0 v2. }
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
  { inverts Ht as Hop Hv1 Hv2.
    inverts Hv1 as Hv1. inverts Hv2 as Hv2.
    splits~. applys* redbinop_valid. }
  { subst. inverts Ht as Hop Hp. inverts Hp as Hp. 
    inverts Hp as Hπ. splits~. 
    applys* read_state_valid. }
  { subst. inverts Ht as Hop Hp Hv2. 
    inverts Hv2 as Hv2. inverts Hp as Hp.
    inverts Hp as Hπ. splits; try constructors*.
    applys* write_state_valid. }
  {  }
Qed.




























