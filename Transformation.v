(**

Basic transformations.

*)


Set Implicit Arguments.

Require Export Semantics.

(*
Definition is_add_zero t : option trm :=
  match t with
  | trm_binop binop_add (val_int 0) t2 => Some t2
  | _ => None
  end.

Lemma is_add_zero_some : forall t1 t2 t m1 m2 v,
  is_add_zero t1 t2 = Some t ->
  red m1 (trm_app t1 t2) m2 v ->
  red m1 t m2 v.
Proof.
  introv M R. unfolds is_add_zero.
  destruct t1; tryfalse. destruct t1_1; tryfalse.
  destruct v0; tryfalse. destruct p; tryfalse.
  destruct t1_2; tryfalse. destruct v0; tryfalse.
  destruct z; tryfalse. inverts M. Search val_add.
  inverts R.
  { inverts H1. inverts H3. }
  { asserts_rewrite (0 + n2 = n2). math. constructor. }
Qed.
*)

Global Instance Inhab_trm : Inhab trm.
Proof using. apply (Inhab_of_val (trm_val val_unit)). Qed.


(* Remove plus zero, i.e. t + 0 = t and 0 + t = t *)
Fixpoint tr (t:trm) : trm :=
  let aux := tr in
  match t with
  | trm_val v => trm_val v
  | trm_var x => trm_var x
  | trm_if t1 t2 t3 =>
      trm_if (aux t1) (aux t2) (aux t3)
  | trm_let b t1 t2 =>
      trm_let b (aux t1) (aux t2)
  | trm_app f ts => 
      let ts' := List.map aux ts in
      if isTrue (f = binop_add /\ exists t2, ts' = (trm_val 0)::t2::nil)  (* k = t2::nil *)
        then LibList.nth 1 ts'
        else trm_app f ts'
  end.


Theorem red_tr: forall E t s1 s2 v,
  red E s1 t s2 v -> 
  red E s1 (tr t) s2 v.
Proof.
  introv R. induction R; simpl. (*  try solve [ try case_if; constructors* ]. *)
  { constructors*. }
  { constructors*. }
  { eapply red_if.
    { eauto. }
    { case_if*. } }
  { constructors*. }
  { case_if.
     { destruct C as (C1&(k&C2)). inverts C1. inverts H.
        inverts C2. rew_listx. math_rewrite (0 + n2 = n2). constructor~. }
     { rew_logic in *. constructors*. } } }
  (* todo : optimize *)
  { case_if as C; [rew_logic in C; destruct C; tryfalse | ]. constructors*.  }
  { case_if as C; [rew_logic in C; destruct C; tryfalse | ]. constructors*.  }
  { case_if as C; [rew_logic in C; destruct C; tryfalse | ]. constructors*.  }
Qed.





(* 
  | trm_while t1 t2 => trm_while (aux t1) (aux t2)
  | trm_for v t1 t2 t3 =>
      trm_for v t(aux t1) (aux t2) (aux t3)
*)

Print transf_rm_pz.

Example simple_example_1:
  transf_rm_pz (val_add (val_int 0) (val_int 5)) = val_int 5.
Proof.
  simpl. reflexivity.
Qed.

(*Example simple_example_2:
  transf_rm_pz (val_add (val_int 5) (val_int 0)) = val_int 5.
Proof.
  simpl. reflexivity.
Qed.*)

Lemma simple_examples_generalisation: forall t,
  transf_rm_pz (val_add (val_int 0) t) = transf_rm_pz t.
Proof.
  intros. simpl. reflexivity.
Qed.

(* So far we haven't proved anything interesting since the result follows
   from the definition. We want to prove that when transformed terms are
   computed, their final output and their effect on the state is the same
   as that of the original code transformation. *)

(* I don't think this is right. *)
Lemma subst_inert_in_red: forall s1 b v t s2 w,
  red s1 (subst1 b v t) s2 w <-> red s1 t s2 w.
Proof.
Admitted.

Lemma transf_subst_commute: forall b v t,
  subst1 b v (transf_rm_pz t) = transf_rm_pz (subst1 b v t).
Proof.
  intros. rewrite subst1_eq_subst1'.
  induction t; simpl. (*try solve [fequal; try case_if; auto].*)
  { auto. }
  { case_if; simpl; auto. }
  { admit. }
  { fequal*. }
  { fequal.
    { eauto. }
    { case_if; eauto. }}
  { rewrite <- IHt1, <- IHt2. admit. }
  { fequal; eauto. }
  { admit. }
Qed.

Lemma red_transf_subst1 : forall m1 m2 b v t w,
  red m1 (transf_rm_pz (subst1 b v t)) m2 w ->
  red m1 (subst1 b v (transf_rm_pz t)) m2 w.
Proof.
Admitted.

Theorem red_transf: forall t s1 s2 v,
  red s1 t s2 v -> red s1 (transf_rm_pz t) s2 v.
Proof.
  introv R. induction R; simpl.
  { constructor. }
  { admit. }
  { eapply red_if.
    { eauto. }
    { case_if*. } }
  { applys* red_let. applys* red_transf_subst1.  }
  { }

(*
  intros t s1 s2 v. generalize s1, s2, v.
  induction t.
  (* Base cases *)
  - simpl. trivial.
  - simpl. trivial.
  (* Fix *)
  - admit.
  (* If *)
  - admit.
  (* Let *)
  - simpl. inversion 1. applys red_let.
    + apply IHt1 in H6. exact H6.
    + apply subst_inert_in_red in H7. apply IHt2 in H7.
      apply subst_inert_in_red. exact H7.
Admitted.*)

