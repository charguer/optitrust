(**

Basic transformations.

*)


Set Implicit Arguments.

Require Export LambdaSemantics.

(* Remove plus zero, i.e. t + 0 = t and 0 + t = t *)
Fixpoint transf_rm_pz (t:trm) : trm :=
  match t with
  | trm_app (trm_app val_add (val_int 0)) t1 => t1
  | trm_app (trm_app val_add t1) (val_int 0) => t1
  | trm_val v => trm_val v
  | trm_var x => trm_var x
  | trm_fix b1 b2 t1 => trm_fix b1 b2 (transf_rm_pz t1)
  | trm_if t1 t2 t3 => 
      trm_if (transf_rm_pz t1) (transf_rm_pz t2) (transf_rm_pz t3)
  | trm_let b t1 t2 => 
      trm_let b (transf_rm_pz t1) (transf_rm_pz t2)
  | trm_app t1 t2 => 
      trm_app (transf_rm_pz t1) (transf_rm_pz t2)
  | trm_while t1 t2 => trm_while (transf_rm_pz t1) (transf_rm_pz t2)
  | trm_for v t1 t2 t3 => 
      trm_for v t(transf_rm_pz t1) (transf_rm_pz t2) (transf_rm_pz t3)
  end.

Example simple_example_1: 
  transf_rm_pz (val_add (val_int 0) (val_int 5)) = val_int 5.
Proof.
  simpl. reflexivity.
Qed.

Example simple_example_2: 
  transf_rm_pz (val_add (val_int 5) (val_int 0)) = val_int 5.
Proof.
  simpl. reflexivity.
Qed.

Lemma simple_examples_generalisation: forall t,
  transf_rm_pz (val_add t (val_int 0)) = t /\
  transf_rm_pz (val_add (val_int 0) t) = t.
Proof.
  intros t. intuition. destruct t.
  - destruct v. 
    + simpl. reflexivity.
    + simpl. reflexivity.
    + destruct z.
       { simpl. reflexivity. }
       { simpl. reflexivity. }
       { simpl. reflexivity. }
    + simpl. reflexivity.
    + simpl. reflexivity.
    + simpl. reflexivity.
  - simpl. reflexivity.
  - simpl. reflexivity.
  - simpl. reflexivity.
  - simpl. reflexivity.
  - simpl. reflexivity.
  - simpl. reflexivity.
  - simpl. reflexivity.
Qed.

(* So far we haven't proved anything interesting since the result follows
   from the definition. We want to prove that when transformed terms are
   computed, their final output and their effect on the state is the same
   as that of the original code transformation. *)



Theorem transf_preserves_semantics: forall t s1 s2 v,
  red s1 t s2 v -> red s1 (transf_rm_pz t) s2 v.
Proof. 
  intros t s1 s2 v. intros H. 
  induction t.
  - simpl. exact H.
  - simpl. exact H.
  - admit. (* Fix... *)
  - admit. (* Ifs. But how do I know t1 is a bool? *)
  - simpl. applys* red_let. 
Admitted.

