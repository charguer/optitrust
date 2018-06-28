(**

Basic transformations.

*)


Set Implicit Arguments.

Require Export Semantics.


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
      if isTrue (f = binop_add /\ exists t2, ts' = (trm_val 0)::t2::nil)
        then LibList.nth 1 ts'
        else trm_app f ts'
  end.




(* Semantics preserved by tr. *)
(*Theorem red_tr: forall E t s1 s2 v,
  red E s1 t s2 v -> 
  red E s1 (tr t) s2 v.
Proof.
  introv R. induction R; simpl. (* try solve [ try case_if; constructors* ]. *)
  { constructors*. }
  { constructors*. }
  { eapply red_if.
    { eauto. }
    { case_if*. } }
  { constructors*. }
  { case_if.
     { destruct C as (C1&(k&C2)). inverts C1. inverts H.
        inverts C2. rew_listx. math_rewrite (0 + n2 = n2). constructor~. }
     { rew_logic in *. constructors*. } }
  (* TODO: optimize *)
  { case_if as C; [rew_logic in C; destruct C; tryfalse | ]. constructors*.  }
  { case_if as C; [rew_logic in C; destruct C; tryfalse | ]. constructors*.  }
  { case_if as C; [rew_logic in C; destruct C; tryfalse | ]. constructors*.  }
Qed.*)
