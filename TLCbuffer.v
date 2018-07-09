(**

This file contains temporary definitions that will eventually
get merged into the various files from the TLC library.

Author: Arthur Charguéraud.
License: MIT.

*)


Set Implicit Arguments.
Require Import LibTactics LibLogic LibList LibMap LibReflect.
Require LibListZ.
Generalizable Variables A B C.



(*----------------------*)
(* Nat *)

From TLC Require Import LibNat LibInt.


Section NatSimpl.
Open Scope nat_scope.
Implicit Types n : nat.

Lemma nat_zero_plus : forall n,
  0 + n = n.
Proof using. intros. math. Qed.

Lemma nat_plus_zero : forall n,
  n + 0 = n.
Proof using. intros. math. Qed.

Lemma nat_plus_succ : forall n1 n2,
  n1 + S n2 = S (n1 + n2).
Proof using. intros. math. Qed.

Lemma nat_minus_zero : forall n,
  n - 0 = n.
Proof using. intros. math. Qed.

Lemma nat_succ_minus_succ : forall n1 n2,
  S n1 - S n2 = n1 - n2.
Proof using. intros. math. Qed.

Lemma nat_minus_same : forall n,
  n - n = 0.
Proof using. intros. math. Qed.

Lemma nat_plus_minus_same : forall n1 n2,
  n1 + n2 - n1 = n2.
Proof using. intros. math. Qed.

End NatSimpl.

Hint Rewrite nat_zero_plus nat_plus_zero nat_plus_succ
  nat_minus_zero nat_succ_minus_succ
  nat_minus_same nat_plus_minus_same : rew_nat.


(** [nat_seq i n] generates a list of variables [x1;x2;..;xn]
    with [x1=i] and [xn=i+n-1]. Such lists are useful for
    generic programming. *)

Fixpoint nat_seq (start:nat) (nb:nat) :=
  match nb with
  | O => nil
  | S nb' => start :: nat_seq (S start) nb'
  end.


Lemma length_nat_seq : forall start nb,
  length (nat_seq start nb) = nb.
Proof using.
  intros. gen start. induction nb; simpl; intros.
  { auto. } { rew_list. rewrite~ IHnb. }
Qed.


(*----------------------*)
(*-- LATER: move to TLC LibNatExec *)

Fixpoint nat_compare (x y : nat) :=
  match x, y with
  | O, O => true
  | S x', S y' => nat_compare x' y'
  | _, _ => false
  end.

Lemma nat_compare_eq : forall n1 n2,
  nat_compare n1 n2 = isTrue (n1 = n2).
Proof using.
  intros n1. induction n1; intros; destruct n2; simpl; rew_bool_eq; auto_false.
  rewrite IHn1. extens. rew_istrue. math.
Qed.

(*----------------------*)
(* ListExec *)

From TLC Require Import LibLogic. (* needed? *)
From TLC Require Import LibReflect.

Definition is_not_nil A (l:list A) : bool :=
  match l with
  | nil => false
  | _ => true
  end.

Lemma is_not_nil_eq : forall A (l:list A),
  is_not_nil l = isTrue (l <> nil).
Proof.
  intros. destruct l; simpl; rew_bool_eq; auto_false.
Qed.

Lemma List_length_eq : forall A (l:list A),
  List.length l = LibList.length l.
Proof using. intros. induction l; simpl; rew_list; auto. Qed.


Lemma List_app_eq : forall A (L1 L2:list A),
  List.app L1 L2 = LibList.app L1 L2.
Proof using.
  intros. induction L1; simpl; rew_list; congruence.
Qed.

Lemma List_rev_eq : forall A (L:list A),
  List.rev L = LibList.rev L.
Proof using.
  intros. induction L; simpl; rew_list. { auto. }
  { rewrite List_app_eq. congruence. }
Qed.

Lemma List_map_eq : forall A B (f:A->B) (L:list A),
  List.map f L = LibList.map f L.
Proof using.
  intros. induction L; simpl; rew_listx; congruence.
Qed.

Lemma List_combine_eq : forall A B (L1:list A) (L2:list B),
  length L1 = length L2 ->
  List.combine L1 L2 = LibList.combine L1 L2.
Proof using.
  introv E. gen L2.
  induction L1 as [|x1 L1']; intros; destruct L2 as [|x2 L2']; tryfalse.
  { auto. }
  { rew_list in E. simpl. fequals~. }
Qed.


(*----------------------*)
(* LibMap *)

Section IndexProperties.

Lemma index_update_eq : forall A `{Inhab B} (m:map A B) (i j:A) (v:B),
  index (m[i:=v]) j = (j = i \/ index m j).
Proof using. intros. rewrite index_eq_indom, indom_update_eq; auto. Qed.

Lemma index_update_inv : forall A `{Inhab B} (m:map A B) (i j:A) (v:B),
  index (m[i:=v]) j ->
  (j = i \/ index m j).
Proof using. intros. rewrite index_update_eq in *; auto. Qed.

Lemma index_of_index_update_at_index : forall A B `{Inhab B} (m:map A B) (i j:A) (v:B),
  index (m[i:=v]) j ->
  index m i ->
  index m j.
Proof using.
  introv IB H1 H2. rewrite index_update_eq in H1. destruct H1; subst*. 
  rewrite index_eq_indom in *. auto.
Qed.

Lemma index_of_index_update_neq : forall A `{Inhab B} (m:map A B) (i j:A) (v:B),
  index (m[i:=v]) j ->
  i <> j ->
  index m j.
Proof using.
  introv H1 H2 N. rewrite index_update_eq in *; auto. destruct H2; auto_false.
Qed.

Lemma indom_update_of_index : forall A `{Inhab B} (m:map A B) (i j:A) (v:B),
  index m j ->
  index (m[i:=v]) j.
Proof using. intros. rewrite~ index_update_eq. Qed.

Lemma index_update_same : forall A `{Inhab B} (m:map A B) (i:A) (v:B),
  index (m[i:=v]) i.
Proof using. intros. rewrite~ index_update_eq. Qed.

Lemma index_of_update_neq' : forall A (v:A) i i' (l:list A),
  index l[i:=v] i' ->
  i <> i' ->
  index l i'.
Proof using. introv H N. rewrite~ LibListZ.index_update_eq in H. Qed.

Lemma index_dom_same : forall A `{ Inhab B } `{ Inhab  C } (m1:map A B) (m2:map A C) k,
  dom m2 = dom m1 ->
  index m1 k ->
  index m2 k.
Proof using.
  introv _ _ HD Hi. rewrite index_eq_indom in *. rewrite* HD.
Qed.

End IndexProperties.


(*----------------------*)
(* Hint for LibListZ *)

Hint Rewrite LibListZ.length_map LibListZ.index_map_eq : rew_arr.


(*----------------------*)
(* LibInt *)

Global Opaque Z.mul.
Global Opaque Z.add.


(* ---------------------------------------------------------------------- *)
(* LibTactics *)

Ltac fequal_base ::=
  let go := f_equal_fixed; [ fequal_base | ] in
  match goal with
  | |- exist _ _ = exist _ _ => apply exist_eq_exist
  | |- (_,_,_) = (_,_,_) => go
  | |- (_,_,_,_) = (_,_,_,_) => go
  | |- (_,_,_,_,_) = (_,_,_,_,_) => go
  | |- (_,_,_,_,_,_) = (_,_,_,_,_,_) => go
  | |- _ => f_equal_fixed
  end.

(* [isubst] generalizes [intro_subst] *)

Ltac isbust_core tt :=
  match goal with |- forall _, _ = _ -> _ =>
    let X := fresh "TEMP" in
    let HX := fresh "E" X in
    intros X HX; subst X
  end.

Tactic Notation "isubst" :=
  isbust_core tt.


(* ---------------------------------------------------------------------- *)
(* Cases *)

Tactic Notation "cases" constr(E) :=
  let H := fresh "Eq" in cases E as H.




(* ---------------------------------------------------------------------- *)
(* Induction on pairs of lists *)

Lemma list2_ind : forall A B (P:list A->list B->Prop) l1 l2,
  length l1 = length l2 ->
  P nil nil ->
  (forall x1 xs1 x2 xs2,
     length xs1 = length xs2 -> P xs1 xs2 -> P (x1::xs1) (x2::xs2)) ->
  P l1 l2.
Proof using.
  introv E M1 M2. gen l2. induction l1 as [|x1 l1']; intros;
   destruct l2 as [|x2 l2']; try solve [false; math]; auto.
Qed.

Tactic Notation "list2_ind" constr(l1) constr(l2) :=
  pattern l2; pattern l1;
  match goal with |- (fun a => (fun b => @?P a b) _) _ =>
   (* applys list2_ind P *)
   let X := fresh "P" in set (X := P); applys list2_ind X; unfold X; try clear X
 end.

Tactic Notation "list2_ind" "~" constr(l1) constr(l2) :=
  list2_ind l1 l2; auto_tilde.

Tactic Notation "list2_ind" "*" constr(l1) constr(l2) :=
  list2_ind l1 l2; auto_star.

Tactic Notation "list2_ind" constr(E) :=
  match type of E with length ?l1 = length ?l2 =>
    list2_ind l1 l2; [ apply E | | ] end.

(** Same, but on last element *)

Lemma list2_ind_last : forall A B (P:list A->list B->Prop) l1 l2,
  length l1 = length l2 ->
  P nil nil ->
  (forall x1 xs1 x2 xs2,
     length xs1 = length xs2 -> P xs1 xs2 -> P (xs1&x1) (xs2&x2)) ->
  P l1 l2.
Proof using.
  introv E M1 M2. gen l2. induction l1 using list_ind_last;
   [| rename a into x1, l1 into l1']; intros;
   destruct (last_case l2) as [|(x2&l2'&E2)]; subst; rew_list in *;
   try solve [false; math]; auto.
Qed.

Tactic Notation "list2_ind_last" constr(l1) constr(l2) :=
  pattern l2; pattern l1;
  match goal with |- (fun a => (fun b => @?P a b) _) _ =>
   (* applys list2_ind P *)
   let X := fresh "P" in set (X := P); applys list2_ind_last X; unfold X; try clear X
 end.

Tactic Notation "list2_ind_last" "~" constr(l1) constr(l2) :=
  list2_ind_last l1 l2; auto_tilde.

Tactic Notation "list2_ind_last" "*" constr(l1) constr(l2) :=
  list2_ind_last l1 l2; auto_star.

Tactic Notation "list2_ind_last" constr(E) :=
  match type of E with length ?l1 = length ?l2 =>
    list2_ind_last l1 l2; [ apply E | | ] end.





(* ---------------------------------------------------------------------- *)
(* Functional relations *)

Ltac exploit_functional P P_functional :=
  match goal with H1: ?F ?x1, H2: ?F ?x2 |- _=>
  match get_head F with P =>
    let HTEMP := fresh in
    forwards HTEMP: P_functional H2 H1; [typeclass | subst_hyp HTEMP; clear H2] end end.


(* ---------------------------------------------------------------------- *)
(* Functional relations *)

(** [inverts_where C] inverts the bottom-most hypothesis that contains
    subexpression [C] *)

Ltac inverts_where C :=
  match goal with H: context[C] |- _ => inverts H end.

(** [inverts_head P] inverts the bottom-most hypothesis whose
    head is the predicate [P]. *)

Ltac inverts_head P :=
  match goal with H: context[?C] |- _ =>
    match get_head C with P => inverts H end end.

(** [rewrites_head P] rewrites the bottom-most hypothesis whose
    head is the predicate [P]. *)

Ltac rewrites_head P := 
  match goal with H: ?A = ?B |- _ => 
      match get_head A with P => rewrites H end end.


(* ---------------------------------------------------------------------- *)
(* Set *)

Require Import LibSet.

Tactic Notation "rew_set" "~" :=
  rew_set; auto_tilde.
Tactic Notation "rew_set" "*" :=
  rew_set; auto_star.


Notation "x \indom E" := (x \in dom E)
  (at level 39) : container_scope.
Notation "x \notindom E" := (x \notin dom E)
  (at level 39) : container_scope.
Notation "x \indom E" := (x \in (dom E : set _))
  (at level 39) : container_scope.
Notation "x \notindom E" := (x \notin ((dom E) : set _))
  (at level 39) : container_scope.



(* ---------------------------------------------------------------------- *)
(* Map *)

Require Import LibMap.
Section MapLemma.
Transparent binds dom union.
Transparent map empty_inst single_bind_inst binds_inst
 union_inst restrict_inst remove_inst read_inst
 dom_inst disjoint_inst index_inst fold_inst.

(* only for internal use *)
Lemma read_impl_indom_eq : forall A `{Inhab B} (M:map A B) k,
  k \indom M -> M k = Some (M[k]).
Proof using.
  introv R. simpls. unfolds read_impl, dom_impl.
  rewrite in_set_st_eq in R. destruct* (M k).
Qed.

(* only for internal use *)
Lemma read_impl_notindom_eq : forall A B (M:map A B) k,
  k \notindom M -> M k = None.
Proof using.
  introv R. simpls. unfolds read_impl, dom_impl.
  unfold notin in R. rewrite in_set_st_eq in R.
  rewrite~ not_not_eq in R.
Qed.

Arguments read_impl_indom_eq : clear implicits.

Lemma read_extens : forall A `{Inhab B} (m1 m2:map A B),
  dom m1 = dom m2 ->
  (forall i, i \indom m1 -> m1[i] = m2[i]) ->
  m1 = m2.
Proof using. 
  introv ED EV. extens. intros i. tests C: (i \indom m1).
  { rewrites~ (>> read_impl_indom_eq m1).
    rewrites~ (>> read_impl_indom_eq m2). rewrite~ <- ED.
    rewrite~ EV. }
  { rewrite~ read_impl_notindom_eq.
    rewrite~ read_impl_notindom_eq. rewrite~ <- ED. }
Qed.

(* TODO RAMON : rename map_ext with read_extens *)

End MapLemma.

(* TODO RAMON: replace in your dev "index" on maps by "indom" everywhere *)

(* renamings that will be performed in TLC *)
Definition update_update_same := update_update. 
Definition indom_update := @indom_update_of_indom. 

Lemma update_update_neq_swap : forall A i1 i2 `{Inhab B} v1 v2 (M:map A B),
  i1 <> i2 ->
  M[i1:=v1][i2:=v2] = M[i2:=v2][i1:=v1].
Proof using.
  introv IB N. applys read_extens.
  { repeat rewrite dom_update. set_prove. }
  { intros i Hi. repeat rewrite read_update.
    repeat case_if; auto. }
Qed.

Lemma indom_update_same_eq : forall A `{Inhab B} (m:map A B) (i:A) (v:B),
  i \indom (m[i:=v]) = True.
Proof using. intros. rewrite~ indom_update_eq. Qed.

Lemma indom_update_at_indom_eq : forall A i j `{IB:Inhab B} v (M:map A B),
  i \indom M ->
  j \indom (M[i:=v]) = j \indom M.
Proof using. introv IB H. extens. rewrite* indom_update_eq. iff [N|N] N; subst*. Qed.

Lemma indom_update_neq_eq : forall A i j `{IB:Inhab B} v (M:map A B),
  i <> j ->
  j \indom (M[i:=v]) = j \indom M.
Proof using. introv IB H. extens. rewrite* indom_update_eq. iff [N|N] N; subst*. Qed.

Lemma indom_update_inv_neq : forall A `{Inhab B} (m:map A B) (i j:A) (v:B),
  j \indom (m[i:=v]) ->
  i <> j ->
  j \indom m.
Proof using. introv IB H N. rewrite~ indom_update_neq_eq in H. Qed.

(* TODO RAMON: replace index_of_index_update_neq with indom_update_inv_neq *)



Hint Resolve @indom_update @indom_of_indom_update_at_indom @indom_update_same @indom_update_inv_neq.


(* ---------------------------------------------------------------------- *)
(* LibMap automation *)

Tactic Notation "rew_map" "~" "in" "*" :=
  rew_map in *; auto_tilde.
Tactic Notation "rew_map" "*" "in" "*" :=
  rew_map in *; auto_star.


Hint Rewrite @indom_update_same_eq @indom_update_eq : rew_indom.

Tactic Notation "rew_indom" :=
  autorewrite with rew_indom.
Tactic Notation "rew_indom" "in" hyp(H) :=
  autorewrite with rew_indom in H.
Tactic Notation "rew_indom" "in" "*" :=
  autorewrite_in_star_patch ltac:(fun tt => autorewrite with rew_indom).
  (* autorewrite with rew_indom in *. *)
Tactic Notation "rew_indom" "~" :=
  rew_indom; auto_tilde.
Tactic Notation "rew_indom" "*" :=
   rew_indom; auto_star.
Tactic Notation "rew_indom" "~" "in" hyp(H) :=
  rew_indom in H; auto_tilde.
Tactic Notation "rew_indom" "*" "in" hyp(H) :=
  rew_indom in H; auto_star.
Tactic Notation "rew_indom" "~" "in" "*" :=
  rew_indom in *; auto_tilde.
Tactic Notation "rew_indom" "*" "in" "*" :=
  rew_indom in *; auto_star.


Ltac prove_indom_core tt :=
  rew_indom in *.

Tactic Notation "prove_indom" :=
  prove_indom_core tt.


Definition ltac_classicT_to_destruct := classicT.

Lemma read_update_with_ltac_if : forall A `{Inhab B} (m:map A B) (i j:A) (v:B),
  (m[i:=v])[j] = (if ltac_classicT_to_destruct (i = j) then v else m[j]).
Proof using. intros. rewrite~ read_update. Qed.

Hint Rewrite @read_update_same @read_update_with_ltac_if : rew_read.

Tactic Notation "rew_read" :=
  autorewrite with rew_read.
Tactic Notation "rew_read" "in" hyp(H) :=
  autorewrite with rew_read in H.
Tactic Notation "rew_read" "in" "*" :=
  autorewrite_in_star_patch ltac:(fun tt => autorewrite with rew_read).
  (* autorewrite with rew_read in *. *)
Tactic Notation "rew_read" "~" :=
  rew_read; auto_tilde.
Tactic Notation "rew_read" "*" :=
   rew_read; auto_star.
Tactic Notation "rew_read" "~" "in" hyp(H) :=
  rew_read in H; auto_tilde.
Tactic Notation "rew_read" "*" "in" hyp(H) :=
  rew_read in H; auto_star.
Tactic Notation "rew_read" "~" "in" "*" :=
  rew_read in *; auto_tilde.
Tactic Notation "rew_read" "*" "in" "*" :=
  rew_read in *; auto_star.


Ltac rew_read_post_on P :=
  let Eq := fresh in
  destruct (ltac_classicT_to_destruct P) as [Eq|Eq];
  case_if_post Eq;
  revert Eq. (* LATER: TLC's case_if should follow the same pattern *)

Ltac rew_read_post Eq :=
  repeat match goal with 
  | |- context [ if ltac_classicT_to_destruct ?P then _ else _ ] => rew_read_post_on P
  | H: context [ if ltac_classicT_to_destruct ?P then _ else _ ] |- _ => rew_read_post_on P
  end.

Lemma demo_rew_read_post : forall (P:Prop) A (x y:A),
  (if ltac_classicT_to_destruct P then x else y) = x ->
  (if ltac_classicT_to_destruct P then x else y) = x.
Proof using.  
  intros. rew_read_post tt ;=> C. auto. auto.
Qed.

Tactic Notation "rew_reads" :=
  rew_read; rew_read_post tt.
Tactic Notation "rew_reads" "in" hyp(H) :=
  rew_read in H; rew_read_post tt.
Tactic Notation "rew_reads" "in" "*" :=
  rew_read in *; rew_read_post tt.
Tactic Notation "rew_reads" "~" :=
  rew_reads; auto_tilde.
Tactic Notation "rew_reads" "*" :=
   rew_reads; auto_star.
Tactic Notation "rew_reads" "~" "in" hyp(H) :=
  rew_reads in H; auto_tilde.
Tactic Notation "rew_reads" "*" "in" hyp(H) :=
  rew_reads in H; auto_star.
Tactic Notation "rew_reads" "~" "in" "*" :=
  rew_reads in *; auto_tilde.
Tactic Notation "rew_reads" "*" "in" "*" :=
  rew_reads in *; auto_star.


Lemma demo_rew_reads : forall A i1 i2 `{Inhab B} v1 v2 (M:map A B),
  i1 <> i2 ->
  M[i1:=v1][i2:=v2] = M[i2:=v2][i1:=v1].
Proof using.
  introv IB N. applys read_extens.
  { repeat rewrite dom_update. set_prove. }
  { intros i Hi. (* rew_reads. *) rew_reads; auto_false. } 
Qed.


(* ---------------------------------------------------------------------- *)
(* ListZ *)

Hint Rewrite @LibListZ.index_update_eq : rew_index.

Tactic Notation "rew_index" :=
  autorewrite with rew_index.
Tactic Notation "rew_index" "in" hyp(H) :=
  autorewrite with rew_index in H.
Tactic Notation "rew_index" "in" "*" :=
  autorewrite_in_star_patch ltac:(fun tt => autorewrite with rew_index).
  (* autorewrite with rew_index in *. *)
Tactic Notation "rew_index" "~" :=
  rew_index; auto_tilde.
Tactic Notation "rew_index" "*" :=
   rew_index; auto_star.
Tactic Notation "rew_index" "~" "in" hyp(H) :=
  rew_index in H; auto_tilde.
Tactic Notation "rew_index" "*" "in" hyp(H) :=
  rew_index in H; auto_star.
Tactic Notation "rew_index" "~" "in" "*" :=
  rew_index in *; auto_tilde.
Tactic Notation "rew_index" "*" "in" "*" :=
  rew_index in *; auto_star.


Hint Resolve LibListZ.index_update.

Lemma read_update_case_with_ltac_if : forall `{Inhab A} (l:list A) i j v,
  index l j ->
  l[i:=v][j] = (if ltac_classicT_to_destruct (i = j) then v else l[j]).
Proof using. intros. rewrite~ LibListZ.read_update_case. Qed.

Hint Rewrite @read_update_same @read_update_case_with_ltac_if : rew_read.


















