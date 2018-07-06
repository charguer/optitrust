(**

This file contains temporary definitions that will eventually
get merged into the various files from the TLC library.

Author: Arthur Charguéraud.
License: MIT.

*)


Set Implicit Arguments.
Require Import LibTactics LibLogic LibList LibReflect.
Require LibListZ.
Generalizable Variables A B.



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


(* ---------------------------------------------------------------------- *)
(* Set *)

Require Import LibSet.

Tactic Notation "rew_set" "~" := 
  rew_set; auto_tilde.
Tactic Notation "rew_set" "*" := 
  rew_set; auto_star.








