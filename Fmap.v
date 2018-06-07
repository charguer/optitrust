(**

This file contains a representation of finite maps
that may be used for representing a store. It also
provides lemmas and tactics for reasoning about
operations on the store (read, write, union).

Author: Arthur Charguéraud.
License: MIT.

*)

Set Implicit Arguments.
From TLC Require Import LibCore.


(* ********************************************************************** *)
(** * Maps (partial functions) *)

(* ---------------------------------------------------------------------- *)
(* ** Representation *)

(** Type of partial functions from A to B *)

Definition map (A B : Type) : Type :=
  A -> option B.


(* ---------------------------------------------------------------------- *)
(* ** Operations *)

(** Disjoint union of two partial functions *)

Definition map_union (A B : Type) (f1 f2 : map A B) : map A B :=
  fun (x:A) => match f1 x with
           | Some y => Some y
           | None => f2 x
           end.

(** Finite domain of a partial function *)

Definition map_finite (A B : Type) (f : map A B) :=
  exists (L : list A), forall (x:A), f x <> None -> mem x L.

(** Disjointness of domain of two partial functions *)

Definition map_disjoint (A B : Type) (f1 f2 : map A B) :=
  forall (x:A), f1 x = None \/ f2 x = None.

(** Compatibility of two partial functions on the intersection
    of their domains *)

Definition map_agree (A B : Type) (f1 f2 : map A B) :=
  forall x v1 v2,
  f1 x = Some v1 ->
  f2 x = Some v2 ->
  v1 = v2.


(* ---------------------------------------------------------------------- *)
(** Properties *)

Section MapOps.
Variables (A B : Type).
Implicit Types f : map A B.

(** Symmetry of disjointness *)

Lemma map_disjoint_sym :
  sym (@map_disjoint A B).
Proof using.
  introv H. unfolds map_disjoint. intros z. specializes H z. intuition.
Qed.

(** Commutativity of disjoint union *)

Lemma map_union_comm : forall f1 f2,
  map_disjoint f1 f2 ->
  map_union f1 f2 = map_union f2 f1.
Proof using.
  introv H. unfold map.
  extens. intros x. unfolds map_disjoint, map_union.
  specializes H x. cases (f1 x); cases (f2 x); auto. destruct H; false.
Qed.

(** Finiteness of union *)

Lemma map_union_finite : forall f1 f2,
  map_finite f1 ->
  map_finite f2 ->
  map_finite (map_union f1 f2).
Proof using.
  introv [L1 F1] [L2 F2]. exists (L1 ++ L2). introv M.
  specializes F1 x. specializes F2 x. unfold map_union in M.
  apply mem_app. destruct~ (f1 x).
Qed.

End MapOps.


(* ********************************************************************** *)
(** * Finite maps *)

(* ---------------------------------------------------------------------- *)
(** Definition of the type of finite maps *)

Inductive fmap (A B : Type) : Type := fmap_make {
  fmap_data :> map A B;
  fmap_finite : map_finite fmap_data }.

Arguments fmap_make [A] [B].


(* ---------------------------------------------------------------------- *)
(** Operations *)

(** Empty fmap *)

Program Definition fmap_empty (A B : Type) : fmap A B :=
  fmap_make (fun l => None) _.
Next Obligation. exists (@nil A). auto_false. Qed.

Arguments fmap_empty {A} {B}.

(** Singleton fmap *)

Program Definition fmap_single A B (x:A) (v:B) : fmap A B :=
  fmap_make (fun x' => If x = x' then Some v else None) _.
Next Obligation.
  exists (x::nil). intros. case_if. subst~.
Qed.

(** Union of fmaps *)

Program Definition fmap_union A B (h1 h2:fmap A B) : fmap A B :=
  fmap_make (map_union h1 h2) _.
Next Obligation. destruct h1. destruct h2. apply~ map_union_finite. Qed.

Notation "h1 \+ h2" := (fmap_union h1 h2)
   (at level 51, right associativity) : fmap_scope.

Open Scope fmap_scope.

(** Update of a fmap *)

Definition fmap_update A B (h:fmap A B) (x:A) (v:B) :=
  fmap_union (fmap_single x v) h.
  (* Note: the union operation first reads in the first argument. *)


(* ---------------------------------------------------------------------- *)
(** Properties *)

(** Inhabited type [fmap] *)

Global Instance Inhab_fmap A B : Inhab (fmap A B).
Proof using. intros. applys Inhab_of_val (@fmap_empty A B). Qed.

(** Compatible fmaps *)

Definition fmap_agree A B (h1 h2:fmap A B) :=
  map_agree h1 h2.

(** Disjoint fmaps *)

Definition fmap_disjoint A B (h1 h2 : fmap A B) : Prop :=
  map_disjoint h1 h2.

Notation "\# h1 h2" := (fmap_disjoint h1 h2)
  (at level 40, h1 at level 0, h2 at level 0, no associativity) : fmap_scope.

(** Three disjoint fmaps *)

Definition fmap_disjoint_3 A B (h1 h2 h3 : fmap A B) :=
     fmap_disjoint h1 h2
  /\ fmap_disjoint h2 h3
  /\ fmap_disjoint h1 h3.

Notation "\# h1 h2 h3" := (fmap_disjoint_3 h1 h2 h3)
  (at level 40, h1 at level 0, h2 at level 0, h3 at level 0, no associativity)
  : fmap_scope.



(* ********************************************************************** *)
(* * Lemmas about Fmap *)

Section FmapProp.
Variables (A B : Type).
Implicit Types f g h : fmap A B.

(* ---------------------------------------------------------------------- *)
(* ** Equality *)

Lemma fmap_make_eq : forall (f1 f2:map A B) F1 F2,
  (forall x, f1 x = f2 x) ->
  fmap_make f1 F1 = fmap_make f2 F2.
Proof using.
  introv H. asserts: (f1 = f2). { unfold map. extens~. }
  subst. fequals. (* note: involves proof irrelevance *)
Qed.

Lemma fmap_eq_inv_fmap_data_eq : forall h1 h2,
  h1 = h2 ->
  forall x, fmap_data h1 x = fmap_data h2 x.
Proof using. intros. fequals. Qed.


(* ---------------------------------------------------------------------- *)
(* ** Disjointness *)

Lemma fmap_disjoint_sym : forall h1 h2,
  \# h1 h2 ->
  \# h2 h1.
Proof using. intros [f1 F1] [f2 F2]. apply map_disjoint_sym. Qed.

Lemma fmap_disjoint_comm : forall h1 h2,
  \# h1 h2 = \# h2 h1.
Proof using. lets: fmap_disjoint_sym. extens*. Qed.

Lemma fmap_disjoint_empty_l : forall h,
  \# fmap_empty h.
Proof using. intros [f F] x. simple~. Qed.

Lemma fmap_disjoint_empty_r : forall h,
  \# h fmap_empty.
Proof using. intros [f F] x. simple~. Qed.

Hint Resolve fmap_disjoint_sym fmap_disjoint_empty_l fmap_disjoint_empty_r.

Lemma fmap_disjoint_union_eq_r : forall h1 h2 h3,
  \# h1 (h2 \+ h3) =
  (\# h1 h2 /\ \# h1 h3).
Proof using.
  intros [f1 F1] [f2 F2] [f3 F3].
  unfolds fmap_disjoint, fmap_union. simpls.
  unfolds map_disjoint, map_union. extens. iff M [M1 M2].
  split; intros x; specializes M x;
   destruct (f2 x); intuition; tryfalse.
  intros x. specializes M1 x. specializes M2 x.
   destruct (f2 x); intuition.
Qed.

Lemma fmap_disjoint_union_eq_l : forall h1 h2 h3,
  \# (h2 \+ h3) h1 =
  (\# h1 h2 /\ \# h1 h3).
Proof using.
  intros. rewrite fmap_disjoint_comm.
  apply fmap_disjoint_union_eq_r.
Qed.

Lemma fmap_disjoint_single_single : forall (x1 x2:A) (v1 v2:B),
  x1 <> x2 ->
  \# (fmap_single x1 v1) (fmap_single x2 v2).
Proof using.
  introv N. intros x. simpls. do 2 case_if; auto.
Qed.

Lemma fmap_disjoint_single_single_same_inv : forall (x:A) (v1 v2:B),
  \# (fmap_single x v1) (fmap_single x v2) ->
  False.
Proof using.
  introv D. specializes D x. simpls. case_if. destruct D; tryfalse.
Qed.

Lemma fmap_disjoint_3_unfold : forall h1 h2 h3,
  \# h1 h2 h3 = (\# h1 h2 /\ \# h2 h3 /\ \# h1 h3).
Proof using. auto. Qed.

Lemma fmap_disjoint_single_set : forall h l v1 v2,
  \# (fmap_single l v1) h ->
  \# (fmap_single l v2) h.
Proof using.
  introv M. unfolds fmap_disjoint, fmap_single, map_disjoint; simpls.
  intros l'. specializes M l'. case_if~. destruct M; auto_false.
Qed.


(* ---------------------------------------------------------------------- *)
(* ** Union *)

Lemma fmap_union_self : forall h,
  h \+ h = h.
Proof using.
  intros [f F]. apply~ fmap_make_eq. simpl. intros x.
  unfold map_union. cases~ (f x).
Qed.

Lemma fmap_union_empty_l : forall h,
  fmap_empty \+ h = h.
Proof using.
  intros [f F]. unfold fmap_union, map_union, fmap_empty. simpl.
  apply~ fmap_make_eq.
Qed.

Lemma fmap_union_empty_r : forall h,
  h \+ fmap_empty = h.
Proof using.
  intros [f F]. unfold fmap_union, map_union, fmap_empty. simpl.
  apply fmap_make_eq. intros x. destruct~ (f x).
Qed.

Lemma fmap_union_eq_empty_inv_l : forall h1 h2,
  h1 \+ h2 = fmap_empty ->
  h1 = fmap_empty.
Proof using.
  intros (f1&F1) (f2&F2) M. inverts M as M.
  applys fmap_make_eq. intros l.
  unfolds map_union.
  lets: fun_eq_1 l M.
  cases (f1 l); auto_false.
Qed.

Lemma fmap_union_eq_empty_inv_r : forall h1 h2,
  h1 \+ h2 = fmap_empty ->
  h2 = fmap_empty.
Proof using.
  intros (f1&F1) (f2&F2) M. inverts M as M.
  applys fmap_make_eq. intros l.
  unfolds map_union.
  lets: fun_eq_1 l M.
  cases (f1 l); auto_false.
Qed.

Lemma fmap_union_comm_of_disjoint : forall h1 h2,
  \# h1 h2 ->
  h1 \+ h2 = h2 \+ h1.
Proof using.
  intros [f1 F1] [f2 F2] H. simpls. apply fmap_make_eq. simpl.
  intros. rewrite~ map_union_comm.
Qed.

Lemma fmap_union_comm_of_agree : forall h1 h2,
  fmap_agree h1 h2 ->
  h1 \+ h2 = h2 \+ h1.
Proof using.
  intros [f1 F1] [f2 F2] H. simpls. apply fmap_make_eq. simpl.
  intros l. specializes H l. unfolds map_union. simpls.
  cases (f1 l); cases (f2 l); auto. fequals. applys* H.
Qed.

Lemma fmap_union_assoc : forall h1 h2 h3,
  (h1 \+ h2) \+ h3 = h1 \+ (h2 \+ h3).
Proof using.
  intros [f1 F1] [f2 F2] [f3 F3]. unfolds fmap_union. simpls.
  apply fmap_make_eq. intros x. unfold map_union. destruct~ (f1 x).
Qed.

(*
Lemma fmap_union_eq_inv_of_disjoint : forall h2 h1 h1' : fmap,
  \# h1 h2 ->
  fmap_agree h1' h2 ->
  h1 \+ h2 = h1' \+ h2 ->
  h1 = h1'.
Proof using.
  intros [f2 F2] [f1 F1] [f1' F1'] D D' E.
  applys fmap_make_eq. intros x. specializes D x. specializes D' x.
  lets E': fmap_eq_inv_fmap_data_eq (rm E) x. simpls.
  unfolds map_union.
  cases (f1 x); cases (f2 x); try solve [cases (f1' x); destruct D; congruence ].
  destruct D; try false.
  rewrite H in E'. inverts E'.
  cases (f1' x); cases (f1 x);
    destruct D; try congruence.
  false.
    destruct D'; try congruence.
Qed.
*)

Lemma fmap_union_eq_inv_of_disjoint : forall h2 h1 h1',
  \# h1 h2 ->
  \# h1' h2 ->
  h1 \+ h2 = h1' \+ h2 ->
  h1 = h1'.
Proof using.
  intros [f2 F2] [f1' F1'] [f1 F1] D D' E.
  applys fmap_make_eq. intros x. specializes D x. specializes D' x.
  lets E': fmap_eq_inv_fmap_data_eq (rm E) x. simpls.
  unfolds map_union.
  cases (f1' x); cases (f1 x);
    destruct D; try congruence;
    destruct D'; try congruence.
Qed.


(* ---------------------------------------------------------------------- *)
(* ** Compatibility *)

Lemma fmap_agree_refl : forall h,
  fmap_agree h h.
Proof using.
  intros h. introv M1 M2. congruence.
Qed.

Lemma fmap_agree_sym : forall f1 f2,
  fmap_agree f1 f2 ->
  fmap_agree f2 f1.
Proof using.
  introv M. intros l v1 v2 E1 E2.
  specializes M l E1.
Qed.

Lemma fmap_agree_of_disjoint : forall h1 h2,
  fmap_disjoint h1 h2 ->
  fmap_agree h1 h2.
Proof using.
  introv HD. intros l v1 v2 M1 M2. destruct (HD l); false.
Qed.

Lemma fmap_agree_empty_l : forall h,
  fmap_agree fmap_empty h.
Proof using. intros h l v1 v2 E1 E2. simpls. false. Qed.

Lemma fmap_agree_empty_r : forall h,
  fmap_agree h fmap_empty.
Proof using.
  hint fmap_agree_sym, fmap_agree_empty_l. eauto.
Qed.

Lemma fmap_agree_union_l : forall f1 f2 f3,
  fmap_agree f1 f3 ->
  fmap_agree f2 f3 ->
  fmap_agree (f1 \+ f2) f3.
Proof using.
  introv M1 M2. intros l v1 v2 E1 E2.
  specializes M1 l. specializes M2 l.
  simpls. unfolds map_union. cases (fmap_data f1 l).
  { inverts E1. applys* M1. }
  { applys* M2. }
Qed.

Lemma fmap_agree_union_r : forall f1 f2 f3,
  fmap_agree f1 f2 ->
  fmap_agree f1 f3 ->
  fmap_agree f1 (f2 \+ f3).
Proof using.
  hint fmap_agree_sym, fmap_agree_union_l. eauto.
Qed.

Lemma fmap_agree_union_lr : forall f1 g1 f2 g2,
  fmap_agree g1 g2 ->
  \# f1 f2 (g1 \+ g2) ->
  fmap_agree (f1 \+ g1) (f2 \+ g2).
Proof using.
  introv M1 (M2a&M2b&M2c).
  rewrite fmap_disjoint_union_eq_r in *.
  applys fmap_agree_union_l; applys fmap_agree_union_r;
    try solve [ applys* fmap_agree_of_disjoint ].
  auto.
Qed.

Lemma fmap_agree_union_ll_inv : forall f1 f2 f3,
  fmap_agree (f1 \+ f2) f3 ->
  fmap_agree f1 f3.
Proof using.
  introv M. intros l v1 v2 E1 E2.
  specializes M l. simpls. unfolds map_union.
  rewrite E1 in M. applys* M.
Qed.

Lemma fmap_agree_union_rl_inv : forall f1 f2 f3,
  fmap_agree f1 (f2 \+ f3) ->
  fmap_agree f1 f2.
Proof using.
  hint fmap_agree_union_ll_inv, fmap_agree_sym. eauto.
Qed.

Lemma fmap_agree_union_lr_inv_agree_agree : forall f1 f2 f3,
  fmap_agree (f1 \+ f2) f3 ->
  fmap_agree f1 f2 ->
  fmap_agree f2 f3.
Proof using.
  introv M D. rewrite~ (@fmap_union_comm_of_agree f1 f2) in M.
  applys* fmap_agree_union_ll_inv.
Qed.

Lemma fmap_agree_union_rr_inv_agree : forall f1 f2 f3,
  fmap_agree f1 (f2 \+ f3) ->
  fmap_agree f2 f3 ->
  fmap_agree f1 f3.
Proof using.
  hint fmap_agree_union_lr_inv_agree_agree, fmap_agree_sym. eauto.
Qed.

Lemma fmap_agree_union_l_inv : forall f1 f2 f3,
  fmap_agree (f1 \+ f2) f3 ->
  fmap_agree f1 f2 ->
     fmap_agree f1 f3
  /\ fmap_agree f2 f3.
Proof using.
  (* LATER: proofs redundant with others above *)
  introv M2 M1. split.
  { intros l v1 v2 E1 E2.
    specializes M1 l v1 v2 E1. applys~ M2 l v1 v2.
    unfold fmap_union, map_union; simpl. rewrite~ E1. }
  { intros l v1 v2 E1 E2.
    specializes M1 l. specializes M2 l.
    unfolds fmap_union, map_union; simpls.
    cases (fmap_data f1 l). (* LATER: name b *)
    { applys eq_trans b. symmetry. applys~ M1. applys~ M2. }
    { auto. } }
Qed.

Lemma fmap_agree_union_r_inv : forall f1 f2 f3,
  fmap_agree f1 (f2 \+ f3) ->
  fmap_agree f2 f3 ->
     fmap_agree f1 f2
  /\ fmap_agree f1 f3.
Proof using.
  hint fmap_agree_sym.
  intros. forwards~ (M1&M2): fmap_agree_union_l_inv f2 f3 f1.
Qed.


(* ---------------------------------------------------------------------- *)
(* ** Read and write *)

Lemma fmap_union_single_l_read : forall f1 f2 l v,
  f1 = fmap_single l v ->
  fmap_data (f1 \+ f2) l = Some v.
Proof using.
  intros. subst. simpl. unfold map_union. case_if~.
Qed.

Lemma fmap_union_single_to_update : forall f1 f1' f2 l v v',
  f1 = fmap_single l v ->
  f1' = fmap_single l v' ->
  (f1' \+ f2) = fmap_update (f1 \+ f2) l v'.
Proof using.
  intros. subst. unfold fmap_update.
  rewrite <- fmap_union_assoc. fequals.
  applys fmap_make_eq. intros l'.
  unfolds map_union, fmap_single; simpl. case_if~.
Qed.

End FmapProp.

Arguments fmap_union_assoc [A] [B].
Arguments fmap_union_comm_of_disjoint [A] [B].
Arguments fmap_union_comm_of_agree [A] [B].


(* ********************************************************************** *)
(* * Tactics *)

(* ---------------------------------------------------------------------- *)
(* ** Tactic [fmap_disjoint] for proving disjointness results *)

(** [fmap_disjoint] proves goals of the form [\# h1 h2] and
    [\# h1 h2 h3] by expanding all hypotheses into binary forms
    [\# h1 h2] and then exploiting symmetry and disjointness
    with [fmap_empty]. *)

Hint Resolve fmap_disjoint_sym fmap_disjoint_empty_l fmap_disjoint_empty_r.

Hint Rewrite
  fmap_disjoint_union_eq_l
  fmap_disjoint_union_eq_r
  fmap_disjoint_3_unfold : rew_disjoint.

Tactic Notation "rew_disjoint" :=
  autorewrite with rew_disjoint in *.
Tactic Notation "rew_disjoint" "*" :=
  rew_disjoint; auto_star.

Tactic Notation "fmap_disjoint" :=
  solve [ subst; rew_disjoint; jauto_set; auto ].

Tactic Notation "fmap_disjoint_if_needed" :=
  match goal with
  | |- \# _ _ => fmap_disjoint
  | |- \# _ _ _ => fmap_disjoint
  end.

Lemma fmap_disjoint_demo : forall A B (h1 h2 h3 h4 h5:fmap A B),
  h1 = h2 \+ h3 ->
  \# h2 h3 ->
  \# h1 h4 h5 ->
  \# h3 h2 h5 /\ \# h4 h5.
Proof using.
  intros. dup 2.
  { subst. rew_disjoint. jauto_set. auto. auto. auto. auto. }
  { fmap_disjoint. }
Qed.


(* ---------------------------------------------------------------------- *)
(* ** Tactic [fmap_eq] for proving equality of fmaps,
      and tactic [rew_fmap] to normalize fmap expressions. *)

Section StateEq.
Variables (A B : Type).
Implicit Types h : fmap A B.

(** [fmap_eq] proves equalities between unions of fmaps, of the form
    [h1 \+ h2 \+ h3 \+ h4 = h1' \+ h2' \+ h3' \+ h4']
    It attempts to discharge the disjointness side-conditions.
    Disclaimer: it cancels heaps at depth up to 4, but no more. *)

Lemma fmap_union_eq_cancel_1 : forall h1 h2 h2',
  h2 = h2' ->
  h1 \+ h2 = h1 \+ h2'.
Proof using. intros. subst. auto. Qed.

Lemma fmap_union_eq_cancel_1' : forall h1,
  h1 = h1.
Proof using. intros. auto. Qed.

Lemma fmap_union_eq_cancel_2 : forall h1 h1' h2 h2',
  \# h1 h1' ->
  h2 = h1' \+ h2' ->
  h1 \+ h2 = h1' \+ h1 \+ h2'.
Proof using.
  intros. subst. rewrite <- fmap_union_assoc.
  rewrite (fmap_union_comm_of_disjoint h1 h1').
  rewrite~ fmap_union_assoc. auto.
Qed.

Lemma fmap_union_eq_cancel_2' : forall h1 h1' h2,
  \# h1 h1' ->
  h2 = h1' ->
  h1 \+ h2 = h1' \+ h1.
Proof using.
  intros. subst. apply~ fmap_union_comm_of_disjoint.
Qed.

Lemma fmap_union_eq_cancel_3 : forall h1 h1' h2 h2' h3',
  \# h1 (h1' \+ h2') ->
  h2 = h1' \+ h2' \+ h3' ->
  h1 \+ h2 = h1' \+ h2' \+ h1 \+ h3'.
Proof using.
  intros. subst.
  rewrite <- (fmap_union_assoc h1' h2' h3').
  rewrite <- (fmap_union_assoc h1' h2' (h1 \+ h3')).
  apply~ fmap_union_eq_cancel_2.
Qed.

Lemma fmap_union_eq_cancel_3' : forall h1 h1' h2 h2',
  \# h1 (h1' \+ h2') ->
  h2 = h1' \+ h2' ->
  h1 \+ h2 = h1' \+ h2' \+ h1.
Proof using.
  intros. subst.
  rewrite <- (fmap_union_assoc h1' h2' h1).
  apply~ fmap_union_eq_cancel_2'.
Qed.

Lemma fmap_union_eq_cancel_4 : forall h1 h1' h2 h2' h3' h4',
  \# h1 ((h1' \+ h2') \+ h3') ->
  h2 = h1' \+ h2' \+ h3' \+ h4' ->
  h1 \+ h2 = h1' \+ h2' \+ h3' \+ h1 \+ h4'.
Proof using.
  intros. subst.
  rewrite <- (fmap_union_assoc h1' h2' (h3' \+ h4')).
  rewrite <- (fmap_union_assoc h1' h2' (h3' \+ h1 \+ h4')).
  apply~ fmap_union_eq_cancel_3.
Qed.

Lemma fmap_union_eq_cancel_4' : forall h1 h1' h2 h2' h3',
  \# h1 (h1' \+ h2' \+ h3') ->
  h2 = h1' \+ h2' \+ h3' ->
  h1 \+ h2 = h1' \+ h2' \+ h3' \+ h1.
Proof using.
  intros. subst.
  rewrite <- (fmap_union_assoc h2' h3' h1).
  apply~ fmap_union_eq_cancel_3'.
Qed.

End StateEq.

Hint Rewrite
  fmap_union_assoc
  fmap_union_empty_l
  fmap_union_empty_r : rew_fmap.

Tactic Notation "rew_fmap" :=
  autorewrite with rew_fmap in *.

Tactic Notation "rew_fmap" "~" :=
  rew_fmap; auto_tilde.

Tactic Notation "rew_fmap" "*" :=
  rew_fmap; auto_star.

Ltac fmap_eq_step tt :=
  match goal with | |- ?G => match G with
  | ?h1 = ?h1 => apply fmap_union_eq_cancel_1'
  | ?h1 \+ ?h2 = ?h1 \+ ?h2' => apply fmap_union_eq_cancel_1
  | ?h1 \+ ?h2 = ?h1' \+ ?h1 => apply fmap_union_eq_cancel_2'
  | ?h1 \+ ?h2 = ?h1' \+ ?h1 \+ ?h2' => apply fmap_union_eq_cancel_2
  | ?h1 \+ ?h2 = ?h1' \+ ?h2' \+ ?h1 => apply fmap_union_eq_cancel_3'
  | ?h1 \+ ?h2 = ?h1' \+ ?h2' \+ ?h1 \+ ?h3' => apply fmap_union_eq_cancel_3
  | ?h1 \+ ?h2 = ?h1' \+ ?h2' \+ ?h3' \+ ?h1 => apply fmap_union_eq_cancel_4'
  | ?h1 \+ ?h2 = ?h1' \+ ?h2' \+ ?h3' \+ ?h1 \+ ?h4' => apply fmap_union_eq_cancel_4
  end end.

Tactic Notation "fmap_eq" :=
  subst;
  rew_fmap;
  repeat (fmap_eq_step tt);
  try fmap_disjoint_if_needed.

Tactic Notation "fmap_eq" "~" :=
  fmap_eq; auto_tilde.

Tactic Notation "fmap_eq" "*" :=
  fmap_eq; auto_star.

Lemma fmap_eq_demo : forall A B (h1 h2 h3 h4 h5:fmap A B),
  \# h1 h2 h3 ->
  \# (h1 \+ h2 \+ h3) h4 h5 ->
  h1 = h2 \+ h3 ->
  h4 \+ h1 \+ h5 = h2 \+ h5 \+ h4 \+ h3.
Proof using.
  intros. dup 2.
  { subst. rew_fmap.
    fmap_eq_step tt. fmap_disjoint.
    fmap_eq_step tt.
    fmap_eq_step tt. fmap_disjoint. auto. }
  { fmap_eq. }
Qed.


(* ---------------------------------------------------------------------- *)
(* ** Tactic [fmap_red] for proving [red] goals
      (reduction according to a big-step semantics)
      modulo equalities between fmaps *)

(** [fmap_red] proves a goal of the form [red h1 t h2 v]
    using an hypothesis of the shape [red h1' t h2' v],
    generating [h1 = h1'] and [h2 = h2'] as subgoals, and
    attempting to solve them using the tactic [fmap_eq].
    The tactic should be configured depending on [red].
    For example:

       Ltac fmap_red_base tt :=
        match goal with H: red _ ?t _ _ |- red _ ?t _ _ =>
          applys_eq H 2 4; try fmap_eq end.

    The default implementation is a dummy one.
*)

Ltac fmap_red_base tt := fail.

Tactic Notation "fmap_red" :=
  fmap_red_base tt.

Tactic Notation "fmap_red" "~" :=
  fmap_red; auto_tilde.

Tactic Notation "fmap_red" "*" :=
  fmap_red; auto_star.


(* ********************************************************************** *)
(** * Consecutive locations and fresh locations *)

(* ---------------------------------------------------------------------- *)
(** ** Existence of fresh locations *)

Fixpoint fmap_conseq (B:Type) (l:nat) (k:nat) (v:B) : fmap nat B :=
  match k with
  | O => fmap_empty
  | S k' => (fmap_single l v) \+ (fmap_conseq (S l) k' v)
  end.

Lemma fmap_conseq_zero : forall B (l:nat) (v:B),
  fmap_conseq l O v = fmap_empty.
Proof using. auto. Qed.

Lemma fmap_conseq_succ : forall B (l:nat) (k:nat) (v:B),
  fmap_conseq l (S k) v = (fmap_single l v) \+ (fmap_conseq (S l) k v).
Proof using. auto. Qed.

Opaque fmap_conseq.


(* ---------------------------------------------------------------------- *)
(** ** Existence of fresh locations *)

(** These lemmas are useful to prove:
    [forall h v, exists l, fmap_disjoint (fmap_single l v) h]. *)

Definition loc_fresh_gen (L : list nat) :=
  (1 + fold_right plus O L)%nat.

Lemma loc_fresh_ind : forall l L,
  mem l L ->
  (l < loc_fresh_gen L)%nat.
Proof using.
  intros l L. unfold loc_fresh_gen.
  induction L; introv M; inverts M; rew_listx.
  { math. }
  { forwards~: IHL. math. }
Qed.

Lemma loc_fresh_nat_ge : forall (L:list nat),
  exists (l:nat), forall (i:nat), ~ mem (l+i)%nat L.
Proof using.
  intros L. exists (loc_fresh_gen L). intros i M.
  lets: loc_fresh_ind M. math.
Qed.

Lemma loc_fresh_nat : forall (L:list nat),
  exists (l:nat), ~ mem l L.
Proof using.
  intros L. forwards (l&P): loc_fresh_nat_ge L.
  exists l. intros M. applys (P 0%nat). applys_eq M 2. math.
Qed.


(* ---------------------------------------------------------------------- *)
(** ** Extension of a number of consecutive fresh locations *)

Section FmapFresh.
Variables (B : Type).
Implicit Types h : fmap nat B.

Lemma fmap_single_fresh : forall null h v,
  exists l, \# (fmap_single l v) h /\ l <> null.
Proof using.
  intros null (m&(L&M)) v.
  unfold fmap_disjoint, map_disjoint. simpl.
  lets (l&F): (loc_fresh_nat (null::L)).
  exists l. split.
  { intros l'. case_if~. (* --LATER: fix TLC substitution in case_if *)
    { subst. right. applys not_not_inv. intros H. applys F.
      constructor. applys~ M. } }
  { intro_subst. applys~ F. }
Qed.

Lemma fmap_conseq_fresh : forall null h k v,
  exists l, \# (fmap_conseq l k v) h /\ l <> null.
Proof using.
  intros null (m&(L&M)) k v.
  unfold fmap_disjoint, map_disjoint. simpl.
  lets (l&F): (loc_fresh_nat_ge (null::L)).
  exists l. split.
  { intros l'. gen l. induction k; intros.
    { simple~. }
    { rewrite fmap_conseq_succ.
      destruct (IHk (S l)%nat) as [E|?].
      { intros i N. applys F (S i). applys_eq N 2. math. }
      { simpl. unfold map_union. case_if~.
        { subst. right. applys not_not_inv. intros H. applys F 0%nat.
          constructor. math_rewrite (l'+0 = l')%nat. applys~ M. } }
      { auto. } } }
  { intro_subst. applys~ F 0%nat. rew_nat. auto. }
Qed.

Lemma fmap_disjoint_single_conseq : forall B l l' k (v:B),
  (l < l')%nat \/ (l >= l'+k)%nat ->
  \# (fmap_single l v) (fmap_conseq l' k v).
Proof using.
  introv N. gen l'. induction k; intros.
  { rewrite~ fmap_conseq_zero. }
  { rewrite fmap_conseq_succ. rew_disjoint. split.
    { applys fmap_disjoint_single_single. destruct N; math. }
    { applys IHk. destruct N. { left; math. } { right; math. } } }
Qed.

End FmapFresh.
