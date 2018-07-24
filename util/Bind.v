(**

This file describes the representation of variables, binders, and
contexts.

Author: Arthur Charguéraud.
License: MIT.

*)

Set Implicit Arguments.
Require Export LibString LibList LibCore.
Require Export TLCbuffer MyLibVar.
Open Scope string_scope.


(* ********************************************************************** *)
(* * Variables *)

(** Tactic [var_neq] for proving two concrete variables distinct.
    Also registered as hint for [auto] *)

Ltac var_neq :=
  match goal with |- ?x <> ?y :> var =>
    solve [ let E := fresh in
            destruct (String.string_dec x y) as [E|E];
              [ false | apply E ] ] end.

Hint Extern 1 (?x <> ?y) => var_neq.


(* ---------------------------------------------------------------------- *)
(** Distinct variables *)

(** [vars] is the type of a list of variables *)

Definition vars : Type := list var.

(** [var_fresh y xs] asserts that [y] does not belong to the list [xs] *)

Fixpoint var_fresh (y:var) (xs:vars) : bool :=
  match xs with
  | nil => true
  | x::xs' => if var_eq x y then false else var_fresh y xs'
  end.

(** [var_distinct xs] asserts that [xs] consists of a list of distinct variables. *)

Fixpoint var_distinct (xs:vars) : Prop :=
  match xs with
  | nil => True
  | x::xs' => var_fresh x xs' /\ var_distinct xs'
  end.

(** Computable version of [var_distinct] *)

Fixpoint var_distinct_exec (xs:vars) : bool :=
  match xs with
  | nil => true
  | x::xs' => var_fresh x xs' && var_distinct_exec xs'
  end.

Lemma var_distinct_exec_eq : forall xs,
  var_distinct_exec xs = isTrue (var_distinct xs).
Proof using.
  intros. induction xs as [|x xs']; simpl; rew_isTrue.
  { auto. } { rewrite~ IHxs'. }
Qed.


(* ---------------------------------------------------------------------- *)
(** Nonempty lists of [n] distinct variables *)

(** [var_funs n xs] asserts that [xs] consists of [n] distinct variables *)

Definition var_funs (n:nat) (xs:vars) : Prop :=
     var_distinct xs
  /\ length xs = n
  /\ xs <> nil.

(** [var_fixs f n xs] asserts that [f::xs] consists of [n+1]
    distinct variables. *)

Definition var_fixs (f:var) (n:nat) (xs:vars) : Prop :=
     var_distinct (f::xs)
  /\ length xs = n
  /\ xs <> nil.

(** Computable version of [var_funs] *)

Definition var_funs_exec (n:nat) (xs:vars) : bool :=
     nat_compare n (List.length xs)
  && is_not_nil xs
  && var_distinct_exec xs.

Lemma var_funs_exec_eq : forall (n:nat) xs,
  var_funs_exec n xs = isTrue (var_funs n xs).
Proof using.
  intros. unfold var_funs_exec, var_funs.
  rewrite nat_compare_eq.
  rewrite is_not_nil_eq.
  rewrite List_length_eq.
  rewrite var_distinct_exec_eq.
  extens. rew_istrue. iff*.
Qed.

(** Computable version of [var_fixs] *)

Definition var_fixs_exec (f:var) (n:nat) (xs:vars) : bool :=
     nat_compare n (List.length xs)
  && is_not_nil xs
  && var_distinct_exec (f::xs).

Lemma var_fixs_exec_eq : forall f (n:nat) xs,
  var_fixs_exec f n xs = isTrue (var_fixs f n xs).
Proof using.
  intros. unfold var_fixs_exec, var_fixs.
  rewrite nat_compare_eq.
  rewrite is_not_nil_eq.
  rewrite List_length_eq.
  rewrite var_distinct_exec_eq.
  extens. rew_istrue. iff*.
Qed.


(* ---------------------------------------------------------------------- *)
(* ** Generation of n variables *)

(** [nat_to_var n] converts [nat] values into distinct
    [name] values.
    Warning: the current implementation is inefficient. *)

Definition dummy_char := Ascii.ascii_of_nat 0%nat.

Fixpoint nat_to_var (n:nat) : var :=
  match n with
  | O => String.EmptyString
  | S n' => String.String dummy_char (nat_to_var n')
  end.

Lemma injective_nat_to_var : injective nat_to_var.
Proof using.
  intros n. induction n as [|n']; intros m E; destruct m as [|m']; tryfalse.
  { auto. }
  { inverts E. fequals~. }
Qed.

(** [var_seq i n] generates a list of variables [x1;x2;..;xn]
    with [x1=i] and [xn=i+n-1]. Such lists are useful for
    generic programming. *)

Fixpoint var_seq (start:nat) (nb:nat) : vars :=
  match nb with
  | O => nil
  | S nb' => (nat_to_var start) :: var_seq (S start) nb'
  end.

(** Properties of [var_seq] follow *)

Section Var_seq.
Implicit Types start nb : nat.

Lemma var_fresh_var_seq_lt : forall (x:nat) start nb,
  (x < start)%nat ->
  var_fresh (nat_to_var x) (var_seq start nb).
Proof using.
  intros. gen start. induction nb; intros.
  { auto. }
  { simpl. rewrite var_eq_spec. case_if.
    { lets: injective_nat_to_var C. math. }
    { applys IHnb. math. } }
Qed.

Lemma var_fresh_var_seq_ge : forall (x:nat) start nb,
  (x >= start+nb)%nat ->
  var_fresh (nat_to_var x) (var_seq start nb).
Proof using.
  intros. gen start. induction nb; intros.
  { auto. }
  { simpl. rewrite var_eq_spec. case_if.
    { lets: injective_nat_to_var C. math. }
    { applys IHnb. math. } }
Qed.

Lemma var_distinct_var_seq : forall start nb,
  var_distinct (var_seq start nb).
Proof using.
  intros. gen start. induction nb; intros.
  { simple~. }
  { split.
    { applys var_fresh_var_seq_lt. math. }
    { auto. } }
Qed.

Lemma length_var_seq : forall start nb,
  length (var_seq start nb) = nb.
Proof using.
  intros. gen start. induction nb; simpl; intros.
  { auto. } { rew_list. rewrite~ IHnb. }
Qed.

Lemma var_funs_var_seq : forall start nb,
  (nb > 0%nat)%nat ->
  var_funs nb (var_seq start nb).
Proof using.
  introv E. splits.
  { applys var_distinct_var_seq. }
  { applys length_var_seq. }
  { destruct nb. { false. math. } { simpl. auto_false. } }
Qed.

End Var_seq.



(* ********************************************************************** *)
(* * Binders *)

(** A binder is either a variable or the anonymous binder. *)

Inductive bind : Type :=
  | bind_anon : bind
  | bind_var : var -> bind.

(** Variables and strinsg can be coerced to binders. *)

Coercion bind_var : var >-> bind.
Coercion bind_var' (x:string) : bind := bind_var x.

(** Comparison between two bindings *)

Definition bind_eq (z1 z2 : bind) : bool :=
  match z1, z2 with
  | bind_anon, bind_anon => true
  | bind_var x1, bind_var x2 => var_eq x1 x2
  | _, _ => false
  end.


(* ********************************************************************** *)
(* * Contexts *)

Module Ctx.

(* ---------------------------------------------------------------------- *)
(** Definition of contexts *)

(** A context associate variables with values. *)

(** [ctx] describes a list of bindings *)

Definition ctx (A:Type) : Type :=
  list (var * A).

(* ---------------------------------------------------------------------- *)
(** Operations of contexts *)

(** [empty] describes the empty context *)

Definition empty A : ctx A :=
  @nil _.

Arguments empty {A}.

(** [add z v E] is defined as:
    - [E] if [z] is the anonymous binder
    - [E] extended with the pair [(x,v)] if [z] is a variable [x]. *)

Definition add A (z:bind) (v:A) (E:ctx A) : ctx A :=
  match z with
  | bind_anon => E
  | bind_var x => (x,v)::E
  end.

(** [one z v] is a shorthand for [add z v empty] *)

Definition one A (z:bind) (v:A) : ctx A :=
  add z v empty.

(** [rem x E] removes all bindings on [x] stored in [E] *)

Fixpoint rem_var A (x:var) (E:ctx A) : ctx A :=
  match E with
  | nil => nil
  | (y,v)::E' =>
      let E'' := rem_var x E' in
      if var_eq x y then E'' else (y,v)::E''
  end.

Fixpoint rem A (z:bind) (E:ctx A) : ctx A :=
  match z with
  | bind_anon => E
  | bind_var x => rem_var x E
  end.

(** [lookup x E] returns
    - [None] if [x] is not bound in [E]
    - [Some v] if [x] is bound to [v] in [E]. *)

Fixpoint lookup A (x:var) (E:ctx A) : option A :=
  match E with
  | nil => None
  | (y,v)::E' => if var_eq x y
                   then Some v
                   else lookup x E'
  end.

(** [fresh x E] asserts that [x] is not bound in [E] *)

Definition fresh A (x:var) (E:ctx A) : Prop :=
  lookup x E = None.


(* ---------------------------------------------------------------------- *)
(** Properties of operations on contexts *)

Section CtxOps.
Variables (A : Type).
Implicit Type (E : ctx A).


(* ---------------------------------------------------------------------- *)
(** Lemmas to reintroduce contexts *)

Lemma nil_eq_ctx_empty :
  @nil (prod var A) = empty.
Proof using. auto. Qed.

Lemma cons_eq_ctx_add : forall x v E,
  (x,v)::E = add x v E.
Proof using. auto. Qed.


(* ---------------------------------------------------------------------- *)
(** Properties of operations on contexts  *)

Lemma fresh_inv : forall (x1 x2:var) v E,
  fresh x1 (add x2 v E) ->
  x1 <> x2 /\ fresh x1 E.
Proof using.
  introv M. unfolds fresh. simpls.
  rewrite var_eq_spec in *. case_if~.
Qed.

(** [rem x empty] *)

Lemma rem_empty : forall z,
  rem z (@empty A) = empty.
Proof using. intros. destruct z as [|x]; auto. Qed.

(** Result of removing a variable from a context. *)
(* -- TODO: rename and check useful *)

Lemma rem_add_same : forall z v E,
  rem z (add z v E) = rem z E.
Proof using.
  intros. destruct z as [|x].
  { auto. }
  { simpl. rewrite var_eq_spec. case_if~. }
Qed.

Lemma rem_add_neq : forall z1 z2 v E,
  z1 <> z2 ->
  rem z1 (add z2 v E) = add z2 v (rem z1 E).
Proof using.
  intros. destruct z2 as [|x2].
  { auto. }
  { destruct z1 as [|x1].
    { auto. }
    { simpl. rewrite var_eq_spec. case_if~. } }
Qed.

(** Removing a variable that does not occur in a context changes nothing. *)

Lemma rem_fresh : forall x E,
  fresh x E ->
  rem x E = E.
Proof using.
  introv M. unfold rem. induction E as [|(y,v) E'].
  { auto. }
  { simpls. lets (N1&N2): fresh_inv (rm M).
    rewrite var_eq_spec in *. case_if. rewrite~ IHE'. }
Qed.

End CtxOps.

End Ctx.

(* LATER: how to place this rewrite base and tactics inside the
   module and still be able to use it without importing the module? *)

Hint Rewrite Ctx.nil_eq_ctx_empty Ctx.cons_eq_ctx_add : rew_ctx.

Tactic Notation "rew_ctx" :=
  autorewrite with rew_ctx.
Tactic Notation "rew_ctx" "in" hyp(H) :=
  autorewrite with rew_ctx in H.
Tactic Notation "rew_ctx" "in" "*" :=
  autorewrite with rew_ctx in *.



(* ********************************************************************** *)
(* * Notation for program variables *)

(** To avoid using the string notation ["x"] for refering to a
    variable called [x], one can use the notation ['x], available
    by importing the following module. *)


Module NotationForVariables.

Notation "''a'" := ("a":var) : var_scope.
Notation "''b'" := ("b":var) : var_scope.
Notation "''c'" := ("c":var) : var_scope.
Notation "''d'" := ("d":var) : var_scope.
Notation "''e'" := ("e":var) : var_scope.
Notation "''f'" := ("f":var) : var_scope.
Notation "''g'" := ("g":var) : var_scope.
Notation "''h'" := ("h":var) : var_scope.
Notation "''i'" := ("i":var) : var_scope.
Notation "''j'" := ("j":var) : var_scope.
Notation "''k'" := ("k":var) : var_scope.
Notation "''l'" := ("l":var) : var_scope.
Notation "''m'" := ("m":var) : var_scope.
Notation "''n'" := ("n":var) : var_scope.
Notation "''o'" := ("o":var) : var_scope.
Notation "''p'" := ("p":var) : var_scope.
Notation "''q'" := ("q":var) : var_scope.
Notation "''r'" := ("r":var) : var_scope.
Notation "''s'" := ("s":var) : var_scope.
Notation "''t'" := ("t":var) : var_scope.
Notation "''u'" := ("u":var) : var_scope.
Notation "''v'" := ("v":var) : var_scope.
Notation "''w'" := ("w":var) : var_scope.
Notation "''x'" := ("x":var) : var_scope.
Notation "''y'" := ("y":var) : var_scope.
Notation "''z'" := ("z":var) : var_scope.

Notation "''a1'" := ("a1":var) : var_scope.
Notation "''b1'" := ("b1":var) : var_scope.
Notation "''c1'" := ("c1":var) : var_scope.
Notation "''d1'" := ("d1":var) : var_scope.
Notation "''e1'" := ("e1":var) : var_scope.
Notation "''f1'" := ("f1":var) : var_scope.
Notation "''g1'" := ("g1":var) : var_scope.
Notation "''h1'" := ("h1":var) : var_scope.
Notation "''i1'" := ("i1":var) : var_scope.
Notation "''j1'" := ("j1":var) : var_scope.
Notation "''k1'" := ("k1":var) : var_scope.
Notation "''l1'" := ("l1":var) : var_scope.
Notation "''m1'" := ("m1":var) : var_scope.
Notation "''n1'" := ("n1":var) : var_scope.
Notation "''o1'" := ("o1":var) : var_scope.
Notation "''p1'" := ("p1":var) : var_scope.
Notation "''q1'" := ("q1":var) : var_scope.
Notation "''r1'" := ("r1":var) : var_scope.
Notation "''s1'" := ("s1":var) : var_scope.
Notation "''t1'" := ("t1":var) : var_scope.
Notation "''u1'" := ("u1":var) : var_scope.
Notation "''v1'" := ("v1":var) : var_scope.
Notation "''w1'" := ("w1":var) : var_scope.
Notation "''x1'" := ("x1":var) : var_scope.
Notation "''y1'" := ("y1":var) : var_scope.
Notation "''z1'" := ("z1":var) : var_scope.

Notation "''a2'" := ("a2":var) : var_scope.
Notation "''b2'" := ("b2":var) : var_scope.
Notation "''c2'" := ("c2":var) : var_scope.
Notation "''d2'" := ("d2":var) : var_scope.
Notation "''e2'" := ("e2":var) : var_scope.
Notation "''f2'" := ("f2":var) : var_scope.
Notation "''g2'" := ("g2":var) : var_scope.
Notation "''h2'" := ("h2":var) : var_scope.
Notation "''i2'" := ("i2":var) : var_scope.
Notation "''j2'" := ("j2":var) : var_scope.
Notation "''k2'" := ("k2":var) : var_scope.
Notation "''l2'" := ("l2":var) : var_scope.
Notation "''m2'" := ("m2":var) : var_scope.
Notation "''n2'" := ("n2":var) : var_scope.
Notation "''o2'" := ("o2":var) : var_scope.
Notation "''p2'" := ("p2":var) : var_scope.
Notation "''q2'" := ("q2":var) : var_scope.
Notation "''r2'" := ("r2":var) : var_scope.
Notation "''s2'" := ("s2":var) : var_scope.
Notation "''t2'" := ("t2":var) : var_scope.
Notation "''u2'" := ("u2":var) : var_scope.
Notation "''v2'" := ("v2":var) : var_scope.
Notation "''w2'" := ("w2":var) : var_scope.
Notation "''x2'" := ("x2":var) : var_scope.
Notation "''y2'" := ("y2":var) : var_scope.
Notation "''z2'" := ("z2":var) : var_scope.

Notation "''a3'" := ("a3":var) : var_scope.
Notation "''b3'" := ("b3":var) : var_scope.
Notation "''c3'" := ("c3":var) : var_scope.
Notation "''d3'" := ("d3":var) : var_scope.
Notation "''e3'" := ("e3":var) : var_scope.
Notation "''f3'" := ("f3":var) : var_scope.
Notation "''g3'" := ("g3":var) : var_scope.
Notation "''h3'" := ("h3":var) : var_scope.
Notation "''i3'" := ("i3":var) : var_scope.
Notation "''j3'" := ("j3":var) : var_scope.
Notation "''k3'" := ("k3":var) : var_scope.
Notation "''l3'" := ("l3":var) : var_scope.
Notation "''m3'" := ("m3":var) : var_scope.
Notation "''n3'" := ("n3":var) : var_scope.
Notation "''o3'" := ("o3":var) : var_scope.
Notation "''p3'" := ("p3":var) : var_scope.
Notation "''q3'" := ("q3":var) : var_scope.
Notation "''r3'" := ("r3":var) : var_scope.
Notation "''s3'" := ("s3":var) : var_scope.
Notation "''t3'" := ("t3":var) : var_scope.
Notation "''u3'" := ("u3":var) : var_scope.
Notation "''v3'" := ("v3":var) : var_scope.
Notation "''w3'" := ("w3":var) : var_scope.
Notation "''x3'" := ("x3":var) : var_scope.
Notation "''y3'" := ("y3":var) : var_scope.
Notation "''z3'" := ("z3":var) : var_scope.

Open Scope var_scope.

End NotationForVariables.
