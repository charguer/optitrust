(**

This file describes the syntax and semantics of a lambda calculus
with mutable heap. The language includes recursive functions, and a
couple of primitive functions. Records and arrays operations are
encoded using pointer arithmetics, and using the [alloc] operation
which allocates at once a requested number of consecutive memory cells.

Author: Arthur Charguéraud.
License: MIT.

*)

Set Implicit Arguments.
Require Export LibString LibList LibCore.
Require Export Fmap Bind TLCbuffer.
Open Scope string_scope.


(* ********************************************************************** *)
(* * Source language syntax *)


(* ---------------------------------------------------------------------- *)
(** Representation of locations and fields *)

Definition loc := nat.

Definition null : loc := 0%nat.

Definition field := nat.

Global Opaque field loc.


(* ---------------------------------------------------------------------- *)
(** Grammar of types *)

Inductive typ : Type :=
  | typ_int : typ
  | typ_var : var -> typ.

Inductive typdef : Type :=
  | typ_struct : list (typ * field) -> typdef.

Definition typsize := nat.

Definition typctx := fmap var (typsize * typdef).

Definition sizeof (c:typctx) (T:typ) : typsize :=
  match T with
  | typ_int => 1%nat
  | typ_var x =>
    match fmap_data c x with
    | Some v => fst v
    | None => 0%nat
    end
  end.


(* ---------------------------------------------------------------------- *)
(** Syntax of the source language *)

Inductive prim : Type :=
  | val_ref : prim
  | val_get : prim
  | val_set : prim
  | val_alloc : prim
  | val_eq : prim
  | val_sub : prim
  | val_add : prim
  | val_ptr_add : prim.

Inductive val : Type :=
  | val_unit : val
  | val_bool : bool -> val
  | val_int : int -> val
  | val_loc : loc -> val
  | val_prim : prim -> val
  | val_fix : bind -> bind -> trm -> val

with trm : Type :=
  | trm_val : val -> trm
  | trm_var : var -> trm
  | trm_fix : bind -> bind -> trm -> trm
  | trm_if : trm -> trm -> trm -> trm
  | trm_let : bind -> trm -> trm -> trm
  | trm_app : trm -> trm -> trm
  | trm_while : trm -> trm -> trm
  | trm_for : var -> trm -> trm -> trm -> trm.

(** The type of values is inhabited *)

Global Instance Inhab_val : Inhab val.
Proof using. apply (Inhab_of_val val_unit). Qed.

(** Encoded constructs *)

Notation trm_seq := (trm_let bind_anon).
Notation trm_fun := (trm_fix bind_anon).
Notation val_fun := (val_fix bind_anon).

(** Shorthand [vars], [vals] and [trms] for lists of items. *)

Definition vals : Type := list val.
Definition trms : Type := list trm.


(* ---------------------------------------------------------------------- *)
(** Coercions *)

Coercion val_prim : prim >-> val.
Coercion val_bool : bool >-> val.
Coercion val_int : Z >-> val.
Coercion val_loc : loc >-> val.
Coercion trm_val : val >-> trm.
Coercion trm_var : var >-> trm.
Coercion trm_app : trm >-> Funclass.



(* ********************************************************************** *)
(* * Definition of capture avoiding substitution *)

(* ---------------------------------------------------------------------- *)
(** Substition of bindings in a term *)

Fixpoint subst (z:bind) (w:val) (t:trm) : trm :=
  let aux := subst z w in
  match t with
  | trm_val v => t
  | trm_var x => if bind_eq z x then w else t
  | trm_fix f z1 t1 => trm_fix f z1 (if bind_eq z z1 || bind_eq z f then t1 else aux t1)
  | trm_if t0 t1 t2 => trm_if (aux t0) (aux t1) (aux t2)
  | trm_let z1 t1 t2 => trm_let z (aux t1) (if bind_eq z z1 then t2 else aux t2)
  | trm_app t1 t2 => trm_app (aux t1) (aux t2)
  | trm_while t1 t2 => trm_while (aux t1) (aux t2)
  | trm_for x t1 t2 t3 => trm_for x (aux t1) (aux t2) (if bind_eq z x then t3 else aux t3)
  end.

Lemma subst_anon : forall v t,
  subst bind_anon v t = t.
Proof using. Admitted.



(* ********************************************************************** *)
(* * Alternative Definition of capture avoiding substitution *)

(* ---------------------------------------------------------------------- *)
(** Definition of multiple substitution. *)

(** [ctx] is the type of bindings from variables to values *)

Definition ctx := Ctx.ctx val.

(** [subst E t] substitutes all the bindings from [E] inside [t] *)

Fixpoint substx (E:ctx) (t:trm) : trm :=
  let aux := substx E in
  match t with
  | trm_val v => trm_val v
  | trm_var x => match Ctx.lookup x E with
                 | None => t
                 | Some v => v
                 end
  | trm_fix f z t1 => trm_fix f z (substx (Ctx.rem z (Ctx.rem f E)) t1)
  | trm_if t0 t1 t2 => trm_if (aux t0) (aux t1) (aux t2)
  | trm_let z t1 t2 => trm_let z (aux t1) (substx (Ctx.rem z E) t2)
  | trm_app t1 t2 => trm_app (aux t1) (aux t2)
  | trm_while t1 t2 => trm_while (aux t1) (aux t2)
  | trm_for x t1 t2 t3 => trm_for x (aux t1) (aux t2) (substx (Ctx.rem x E) t3)
  end.


(* ---------------------------------------------------------------------- *)
(** Special calls to [subst] on 1, 2, or n bindings. *)

(** [subst1 z v t] replaces occurences of binder [z] with [v] in [t]. *)

Definition subst1 (z:bind) (v:val) (t:trm) :=
  substx (Ctx.one z v) t.

(** [subst2 z1 v1 z2 v2 t] is similar. *)

Definition subst2 (z1:bind) (v1:val) (z2:bind) (v2:val) (t:trm) :=
   substx (Ctx.add z1 v1 (Ctx.one z2 v2)) t.

(** [substn xs vs t] is a shorthand for [substs (List.combine xs vs) t].
    It substitutes the values [vs] for the corresponding variables in [xs].
    This operation is only specified when [length xs = length vs]. *)

Definition substn (xs:vars) (vs:vals) (t:trm) : trm :=
  substx (LibList.combine xs vs) t.


(* ---------------------------------------------------------------------- *)
(** Lemmas about substitution *)

(** [substx] with [empty] changes nothing. *)

Lemma substx_empty : forall t,
  substx Ctx.empty t = t.
Proof using.
  intros. induction t; simpl;
   try solve [ repeat rewrite Ctx.rem_empty; fequals* ].
Qed.

(** [subst1 z v t]  returns [t] unchanged when [z] is anonymous. *)

Lemma subst1_anon : forall v t,
  subst1 bind_anon v t = t.
Proof using.
  intros. unfold subst1, Ctx.one, Ctx.add. rewrite~ substx_empty.
Qed.

(** [subst] can be combuted  by iteratively substituting its bindings. *)

Lemma substx_cons : forall x v E t,
  substx ((x,v)::E) t = substx E (subst1 x v t).
Proof using.
  intros. unfold subst1. simpl. rew_ctx. gen E.
    induction t; intros; simpl; try solve [ fequals* ].
    { rewrite var_eq_spec. case_if~. }
    { rew_ctx. fequals. tests: (b = x).
      { repeat rewrite Ctx.rem_add_same. fequals.
        rewrite Ctx.rem_empty, substx_empty. auto. }
      { repeat rewrites~ (>> Ctx.rem_add_neq b). rewrite Ctx.rem_empty.
        tests: (b0 = x).
        { repeat rewrite Ctx.rem_add_same.
          rewrite Ctx.rem_empty. rewrite~ substx_empty. }
        { repeat rewrite~ Ctx.rem_add_neq. rewrite Ctx.rem_empty.
          rewrite~ IHt. } } }
  { rew_ctx. fequals. tests: (b = x).
    { do 2 rewrite Ctx.rem_add_same. fequals. rewrite~ substx_empty. }
    { do 2 (rewrite~ Ctx.rem_add_neq). rewrite Ctx.rem_empty. rewrite~ IHt2. } }
  { admit. (* todo for loops *) }
Qed.

(** Lifting of the above lemma to handle the substitution of binders. *)

Lemma substx_add : forall z v E t,
  substx (Ctx.add z v E) t = substx E (subst1 z v t).
Proof using.
  intros. destruct z as [|x].
  { simpl. rewrite~ subst1_anon. }
  { applys~ substx_cons. }
Qed.

(** Reformulation of the definition of [subst2] *)

Lemma subst2_eq_subst1_subst1 : forall x1 x2 v1 v2 t,
  subst2 x1 v1 x2 v2 t = subst1 x2 v2 (subst1 x1 v1 t).
Proof using. intros. unfold subst2. rewrite~ substx_add. Qed.

(** Distribution of [substn] on [nil] and [cons] lists *)

Lemma substn_nil : forall t,
  substn nil nil t = t.
Proof using. intros. unfold substn. simpl. rew_ctx. apply substx_empty. Qed.

Lemma substn_cons : forall x xs v vs t,
  substn (x::xs) (v::vs) t = substn xs vs (subst1 x v t).
Proof using.
  intros. unfold substn. rewrite combine_cons. rewrite~ substx_cons.
Qed.


(* ---------------------------------------------------------------------- *)
(** Equivalence with substitution *)

Lemma substx_eq_iter_subst : forall E t,
  substx E t = LibList.fold_left (fun '(x,v) t' => subst (bind_var x) v t') t E.
Proof using.
Admitted.

Lemma subst1_eq_subst : subst1 = subst.
Proof using.
  applys fun_ext_3. intros z v t. unfold subst1. 
  rewrite substx_eq_iter_subst. unfold Ctx.one, Ctx.add.
  destruct z as [|x]; simpl.
  { rewrite~ subst_anon. }
  { auto. }
Qed.


(* ********************************************************************** *)
(* * Source language semantics *)

(* ---------------------------------------------------------------------- *)
(** Big-step evaluation *)

Implicit Types t : trm.
Implicit Types v : val.
Implicit Types l : loc.
Implicit Types i : field.
Implicit Types b : bool.
Implicit Types n : int.
Implicit Types x : var.
Implicit Types z : bind.

Definition state := fmap loc val.

Section Red.

Local Open Scope fmap_scope.

Inductive red : state -> trm -> state -> val -> Prop :=
  (* Core constructs *)
  | red_val : forall m v,
      red m v m v
  | red_fix : forall m f z t1,
      red m (trm_fix f z t1) m (val_fix f z t1)
  | red_if : forall m1 m2 m3 b r t0 t1 t2,
      red m1 t0 m2 (val_bool b) ->
      red m2 (if b then t1 else t2) m3 r ->
      red m1 (trm_if t0 t1 t2) m3 r
  | red_let : forall m1 m2 m3 z t1 t2 v1 r,
      red m1 t1 m2 v1 ->
      red m2 (subst1 z v1 t2) m3 r ->
      red m1 (trm_let z t1 t2) m3 r
  | red_app : forall m1 m2 m3 m4 t1 t2 f z t3 v1 v2 r,
      red m1 t1 m2 v1 ->
      red m2 t2 m3 v2 ->
      v1 = val_fix f z t3 ->
      red m3 (subst2 f v1 z v2 t3) m4 r ->
      red m1 (trm_app t1 t2) m4 r
  | red_while : forall m1 m2 t1 t2 r,
      red m1 (trm_if t1 (trm_seq t2 (trm_while t1 t2)) val_unit) m2 r ->
      red m1 (trm_while t1 t2) m2 r
  | red_for_arg : forall m1 m2 m3 m4 v1 v2 x t1 t2 t3 r,
      (* LATER: add [not_is_val t1 \/ not_is_val t2] *)
      red m1 t1 m2 v1 ->
      red m2 t2 m3 v2 ->
      red m3 (trm_for x v1 v2 t3) m4 r ->
      red m1 (trm_for x t1 t2 t3) m4 r
  | red_for : forall m1 m2 (x:var) n1 n2 t3 r,
      red m1 (
        If (n1 <= n2)
          then (trm_seq (subst1 x n1 t3) (trm_for x (n1+1) n2 t3))
          else val_unit) m2 r ->
      red m1 (trm_for x n1 n2 t3) m2 r

  (* Primitive operations on the heap *)
  | red_ref : forall ma mb v l,
      mb = (fmap_single l v) ->
      l <> null ->
      \# ma mb ->
      red ma (val_ref v) (mb \+ ma) (val_loc l)
  | red_get : forall m l v,
      fmap_data m l = Some v ->
      red m (val_get (val_loc l)) m v
  | red_set : forall m m' l v,
      m' = fmap_update m l v ->
      red m (val_set (val_loc l) v) m' val_unit
  | red_alloc : forall k n ma mb l,
      mb = fmap_conseq l k val_unit ->
      n = nat_to_Z k ->
      l <> null ->
      \# ma mb ->
      red ma (val_alloc (val_int n)) (mb \+ ma) (val_loc l)
  (* Other primitive operations *)
  | red_add : forall m n1 n2 n',
      n' = n1 + n2 ->
      red m (val_add (val_int n1) (val_int n2)) m (val_int n')
  | red_sub : forall m n1 n2 n',
      n' = n1 - n2 ->
      red m (val_sub (val_int n1) (val_int n2)) m (val_int n')
  | red_ptr_add : forall l' m l n,
      (l':nat) = (l:nat) + n :> int ->
      red m (val_ptr_add (val_loc l) (val_int n)) m (val_loc l')
  | red_eq : forall m v1 v2,
      red m (val_eq v1 v2) m (val_bool (isTrue (v1 = v2))).

  (* Remark: alternative for red_for rules.
    | red_for : forall m1 m2 m3 m4 v1 v2 x t1 t2 t3 r,
        red m1 (
          (trm_seq (trm_let x n1 t3) (trm_for x (n1+1) n2 t3))
          val_unit) m2 r ->
        red m1 (trm_for x n1 n2 t3) m2 r

  Definition trm_is_val (t:trm) : Prop :=
  match t with
  | trm_val v => True
  | _ => False
  end.

  *)

End Red.


(* ---------------------------------------------------------------------- *)
(* ** Derived rules *)

Lemma red_fun : forall m x t1,
  red m (trm_fun x t1) m (val_fun x t1).
Proof using. intros. apply red_fix. Qed.

Lemma red_app_fun : forall m1 m2 m3 m4 t1 t2 z t3 v1 v2 r,
  red m1 t1 m2 v1 ->
  red m2 t2 m3 v2 ->
  v1 = val_fun z t3 ->
  red m3 (subst1 z v2 t3) m4 r ->
  red m1 (trm_app t1 t2) m4 r.
Proof using. intros. applys* red_app. Qed.

Lemma red_seq : forall m1 m2 m3 t1 t2 r1 r,
  red m1 t1 m2 r1 ->
  red m2 t2 m3 r ->
  red m1 (trm_seq t1 t2) m3 r.
Proof using. introv M1 M2. applys* red_let. rewrite* subst1_anon. Qed.

Lemma red_ptr_add_nat : forall m l (f : nat),
  red m (val_ptr_add (val_loc l) (val_int f)) m (val_loc (l+f)%nat).
Proof using. intros. applys* red_ptr_add. math. Qed.

Lemma red_if_bool : forall m1 m2 b r t1 t2,
  red m1 (if b then t1 else t2) m2 r ->
  red m1 (trm_if b t1 t2) m2 r.
Proof using. introv M1. applys* red_if. applys red_val. Qed.

Lemma red_for_le : forall m1 m2 m3 x n1 n2 t3 v1 r,
  n1 <= n2 ->
  red m1 (subst1 x n1 t3) m2 v1 ->
  red m2 (trm_for x (n1+1) n2 t3) m3 r ->
  red m1 (trm_for x n1 n2 t3) m3 r.
Proof using.
  introv N M1 M2. applys red_for. case_if.
  { applys red_seq. applys M1. applys M2. }
  { false; math. }
Qed.

Lemma red_for_gt : forall m x n1 n2 t3,
  n1 > n2 ->
  red m (trm_for x n1 n2 t3) m val_unit.
Proof using.
  introv N. applys red_for. case_if.
  { false; math. }
  { applys red_val. }
Qed.


(* ---------------------------------------------------------------------- *)
(* ** Tactic [fmap_red], defined in file [Fmap] for proving [red] goals
      modulo equalities between states, gets instantiated here. *)

Ltac fmap_red_base tt ::=
  match goal with H: red _ ?t _ _ |- red _ ?t _ _ =>
    applys_eq H 2 4; try fmap_eq end.



(* ********************************************************************** *)
(* * N-ary functions and applications *)

(** [trm_apps t ts] denotes a n-ary application of [t] to the
    arguments from the list [ts].

    [trm_funs xs t] denotes a n-ary function with arguments [xs]
    and body [t].

    [trm_fixs f xs t] denotes a n-ary recursive function [f]
    with arguments [xs] and body [t].

  The tactic [rew_nary] can be used to convert terms in the goal
  to using the nary constructs wherever possible.

  The operation [substn xs vs t] substitutes the variables [xs]
  with the values [vs] inside the term [t].
*)


(* ---------------------------------------------------------------------- *)
(** Coercions from values to terms *)

Coercion vals_to_trms (vs:vals) : trms :=
  List.map trm_val vs.

(** Tactic [rew_vals_to_trms] to fold the coercion where possible *)

Lemma vals_to_trms_fold_start : forall v,
  (trm_val v)::nil = vals_to_trms (v::nil).
Proof using. auto. Qed.

Lemma vals_to_trms_fold_next : forall v vs,
  (trm_val v)::(vals_to_trms vs) = vals_to_trms (v::vs).
Proof using. auto. Qed.

Hint Rewrite vals_to_trms_fold_start vals_to_trms_fold_next
  : rew_vals_to_trms.

Tactic Notation "rew_vals_to_trms" :=
  autorewrite with rew_vals_to_trms.


(* ---------------------------------------------------------------------- *)
(** N-ary applications and N-ary functions *)

(** [trm_apps t (t1::...tn::nil] describes the application term
    [t t1 .. tn]. *)

Fixpoint trm_apps (tf:trm) (ts:trms) : trm :=
  match ts with
  | nil => tf
  | t::ts' => trm_apps (trm_app tf t) ts'
  end.

(** [trm_fixs f (x1::...xn::nil) t] describes the function term
    [trm_fix f x1 (trm_fun x2 ... (trm_fun xn t) ..)]. *)

Fixpoint trm_fixs (f:bind) (xs:vars) (t:trm) : trm :=
  match xs with
  | nil => t
  | x::xs' => trm_fix f x (trm_fixs bind_anon xs' t)
  end.

(** [val_fixs f (x1::...xn::nil) t] describes the function value
    [val_fix f x1 (trm_fun x2 ... (trm_fun xn t) ..)]. *)

Definition val_fixs (f:bind) (xs:vars) (t:trm) : val :=
  match xs with
  | nil => arbitrary
  | x::xs' => val_fix f x (trm_fixs bind_anon xs' t)
  end.

(** [trm_funs (x1::...xn::nil) t] describes the function term
    [trm_fun x1 (trm_fun x2 ... (trm_fun xn t) ..)]. *)

Notation trm_funs := (trm_fixs bind_anon).

(** [val_fun (x1::...xn::nil) t] describes the function value
    [val_fun x1 (trm_fun x2 ... (trm_fun xn t) ..)]. *)

Notation val_funs := (val_fixs bind_anon).


(* ---------------------------------------------------------------------- *)
(** Distribution of [subst] over n-ary functions *)

Lemma subst_trm_funs : forall y w xs t,
  var_fresh y xs ->
  subst1 y w (trm_funs xs t) = trm_funs xs (subst1 y w t).
Proof using.
  introv N. unfold subst1. induction xs as [|x xs']; simpls; fequals.
  { rewrite var_eq_spec in *. case_if. rewrite~ <- IHxs'. }
Qed.

Lemma subst_trm_fixs : forall y w f xs t,
  var_fresh y (f::xs) ->
  subst1 y w (trm_fixs f xs t) = trm_fixs f xs (subst1 y w t).
Proof using.
  introv N. destruct xs as [|x xs'].
  { auto. }
  { unfold subst1. simpls. repeat rewrite var_eq_spec in *.
    do 2 case_if in N. simpl. rewrite var_eq_spec. case_if~.
    fequals.
    forwards K: subst_trm_funs N. unfold subst1, Ctx.one in K.
    simpls. rewrite~ K. }
Qed.  (* LATER: simplify *)


(* ---------------------------------------------------------------------- *)
(** Reduction rules for n-ary functions *)

Lemma red_funs : forall m xs t,
  xs <> nil ->
  red m (trm_funs xs t) m (val_funs xs t).
Proof using.
  introv N. destruct xs as [|x xs']. { false. }
  simpl. applys red_fun.
Qed.

Lemma red_fixs : forall m f xs t,
  xs <> nil ->
  red m (trm_fixs f xs t) m (val_fixs f xs t).
Proof using.
  introv N. destruct xs as [|x xs']. { false. }
  simpl. applys red_fix.
Qed.


(* ---------------------------------------------------------------------- *)
(** Reduction rules for n-ary applications *)

(* Internal lemma *)
Lemma red_app_funs_val_ind : forall xs vs m1 m2 tf t r,
  red m1 tf m1 (val_funs xs t) ->
  red m1 (substn xs vs t) m2 r ->
  var_funs (length vs) xs ->
  red m1 (trm_apps tf vs) m2 r.
Proof using.
  introv M1 M2 (F&L&N). gen tf t r m1 m2 F N. list2_ind~ xs vs; intros.
  { false. }
  { rename xs1 into xs', x1 into x1, x2 into v1, xs2 into vs', H0 into IH.
    simpl in F. rew_istrue in F. destruct F as (F1&F').
    tests C: (xs' = nil).
    { rew_list in *. asserts: (vs' = nil).
      { applys length_zero_inv. math. } subst vs'.
      simpls. applys* red_app. applys* red_val. }
    { rewrite substn_cons in M2. applys~ IH M2. applys* red_app.
      { applys* red_val. }
      { simpl. unfold subst2. simpl. rew_ctx.
        rewrite substx_add. rewrite substx_empty.
        rewrite~ subst_trm_funs. applys~ red_funs. } } }
Qed.

(** Reduction rule for n-ary functions *)

Lemma red_app_funs_val : forall xs vs m1 m2 vf t r,
  vf = val_funs xs t ->
  red m1 (substn xs vs t) m2 r ->
  var_funs (length vs) xs ->
  red m1 (trm_apps vf vs) m2 r.
Proof using.
  introv R M N. applys* red_app_funs_val_ind.
  { subst. apply~ red_val. }
Qed.

(** Reduction rule for n-ary recursive functions *)

Lemma red_app_fixs_val : forall xs (vs:vals) m1 m2 vf (f:var) t r,
  vf = val_fixs f xs t ->
  red m1 (substn (f::xs) (vf::vs) t) m2 r ->
  var_fixs f (length vs) xs ->
  red m1 (trm_apps vf vs) m2 r.
Proof using.
  introv E M (N&L&P). destruct xs as [|x xs']. { false. }
  { destruct vs as [|v vs']; rew_list in *. { false; math. } clear P.
    destruct N as (N1&N2&N3). simpls. case_if.
    tests C':(xs' = nil).
    { rew_list in *. asserts: (vs' = nil).
      { applys length_zero_inv. math. } subst vs'. clear L.
      subst vf. simpls. hint red_val. applys* red_app. }
    { applys~ red_app_funs_val_ind.
      { hint red_val. applys* red_app.
        rewrite subst2_eq_subst1_subst1. do 2 rewrite~ subst_trm_funs.
        applys* red_funs. }
      { do 2 rewrite substn_cons in M. applys M. }
      { splits*. } } }
Qed.


(* ---------------------------------------------------------------------- *)
(** Rewriting lemmas and tactics to "fold" n-ary functions,
    i.e. to introduce [trm_apps] and [trm_funs] and [trm_fixs] from
    the iterated application of the corresponding unary constructors. *)

Lemma trm_apps_fold_start : forall t1 t2,
  trm_app t1 t2 = trm_apps t1 (t2::nil).
Proof using. auto. Qed.

Lemma trm_apps_fold_next : forall t1 t2 t3s,
  trm_apps (trm_app t1 t2) t3s = trm_apps t1 (t2::t3s).
Proof using. auto. Qed.

Lemma trm_apps_fold_concat : forall t1 t2s t3s,
  trm_apps (trm_apps t1 t2s) t3s = trm_apps t1 (List.app t2s t3s).
Proof using.
  intros. gen t1; induction t2s; intros. { auto. }
  { rewrite <- trm_apps_fold_next. simpl. congruence. }
Qed.

Lemma trm_fixs_fold_start : forall f x t,
  trm_fix f x t = trm_fixs f (x::nil) t.
Proof using. auto. Qed.

(* subsumed by iteration of trm_funs_fold_next *)
Lemma trm_funs_fold_app : forall xs ys t,
  trm_funs xs (trm_funs ys t) = trm_funs (List.app xs ys) t.
Proof using.
  intros. induction xs. { auto. } { simpl. congruence. }
Qed.

(* for innermost-first rewriting strategy
Lemma trm_fixs_fold_next : forall f x xs t,
  trm_fixs f (x::nil) (trm_funs xs t) = trm_fixs f (x::xs) t.
Proof using. auto. Qed.
*)

Lemma trm_fixs_fold_app : forall f x xs ys t,
  trm_fixs f (x::xs) (trm_funs ys t) = trm_fixs f (x :: List.app xs ys) t.
Proof using. intros. simpl. rewrite~ trm_funs_fold_app. Qed.

Lemma val_fixs_fold_start : forall f x t,
  val_fix f x t = val_fixs f (x::nil) t.
Proof using. auto. Qed.

Lemma val_fixs_fold_app : forall f x xs ys t,
  val_fixs f (x::xs) (trm_funs ys t) = val_fixs f (List.app (x::xs) ys) t.
Proof using. intros. simpl. rewrite~ trm_funs_fold_app. Qed.

Lemma val_fixs_fold_app' : forall f x xs ys t,
  val_fixs f (List.app (x::nil) xs) (trm_funs ys t) = val_fixs f (List.app (x::xs) ys) t.
Proof using. intros. simpl. rewrite~ trm_funs_fold_app. Qed.

Hint Rewrite
  trm_apps_fold_start trm_apps_fold_next trm_apps_fold_concat
  trm_fixs_fold_start trm_fixs_fold_app
  val_fixs_fold_start val_fixs_fold_app val_fixs_fold_app' : rew_nary.

Tactic Notation "rew_nary" :=
  autorewrite with rew_nary; simpl List.app.
Tactic Notation "rew_nary" "in" hyp(H) :=
  autorewrite with rew_nary in H; simpl List.app in H.
Tactic Notation "rew_nary" "in" "*" :=
  autorewrite with rew_nary in *; simpl List.app in *.
  (* rewrite_strat (any (innermost (hints rew_nary))).
     => way too slow! *)

(* Demos:
Lemma rew_nary_demo_1 : forall (f x y z:var) t1 t2 v,
  val_fix f x (trm_fun y (trm_fun z (f t1 x y t2))) = v.
Proof using. intros. rew_nary. Abort.

Lemma rew_nary_demo_2 : forall f x1 x2 v,
  val_fun f (trm_fun x1 (trm_fun x2 (x1 x2))) = v.
Proof using. intros. rew_nary. Abort.
*)


(* ********************************************************************** *)
(* * Size of a term *)

(* ---------------------------------------------------------------------- *)
(** Size of a term, where all values counting for one unit. *)

(** The definition of size can be useful for some tricky inductions *)

Fixpoint trm_size (t:trm) : nat :=
  match t with
  | trm_var x => 1
  | trm_val v => 1
  | trm_fix f x t1 => 1 + trm_size t1
  | trm_if t0 t1 t2 => 1 + trm_size t0 + trm_size t1 + trm_size t2
  | trm_let x t1 t2 => 1 + trm_size t1 + trm_size t2
  | trm_app t1 t2 => 1 + trm_size t1 + trm_size t2
  | trm_while t1 t2 => 1 + trm_size t1 + trm_size t2
  | trm_for x t1 t2 t3 => 1 + trm_size t1 + trm_size t2 + trm_size t3
  end.

Lemma trm_size_substx : forall t E,
  trm_size (substx E t) = trm_size t.
Proof using.
  intros t. induction t; intros; simpl; repeat case_if; auto.
  { destruct~ (Ctx.lookup v E). }
Qed.

Lemma trm_size_subst1 : forall t z v,
  trm_size (subst1 z v t) = trm_size t.
Proof using. intros. applys trm_size_substx. Qed.

(** Hint for induction on size. Proves subgoals of the form
    [measure trm_size t1 t2], when [t1] and [t2] may have some
    structure or involve substitutions. *)

Ltac solve_measure_trm_size tt :=
  unfold measure in *; simpls; repeat rewrite trm_size_subst1; math.

Hint Extern 1 (measure trm_size _ _) => solve_measure_trm_size tt.


(* ********************************************************************** *)
(* * Notation for terms *)

(* ---------------------------------------------------------------------- *)
(** Notation for concrete programs *)

Module NotationForTerms.

(** Note: below, many occurences of [x] have type [bind], and not [var] *)

Notation "'()" := val_unit : trm_scope.

Notation "'If_' t0 'Then' t1 'Else' t2" :=
  (trm_if t0 t1 t2)
  (at level 69, t0 at level 0) : trm_scope.

Notation "'If_' t0 'Then' t1 'End'" :=
  (trm_if t0 t1 val_unit)
  (at level 69, t0 at level 0) : trm_scope.

Notation "'Let' x ':=' t1 'in' t2" :=
  (trm_let x t1 t2)
  (at level 69, x at level 0, right associativity,
  format "'[v' '[' 'Let'  x  ':='  t1  'in' ']'  '/'  '[' t2 ']' ']'") : trm_scope.

Notation "'Let' 'Rec' f x1 ':=' t1 'in' t2" :=
  (trm_let f (trm_fix f x1 t1) t2)
  (at level 69, f, x1 at level 0, right associativity,
  format "'[v' '[' 'Let'  'Rec'  f  x1  ':='  t1  'in' ']'  '/'  '[' t2 ']' ']'") : trm_scope.

Notation "'Let' 'Rec' f x1 x2 .. xn ':=' t1 'in' t2" :=
  (trm_let f (trm_fix f x1 (trm_fun x2 .. (trm_fun xn t1) ..) t2))
  (at level 69, f, x1, x2, xn at level 0, right associativity,
  format "'[v' '[' 'Let'  'Rec'  f  x1  x2  ..  xn  ':='  t1  'in' ']'  '/'  '[' t2 ']' ']'") : trm_scope.

  (* LATER: the above might need to be fixed. Here is how to test it:
     Definition test2 := Let Rec 'f 'x 'y := val_unit in val_unit.
     Print test2. *)

Notation "t1 ;;; t2" :=
  (trm_seq t1 t2)
  (at level 68, right associativity, only parsing,
   format "'[v' '[' t1 ']'  ;;;  '/'  '[' t2 ']' ']'") : trm_scope.

Notation "'Fix' f x1 ':=' t" :=
  (trm_fix f x1 t)
  (at level 69, f, x1 at level 0) : trm_scope.

Notation "'Fix' f x1 x2 ':=' t" :=
  (trm_fix f x1 (trm_fun x2 t))
  (at level 69, f, x1, x2 at level 0) : trm_scope.

Notation "'Fix' f x1 x2 .. xn ':=' t" :=
  (trm_fix f x1 (trm_fun x2 .. (trm_fun xn t) ..))
  (at level 69, f, x1, x2, xn at level 0) : trm_scope.

Notation "'ValFix' f x1 ':=' t" :=
  (val_fix f x1 t)
  (at level 69, f, x1 at level 0) : trm_scope.

Notation "'ValFix' f x1 x2 .. xn ':=' t" :=
  (val_fix f x1 (trm_fun x2 .. (trm_fun xn t) ..))
  (at level 69, f, x1, x2, xn at level 0) : trm_scope.

Notation "'Fun' x1 ':=' t" :=
  (trm_fun x1 t)
  (at level 69, x1 at level 0) : trm_scope.

Notation "'Fun' x1 x2 .. xn ':=' t" :=
  (trm_fun x1 (trm_fun x2 .. (trm_fun xn t) ..))
  (at level 69, x1, x2, xn at level 0) : trm_scope.

Notation "'ValFun' x1 ':=' t" :=
  (val_fun x1 t)
  (at level 69, x1 at level 0) : trm_scope.

Notation "'ValFun' x1 x2 .. xn ':=' t" :=
  (val_fun x1 (trm_fun x2 .. (trm_fun xn t) ..))
  (at level 69, x1, x2, xn at level 0) : trm_scope.

Notation "'LetFun' f x1 ':=' t1 'in' t2" :=
  (trm_let f (trm_fun x1 t1) t2)
  (at level 69, f at level 0, x1 at level 0) : trm_scope.

Notation "'LetFun' f x1 x2 .. xn ':=' t1 'in' t2" :=
  (trm_let f (trm_fun x1 (trm_fun x2 .. (trm_fun xn t1) ..)) t2)
  (at level 69, f, x1, x2, xn at level 0) : trm_scope.

Notation "'LetFix' f x1 ':=' t1 'in' t2" :=
  (trm_let f (trm_fix f x1 t1) t2)
  (at level 69, f at level 0, x1 at level 0) : trm_scope.

Notation "'While' t1 'Do' t2 'Done'" :=
  (trm_while t1 t2)
  (at level 69, t2 at level 68,
   format "'[v' 'While'  t1  'Do'  '/' '[' t2 ']' '/'  'Done' ']'")
   : trm_scope.

Notation "'For' x ':=' t1 'To' t2 'Do' t3 'Done'" :=
  (trm_for x t1 t2 t3)
  (at level 69, x at level 0, (* t1 at level 0, t2 at level 0, *)
   format "'[v' 'For'  x  ':='  t1  'To'  t2  'Do'  '/' '[' t3 ']' '/'  'Done' ']'")
  : trm_scope.

Notation "'ref t" :=
  (val_ref t)
  (at level 67) : trm_scope.

Notation "'! t" :=
  (val_get t)
  (at level 67) : trm_scope.

Notation "t1 ':= t2" :=
  (val_set t1 t2)
  (at level 67) : trm_scope.

Notation "t1 '+ t2" :=
  (val_add t1 t2)
  (at level 69) : trm_scope.

Notation "t1 '- t2" :=
  (val_sub t1 t2)
  (at level 69) : trm_scope.

Notation "t1 '= t2" :=
  (val_eq t1 t2)
  (at level 69) : trm_scope.


(* Demo for the above notation:

  Open Scope trm_scope.
  Import NotationForVariables.
  Definition test := Fun 'x := val_unit.
  Definition test2 := Fun 'x 'y 'z := val_unit.
  Definition test1 := Fix 'f 'x1 := val_unit.
  Definition test2 := Fix 'f 'x1 'x2 := val_unit.
  Print test2.
  Definition test1 := LetFix 'f 'x1 := val_unit in val_unit.
  Definition test2 := LetFix 'f 'x1 'x2 := val_unit in val_unit.
*)

End NotationForTerms.



