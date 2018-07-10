(**

This file describes transformations of the layout of records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export Semantics LibSet LibMap TLCbuffer.



(* ********************************************************************** *)
(* * Specification of the transformation *)

Module Example.

(* Initial typdefctx *)
Definition pos : typdef_struct := (\{})["x" := typ_int]["y" := typ_int]["z" := typ_int].
Definition C : typdefctx := (\{})["pos" := pos].

(* Final typdefctx *)
Definition struct_x : typdef_struct := (\{})["x" := typ_int].
Definition pos' : typdef_struct := (\{})["s" := (typ_struct "struct_x")]["y" := typ_int]["z" := typ_int].
Definition C' : typdefctx := (\{})["pos" := pos']["struct_x" := struct_x].

End Example.


(* ********************************************************************** *)
(* * Definition of the transformation *)

(** Grouping transformation. Specified by:
    - The name of the struct to be modified.
    - The set of fields to be grouped.
    - The name of the new struct that will hold the fields to be grouped.
    - The name of the field in the new struct that will have as type
      the new struct. *)

Record group_tr := make_group_tr {
  group_tr_struct_name : typvar;
  group_tr_fields : set field;
  group_tr_new_struct_name : typvar;
  group_tr_new_struct_field : field
}.

Notation make_group_tr' := make_group_tr.

(** Transformation of paths: π ~ |π| *)

Inductive tr_accesses (gt:group_tr) : accesses -> accesses -> Prop :=
  | tr_accesses_nil :
      tr_accesses gt nil nil
  | tr_accesses_array : forall π π' i,
      tr_accesses gt π π' ->
      tr_accesses gt ((access_array i)::π) ((access_array i)::π')
  | tr_accesses_field_group : forall π π' f fg Ts Tsg,
      tr_accesses gt π π' ->
      Ts = group_tr_struct_name gt ->
      Tsg = group_tr_new_struct_name gt ->
      f \in (group_tr_fields gt) ->
      fg = group_tr_new_struct_field gt ->
      tr_accesses gt ((access_field Ts f)::π) ((access_field Ts fg)::(access_field Tsg f)::π')
  | tr_accesses_field_other : forall T Ts π π' f,
      tr_accesses gt π π' ->
      Ts = group_tr_struct_name gt ->
      (T <> Ts \/ f \notin (group_tr_fields gt)) ->
      tr_accesses gt ((access_field T f)::π) ((access_field T f)::π').

(** Transformation of values: v ~ |v| *)

Inductive tr_val (gt:group_tr) : val -> val -> Prop :=
  | tr_val_error :
      tr_val gt val_error val_error
  | tr_val_uninitialized :
      tr_val gt val_uninitialized val_uninitialized
  | tr_val_unit :
      tr_val gt val_unit val_unit
  | tr_val_bool : forall b,
      tr_val gt (val_bool b) (val_bool b)
  | tr_val_int : forall i,
      tr_val gt (val_int i) (val_int i)
  | tr_val_double : forall d,
      tr_val gt (val_double d) (val_double d)
  | tr_val_abstract_ptr : forall l π π',
      tr_accesses gt π π' ->
      tr_val gt (val_abstract_ptr l π) (val_abstract_ptr l π')
  | tr_val_array : forall a a',
      length a = length a' ->
      (forall i,
        index a i ->
        tr_val gt a[i] a'[i]) ->
      tr_val gt (val_array a) (val_array a')
  | tr_val_struct_group : forall Ts Tsg s s' fg fs sg,
      gt = make_group_tr Ts fs Tsg fg ->
      fs \c dom s ->
      fg \notindom s ->
      dom s' = (dom s \- fs) \u \{fg} ->
      dom sg = fs ->
      (forall f,
        f \indom sg ->
        tr_val gt s[f] sg[f]) ->
      (forall f,
        f \notin fs ->
        f \indom s ->
        tr_val gt s[f] s'[f]) ->
      s'[fg] = val_struct Tsg sg ->
      tr_val gt (val_struct Ts s) (val_struct Ts s')
  | tr_val_struct_other : forall T s s',
      T <> group_tr_struct_name gt ->
      dom s = dom s' ->
      (forall f,
        f \indom s ->
        tr_val gt s[f] s'[f]) ->
      tr_val gt (val_struct T s) (val_struct T s').

Definition is_prim_struct_access (op:prim) :=
  match op with
  | prim_struct_access _ _ => True
  | _ => False
  end.


(** Transformation of terms: t ~ |t| *)

Inductive tr_trm (gt:group_tr) : trm -> trm -> Prop :=
  | tr_trm_val : forall v v',
      tr_val gt v v' ->
      tr_trm gt (trm_val v) (trm_val v')
  | tr_trm_var : forall x,
      tr_trm gt (trm_var x) (trm_var x)
  | tr_trm_if : forall t1 t2 t3 t1' t2' t3',
      tr_trm gt t1 t1' ->
      tr_trm gt t2 t2' ->
      tr_trm gt t3 t3' ->
      tr_trm gt (trm_if t1 t2 t3) (trm_if t1' t2' t3')
  | tr_trm_let : forall x t1 t2 t1' t2',
      tr_trm gt t1 t1' ->
      tr_trm gt t2 t2' ->
      tr_trm gt (trm_let x t1 t2) (trm_let x t1' t2')
  (* new *)  
  | tr_trm_new : forall T,
      tr_trm gt (trm_app (prim_new T) nil) (trm_app (prim_new T) nil)
  (* Special case: struct access *)
  | tr_trm_struct_access_x : forall p p' s s_g f f_g a1 a2 r,
      tr_trm gt p p' ->
      s = group_tr_struct_name gt ->
      s_g = group_tr_new_struct_name gt ->
      f \in (group_tr_fields gt) ->
      f_g = group_tr_new_struct_field gt ->
      a1 = prim_struct_access s_g f ->
      a2 = prim_struct_access s f_g ->
      r = trm_app a1 ((trm_app a2 (p'::nil))::nil) ->
      tr_trm gt (trm_app (prim_struct_access s f) (p::nil)) r
  | tr_trm_struct_access_other : forall s p p' T f r,
      tr_trm gt p p' ->
      s = group_tr_struct_name gt ->
      (T <> s \/ f \notin (group_tr_fields gt)) ->
      r = (trm_app (prim_struct_access T f) (p'::nil)) ->
      tr_trm gt (trm_app (prim_struct_access T f) (p::nil)) r
  (* Args *)
  | tr_trm_args1 : forall op t1 t1',
      ~ is_prim_struct_access op ->
      tr_trm gt t1 t1' ->
      tr_trm gt (trm_app op (t1::nil)) (trm_app op (t1'::nil))
  | tr_trm_args2 : forall op t1 t1' t2 t2',
      tr_trm gt t1 t1' ->
      tr_trm gt t2 t2' ->
      tr_trm gt (trm_app op (t1::t2::nil)) (trm_app op (t1'::t2'::nil)).


(** Transformation of stacks: S ~ |S| *)

Inductive tr_stack_item (gt:group_tr) : (var * val) -> (var * val) -> Prop :=
  | tr_stack_item_intro : forall x v v',
      tr_val gt v v' -> 
      tr_stack_item gt (x, v) (x, v').

Inductive tr_stack (gt:group_tr) : stack -> stack -> Prop :=
  | tr_stack_intro : forall S S',
      LibList.Forall2 (tr_stack_item gt) S S' ->
      tr_stack gt S S'.

Lemma stack_lookup_tr : forall gt S S' x v,
  tr_stack gt S S' ->
  Ctx.lookup x S = Some v -> 
    exists v', 
       Ctx.lookup x S' = Some v' 
    /\ tr_val gt v v'.
Proof.
  introv HS Hx. inverts HS as HS. induction HS.
  { inverts Hx. }
  { inverts H as Hv. inverts Hx as Hx. case_if in Hx.
    { inverts Hx. exists v'. splits*. unfolds. case_if*. }
    { forwards (v''&Hx'&Hv''): IHHS Hx. exists v''.
      splits*. unfolds. case_if. fold Ctx.lookup. auto. } }
Qed.


(** Transformation of states: m ~ |m| *)

Inductive tr_state (gt:group_tr) : state -> state -> Prop :=
  | tr_state_intro : forall m m',
      dom m = dom m' ->
      (forall l,
        l \indom m ->
        tr_val gt m[l] m'[l]) ->
      tr_state gt m m'.


(** Transformation of typdefctxs: C ~ |C| *)

Inductive tr_typdefctx (gt:group_tr) : typdefctx -> typdefctx -> Prop :=
  | tr_typdefctx_intro : forall Tt fs Tg fg C C',
      gt = make_group_tr Tt fs Tg fg ->
      Tt \indom C ->
      Tg \notindom C ->
      dom C' = dom C \u \{Tg} ->
      (forall T,
        T \indom C ->
        T <> Tt ->
        C'[T] = C[T]) ->
      fs \c dom (C[Tt]) ->
      fg \notindom (C[Tt]) ->
      dom (C'[Tt]) = (dom (C[Tt]) \- fs) \u \{fg} ->
      (forall f,
        f \indom (C[Tt]) ->
        f \notin fs ->
        C'[Tt][f] = C[Tt][f]) ->
      C[Tt][fg] = typ_struct Tg ->
      dom (C'[Tg]) = fs ->
      (forall f,
        f \indom (C'[Tg]) ->
        C'[Tg][f] = C[Tt][f]) ->
      tr_typdefctx gt C C'.


(* ********************************************************************** *)
(* * Correctness of the transformation *)

Section TransformationsProofs.


(* ---------------------------------------------------------------------- *)
(** Hints *)

(* TODO: Factor this out. *)
Lemma index_of_index_length : forall A (l' l : list A) i,
  index l' i ->
  length l' = length l ->
  index l i.
Proof.
  intros. rewrite index_eq_index_length in *.
  applys* index_of_index_length'.
Qed.

Hint Resolve index_of_index_length.

Hint Constructors red.
Hint Constructors tr_trm tr_val tr_accesses tr_state tr_stack.
Hint Constructors read_accesses write_accesses.


(* ---------------------------------------------------------------------- *)
(** Functionality of the relations *)

Theorem functional_tr_accesses : forall gt π π1 π2,
  tr_accesses gt π π1 ->
  tr_accesses gt π π2 ->
    π1 = π2.
Proof.
  introv H1 H2. gen π2. induction H1; intros;
  inverts_head tr_accesses; repeat fequals*;
  inverts_head Logic.or; repeat fequals*.
Qed. 

Theorem functional_tr_val : forall gt v v1 v2,
  tr_val gt v v1 ->
  tr_val gt v v2 ->
  v1 = v2.
Proof using.
  introv H1 H2. gen v2. induction H1; intros;
  inverts_head tr_val; fequals*.
  { applys* functional_tr_accesses. }
  { applys* eq_of_extens. math. }
  { applys read_extens. 
    { inverts_head make_group_tr'. congruence. }
    { introv Hin. tests C: (i = fg).
      { inverts_head make_group_tr'.
        asserts_rewrite~ (s'0[fg0] = val_struct Tsg0 sg0).
        asserts_rewrite~ (s'[fg0] = val_struct Tsg0 sg).
        fequals. applys~ read_extens. introv Hk. 
        asserts_rewrite* (dom sg = dom sg0) in *. }
      { inverts_head make_group_tr'.
        asserts_rewrite~ (dom s' = dom s \- dom sg \u '{fg0}) in Hin.
        inverts Hin as Hin; tryfalse. inverts Hin as Hin Hnotin.
        asserts_rewrite* (dom sg = dom sg0) in *. } } }
  { subst. simpls. contradiction. }
  { applys read_extens.
    { congruence. }
    { introv Hin. 
      asserts_rewrite* (dom s' = dom s) in *. } }
Qed.

Theorem functional_tr_trm : forall gt t t1 t2,
  tr_trm gt t t1 ->
  tr_trm gt t t2 ->
  t1 = t2.
Proof.
  introv H1 H2. gen t2. induction H1; intros;
  try solve [ inverts H2 ; try subst ; repeat fequals* ].
  { inverts H2. fequals. applys* functional_tr_val. }
  { inverts_head tr_trm; subst. 
    { repeat fequals~. }
    { inverts_head Logic.or; tryfalse. } 
    { unfolds is_prim_struct_access. contradiction. } }
  { inverts_head tr_trm; subst.
    { inverts_head Logic.or; tryfalse. }
    { repeat fequals~. } 
    { unfolds is_prim_struct_access. contradiction. } }
  { inverts_head tr_trm; 
    try solve [ unfolds is_prim_struct_access ; contradiction ].
    repeat fequals~. }
Qed.

Theorem functional_tr_stack : forall gt S S1 S2,
  tr_stack gt S S1 ->
  tr_stack gt S S2 ->
  S1 = S2.
Proof.
  admit. (* extens lemma for ctxts. *)
Admitted.

Theorem functional_tr_state : forall gt m m1 m2,
  tr_state gt m m1 ->
  tr_state gt m m2 ->
  m1 = m2.
Proof.
  admit. (* extens lemma for maps. *)
Admitted.

Lemma tr_stack_add : forall gt z v S v' S',
  tr_stack gt S S' ->
  tr_val gt v v' ->
  tr_stack gt (Ctx.add z v S) (Ctx.add z v' S').
Proof.
Admitted.


(* ---------------------------------------------------------------------- *)
(** Path surgery *)

Lemma tr_accesses_app : forall gt π1 π2 π1' π2',
  tr_accesses gt π1 π1' ->
  tr_accesses gt π2 π2' ->
  tr_accesses gt (π1 ++ π2) (π1' ++ π2').
Proof.
  introv Ha1 Ha2. gen π2 π2'. induction Ha1; intros;
  rew_list in *; eauto. 
Qed.


(* ---------------------------------------------------------------------- *)
(** Regularity of the transformation with respect to values *)

Lemma not_is_val_tr : forall gt t1 t2,
  ~ is_val t1 ->
  tr_trm gt t1 t2 ->
  ~ is_val t2.
Proof.
  introv Hv Htr. induction Htr; introv HN;
  try solve [ subst ; inverts HN ]. forwards*: Hv.
Qed.

Lemma not_is_error_tr : forall gt v1 v2,
  ~ is_error v1 ->
  tr_val gt v1 v2 ->
  ~ is_error v2.
Proof.
  introv He Htr. induction Htr; introv HN;
  try solve [ subst ; inverts HN ]. forwards*: He.
Qed.

Lemma not_is_uninitialized_tr : forall gt v v',
  ~ is_uninitialized v ->
  tr_val gt v v' -> 
  ~ is_uninitialized v'.
Proof.
  introv Hu Htr. induction Htr; introv HN;
  subst; inverts HN. forwards~: Hu. unfolds~.
Qed.

Lemma not_is_error_args_1 : forall C S m op ts m' v w,
  red C S m (trm_app op (trm_val w :: ts)) m' v ->
  ~ is_error v ->
  ~ is_error w.
Proof.
  introv HR He HN. inverts HN;
  inverts HR; tryfalse. 
  { inverts_head redbinop. tryfalse. }
  { forwards*: (is_val val_error). }
  { inverts_head red; tryfalse.
    { inverts_head redbinop. tryfalse. }
    { forwards*: (is_val val_error). }
    { forwards*: (is_val v2). } }
Qed.

Lemma not_is_error_args_2 : forall C S m op t ts m' v w,
  red C S m (trm_app op (t :: trm_val w :: ts)) m' v ->
  ~ is_error v ->
  ~ is_error w.
Proof.
  introv HR He HN. inverts HN; inverts HR; tryfalse.
  { inverts_head redbinop. tryfalse. }
  { inverts_head red; tryfalse.
    { inverts_head redbinop. tryfalse. }
    { forwards*: (is_val v1). }
    { forwards*: (is_val val_error). } }
  { forwards*: (is_val val_error). }
Qed.

Lemma neq_tr : forall gt v1 v2 v1' v2',
  v1 <> v2 ->
  tr_val gt v1 v1' ->
  tr_val gt v2 v2' ->
  v1' <> v2'.
Proof.
  (* Exactly the same as saying that tr_val is injective. *)
  introv Hneq Hv1 Hv2 HN. admit.
Admitted.

Axiom not_tr_val_error : forall gt v1 v2,
  tr_val gt v1 v2 ->
  ~ is_error v2.

(* TODO: This doesn't quite work. *)
Lemma tr_uninitialized_val : forall gt v C C' T,
  tr_typdefctx gt C C' ->
  uninitialized_val C T v ->
  exists v',
        tr_val gt v v'
    /\  uninitialized_val C' T v'.
Proof.
Admitted.


(* ---------------------------------------------------------------------- *)
(* TLC BUFFER *)

Section Autorewrite.
Variables (A : Type).
Implicit Types x y : A.
Implicit Types E F : set A.

Lemma set_notin_eq : forall x E,
  x \notin E = ~ x \in E.
Proof using. apply notin_eq. Qed.

End Autorewrite.

Hint Rewrite set_notin_eq : rew_set.

Ltac eliminate_neq_goal tt :=
  match goal with |- ?x <> ?y =>
    let H := fresh in intros H; subst_hyp H end.

Ltac set_prove_setup use_classic ::=
  intros;
  try match goal with |- ?x <> ?y => intros ? end;
  try substs;
  rew_set_tactic tt;
  try set_specialize use_classic;
  rew_set_tactic tt.

Ltac set_prove_conclude ::=
  solve [ intros; subst; intuition eauto ].

(** Use [set_prove_show] to see what preprocessing is done before
   [set_prove_conclude] gets called.  *)

Ltac set_prove_show :=
  set_prove_setup false.


(* ---------------------------------------------------------------------- *)
(** Auxiliary set results *)

Lemma in_notin_neq : forall A (x y:A) (S:set A),
  x \in S ->
  y \notin S ->
  y <> x.
Proof using. set_prove. Qed.


(* ---------------------------------------------------------------------- *)
(** Correctness of access transformations *)

Lemma tr_read_accesses : forall gt v π v' π' w,
  tr_val gt v v' ->
  tr_accesses gt π π' ->
  read_accesses v π w ->
  (exists w',
      tr_val gt w w'
  /\  read_accesses v' π' w').
Proof.
  introv Hv Ha HR. gen gt v' π'. induction HR; intros.
  { (* nil *)
    inverts Ha. exists~ v'. }
  { (* array_access *)
    inverts Ha as Ha. inverts Hv as Hl Htr.
    forwards Htra: Htr H.
    forwards (w'&Hw'&Hπ'): IHHR Htra Ha.
    exists* w'. }
  { (* struct_access *)
    inverts Ha as; inverts Hv as;
    try solve [ intros ; false ].
    { (* one of the fields to group *)
      introv HD1 Hgt HD2 HD3 Hsg Hfs Hsfg Hπ Hin.
      rewrite Hgt in *. simpls.
      forwards Hsf: Hsg Hin.
      forwards (w'&Hw'&HR'): IHHR Hsf Hπ.
      exists w'. splits~.
      constructors. 
      { rewrite HD3. rew_set~. } 
      rewrite Hsfg. constructors~. }
    { (* struct transformed but another field *)
      introv HD1 HD2 HD3 Hsg Hfs Hsfg Hπ Hor.
      inverts Hor as Hf; simpl in Hf; tryfalse.
      forwards Hsf: Hfs Hf H.
      forwards (w'&Hw'&HR'): IHHR Hsf Hπ.
      exists w'. splits~. constructors~.
      rewrite HD3. rew_set~. }
    { (* another struct *)
      intros Hn HD Hfs Hπ Hor.
      forwards Hsf: Hfs H.
      forwards (w'&Hw'&HR'): IHHR Hsf Hπ.
      exists w'. splits~. constructors~.
      rewrite~ <- HD. } }
Qed.


Lemma tr_write_accesses : forall v1 w gt π v1' π' w' v2,
  tr_val gt v1 v1' ->
  tr_val gt w w' ->
  tr_accesses gt π π' ->
  write_accesses v1 π w v2 ->
  (exists v2',
        tr_val gt v2 v2'
    /\  write_accesses v1' π' w' v2').
Proof.
  introv Hv1 Hw Hπ HW. gen gt v1' w' π'. induction HW; intros.
  { (* nil *)
    inverts Hπ. exists~ w'. }
  { (* array_access *)
    inverts Hπ as Hπ. inverts Hv1 as Hl Htr.
    forwards Htra: Htr H.
    forwards (v2'&Hv2'&HW'): IHHW Htra Hw Hπ.
    exists (val_array a'[i:=v2']).
    splits; constructors*; rewrite H0.
    { repeat rewrite~ length_update. }
    { introv Hi0. rew_reads; rew_index* in *. } }
  { (* struct_access *)
    inverts Hπ as; inverts Hv1 as;
    try solve [ intros ; false ].
    { (* one of the fields to group *)
      introv HD1 Hgt HD2 HD3 Hsg Hfs Hsfg Hπ Hin.
      rewrite Hgt in *. simpls.
      forwards Hsf: Hsg Hin.
      forwards (v2'&Hv2'&HW'): IHHW Hsf Hw Hπ.
      remember (group_tr_struct_name gt) as T.
      exists (val_struct T s'[fg:=(val_struct Tsg sg[f:=v2'])]).
      substs. splits.
      { applys~ tr_val_struct_group (sg[f:=v2']);
        repeat rewrite dom_update_at_indom in *; eauto.
        { rewrite HD3. rew_set~. }
        { intros. rew_reads*. }
        { intros. rew_reads; intros; subst; eauto; contradiction. }
        { rew_reads~. } }
      { constructors~. rewrite HD3. rew_set~.
        rewrite Hsfg. constructors*. } }
    { (* struct transformed but another field *)
      introv HD1 HD2 HD3 Hsg Hfs Hsfg Hπ Hor.
      inverts Hor as Hf; simpl in Hf; tryfalse.
      forwards Hsf: Hfs Hf H.
      forwards (v2'&Hv2'&HW'): IHHW Hsf Hw Hπ.
      exists (val_struct T s'[f:=v2']). splits.
      { applys~ tr_val_struct_group; subst;
        repeat rewrite dom_update_at_indom in *; eauto.
        { rewrite HD3. rew_set~. }
        { intros. rew_reads; intros; eauto. 
          subst. forwards*: in_notin_neq. }
        { intros. rew_reads*. }
        { rew_reads~. intros. 
          subst. forwards*: in_notin_neq. } }
      { constructors~. rewrite HD3. rew_set~. auto. } }
    { (* another struct *)
      intros Hn HD Hfs Hπ Hor.
      forwards Hsf: Hfs H.
      forwards (v2'&Hv2'&HW'): IHHW Hsf Hw Hπ.
      exists (val_struct T s'[f:=v2']). splits.
      { constructors~; subst;
        repeat rewrite dom_update_at_indom; eauto.
        { rewrite~ <- HD. }
        { intros. rew_reads~. } }
      { constructors~. rewrite* <- HD. auto. } } }
Qed.


(* ---------------------------------------------------------------------- *)
(** Correctness of the transformation *)

Axiom isTrue_var_eq : forall A (v1 v2:A), v1 = v2 -> isTrue (v1 = v2) = true.

Axiom isTrue_var_neq : forall A (v1 v2:A), v1 <> v2 -> isTrue (v1 = v2) = false.

(* TODO: Rewrites used quite often throughout: 
   - dom_update_at_indom
   - dom_update *)

(* TODO: Sometimes, in order to use map tactics or 
   lemmas, I have to do [unfold state] in order to get
   [map loc var] instead of [state] but then the
   rewrites stop working so I have to [fold state] again.
   How can we fix this? *)

Theorem red_tr: forall gt C C' t t' v S S' m1 m1' m2,
  tr_typdefctx gt C C' ->
  tr_trm gt t t' ->
  tr_stack gt S S' ->
  tr_state gt m1 m1' ->
  red C S m1 t m2 v ->
  ~ is_error v ->
  exists v' m2',
      tr_val gt v v'
  /\  tr_state gt m2 m2'
  /\  red C' S' m1' t' m2' v'.
Proof.
  introv HC Ht HS Hm1 HR He. gen C' t' S' m1'. induction HR; intros;
  try solve [ forwards*: He; unfolds* ].
  { (* var *)
    inverts Ht. forwards* (v'&H'&Hv'): stack_lookup_tr HS H.
    exists* v' m1'. }
  { (* val *)
    inverts Ht. exists* v' m1'. }
  { (* if *)
    inverts Ht as Hb HTrue HFalse.
    forwards* (v'&m2'&Hv'&Hm2'&HR3): IHHR1 Hb HS Hm1.
    introv HN. inverts HN.
    inverts* Hv'.
    forwards* (vr'&m3'&Hvr'&Hm3'&HR4): IHHR2 HS Hm2'. 
    2: exists* vr' m3'. case_if*. }
  { (* let *)
    inverts Ht as Ht1 Ht2.
    forwards* (v'&m2'&Hv'&Hm2'&HR3): IHHR1 Ht1 HS Hm1.
    forwards HS': tr_stack_add z HS Hv'.
    forwards* (vr'&m3'&Hvr'&Hm3'&HR4): IHHR2 Ht2 HS' Hm2'.
    forwards: not_tr_val_error Hv'.
    exists* vr' m3'. }
  { (* binop *)
    inverts Ht as Ht1 Ht2.
    inverts Ht1 as Ht1. inverts Ht2 as Ht2.
    inverts H;
    try solve [ exists __ m1'; splits*;
    inverts Ht1; inverts Ht2;
    repeat constructors* ].
    (* equality case *)
    exists (val_bool (isTrue (v' = v'0))) m1'.
    splits*.
    { tests Heq: (v1 = v2).
      { forwards: functional_tr_val Ht1 Ht2. subst.
        repeat rewrite* isTrue_var_eq.  }
      { forwards*: neq_tr.
        repeat rewrite* isTrue_var_neq. } }
    { repeat constructors*. 
      forwards*: not_is_error_tr Ht1.
      forwards*: not_is_error_tr Ht2. } }
  { (* get *)
    inverts Ht as _ Hp. subst.
    inverts Hm1 as HD Htrm.
    inverts H0 as Hi Ha.
    forwards Htrml: Htrm Hi.
    inverts Hp as Hp. inverts Hp as Hπ.
    forwards (w'&Hw'&Ha'): tr_read_accesses Htrml Hπ Ha.
    exists w' m1'. splits*.
    repeat constructors~. rewrite~ <- HD.
    applys* not_is_uninitialized_tr. }
  { (* set *)
    inverts Ht as Hp Ht. subst.
    inverts Hm1 as HD Htrm.
    inverts H2 as Hin HW. 
    forwards Htrml: Htrm Hin.
    inverts Hp as Hp. 
    inverts Hp as Hπ.
    inverts Ht as Hv.
    forwards (w'&Hw'&HW'): tr_write_accesses Htrml Hv Hπ HW.
    exists val_unit m1'[l:=w']. splits~.
    { constructors.
      { unfold state. repeat rewrite~ dom_update.
        fold state. rewrite~ HD. }
      { introv Hi'. rew_reads~. intros. applys Htrm.  
        applys~ indom_update_inv_neq Hi'. } }
    { constructors~. applys* not_tr_val_error.
      constructors*. rewrite~ <- HD. } }
  { (* new *) 
    inverts Ht. subst.
    inverts Hm1 as HD Htrm. 
    forwards* (v'&Hv'&Hu): tr_uninitialized_val HC.
    exists (val_abstract_ptr l0 nil) m1'[l0:=v']. splits~.    
    { constructors.
      { unfold state. repeat rewrite~ dom_update.
        fold state. rewrite~ HD. }
      { introv Hin. unfolds state. rew_reads; intros; eauto. } }
    { constructors~. rewrite~ <- HD. auto. } }  
  { (* new_array *)
    inverts Ht as _ Ht.
    inverts Ht as Hv.
    inverts Hv. 
    inverts Hm1 as HD Htrm. subst.
    forwards* (v''&Hv''&Hu): tr_uninitialized_val HC.
    exists (val_abstract_ptr l0 nil) m1'[l0:=v'']. splits~.
    { constructors.
      { unfold state. repeat rewrite~ dom_update.
        fold state. rewrite~ HD. }
      { introv Hin. unfolds state. rew_reads; intros; eauto. } }
    { applys~ red_new_array. rewrite~ <- HD. auto. } }
  { (* struct_access *)
    inverts Ht as; inverts Hm1 as HD Htrm.
    { (* accessing grouped field *)
      introv Ht Hf. subst.
      inverts Ht as Hv. inverts Hv as Ha.
      remember (group_tr_struct_name gt) as Ts.
      remember (group_tr_new_struct_name gt) as Tsg.
      remember (group_tr_new_struct_field gt) as fg.
      remember (access_field Ts fg) as a1.
      remember (access_field Tsg f) as a2.
      exists (val_abstract_ptr l (π'++(a1::a2::nil))) m1'.
      splits*.
      { constructors. applys* tr_accesses_app. subst*. }
      { subst. applys* red_args_1. applys* red_struct_access.
        fequals*. rew_list*. } }
    { (* accessing another field *) 
      introv Ht Hor. subst. inverts Ht as Hv. inverts Hv as Ha.
      exists (val_abstract_ptr l (π'++(access_field T f :: nil))) m1'.
      splits; constructors*. applys* tr_accesses_app. }
    { introv HN. forwards*: HN. } }
  { (* array_access *)
    inverts Ht as Ht Hti. subst.
    inverts Ht as Hv. inverts Hv as Ha.
    inverts Hti as Hv. inverts Hv.
    inverts Hm1 as HD Htrm.
    exists (val_abstract_ptr l (π'++(access_array i::nil))) m1'.
    splits; constructors*. applys* tr_accesses_app. }
  { (* args_1 *)
    inverts Ht; forwards* (v'&m2'&Hv'&Hm2'&HR'): IHHR1;
    forwards*: not_is_error_args_1 HR2 He;
    forwards* (v''&m3'&Hv''&Hm3'&HR''): IHHR2;
    exists v'' m3'; splits*;
    try solve [ applys* red_args_1; applys* not_is_val_tr ].
    (* remaining cases are structs *)
    inverts HR''.
    { tryfalse. }
    { applys* red_args_1. applys* red_args_1.
      apply not_is_val_tr with (gt:=gt) (t1:=t1); auto. }
    { forwards HN: not_is_error_tr He Hv''. forwards*: HN. unfolds*. }
    { forwards HN: not_is_error_tr He Hv''. forwards*: HN. unfolds*. } }
  { (* args_2 *)
    inverts Ht as Ht1 Ht2.
    forwards* (v'&m2'&Hv'&Hm2'&HR'): IHHR1.
    forwards*: not_is_error_args_2 HR2 He.
    forwards* (v''&m3'&Hv''&Hm3'&HR''): IHHR2.
    exists v'' m3'. splits*.
    inverts Ht1. applys* red_args_2.
    applys* not_is_val_tr. }
Qed.

End TransformationsProofs.
