(**

This file describes transformations of the layout of records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export Semantics LibSet LibMap.



(* ********************************************************************** *)
(* * Specification of the transformation *)

Module Example.

(* Initial typdefctx *)
Definition pos : typdef_struct := (\{})["x" := typ_int]["y" := typ_int]["z" := typ_int].
Definition C : typdefctx := (\{})["pos" := pos].

(* Final typdefctx *)
Definition struct_x : typdef_struct := (\{})["x" := typ_int].
Definition pos' : typdef_struct := (\{})["s" := (typ_struct "struct_x")]["y" := typ_int]["z" := typ_int].
Definition C' : typdefctx := (\{})["pos" := pos'].

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
        index s f ->
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
        index m l ->
        tr_val gt m[l] m'[l]) ->
      tr_state gt m m'.


(* ********************************************************************** *)
(* * Correctness of the transformation *)

Section TransformationsProofs.


(* ---------------------------------------------------------------------- *)
(** Hints *)

Lemma index_of_index_length' : forall A (l' l : list A) i,
  index l' i ->
  length l' = length l ->
  index l i.
Proof.
  intros. rewrite index_eq_index_length in *.
  applys* index_of_index_length'.
Qed.

Hint Resolve index_of_index_length'.

Hint Constructors red tr_trm tr_val tr_accesses tr_state tr_stack 
                  read_accesses write_accesses.


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

Generalizable Variables A B.
Axiom map_ext : forall A `{ Inhab B } (m1 m2:map A B),
  dom m1 = dom m2 ->
  (forall k, k \indom m1 -> m1[k] = m2[k]) ->
  m1 = m2.

Ltac name_fun_occ F I :=
  match goal with H: context[F ?a] |- _ => 
    match get_head H with F =>
      sets I: (H a) end end.

Theorem functional_tr_val : forall gt v v1 v2,
  tr_val gt v v1 ->
  tr_val gt v v2 ->
  v1 = v2.
Proof using.
  introv H1 H2. gen v2. induction H1; intros;
  inverts_head tr_val; fequals*.
  { applys* functional_tr_accesses. }
  { applys* eq_of_extens. math. }
  { applys* map_ext.
    { inverts_head make_group_tr'. congruence. }
    { introv Hin. tests C: (k = fg).
      { . }
      {  } }
    (*subst. inverts_head make_group_tr'. fequals. 
    applys* map_ext. rew_set in *. intuition. {  } intuition. auto.
    { admit. } 
    { admit. } }
  { subst. simpls. contradiction.*) admit. }
  { admit. }
Admitted.

Theorem functional_tr_trm : forall gt t t1 t2,
  tr_trm gt t t1 ->
  tr_trm gt t t2 ->
  t1 = t2.
Proof. (* TODO: use inverts_head *)
  introv H1 H2. gen t2. induction H1; intros;
  try solve [ inverts H2 ; try subst ; repeat fequals* ]; admit.
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

Lemma not_is_error_args_1 : forall S m op ts m' v w,
  red S m (trm_app op (trm_val w :: ts)) m' v ->
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

Lemma not_is_error_args_2 : forall S m op t ts m' v w,
  red S m (trm_app op (t :: trm_val w :: ts)) m' v ->
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


Lemma in_union : forall A (x:A) (S1 S2:set A), 
  x \in S1 \/ x \in S2 -> 
  x \in S1 \u S2.
Proof using. set_prove. Qed.

Lemma in_setminus : forall A (x:A) (S1 S2: set A),
  x \in S1 -> 
  x \notin S2 -> 
  x \in (S1 \- S2).
Proof using. set_prove. Qed.

Lemma in_notin_neq : forall A (x y:A) (S:set A),
  x \in S ->
  y \notin S ->
  y <> x.
Proof using. set_prove. Qed.

Lemma notin_notin_subset : forall A (x:A) (S1 S2:set A),
  S1 \c S2 ->
  x \notin S2 ->
  x \notin S1.
Proof using. set_prove. Qed.

Lemma in_subset : forall A (x:A) (S1 S2:set A),
  S2 \c S1 ->
  x \in S2 ->
  x \in S1.
Proof using. set_prove. Qed.

Lemma in_single : forall A (x:A) (S:set A),
  S = '{x} ->
  x \in S.
Proof using. set_prove. Qed.

Lemma union_eq : forall A (S1 S2 S3 S4:set A),
  S1 = S3 ->
  S2 = S4 ->
  S1 \u S2 = S3 \u S4.
Proof using. set_prove. Qed.


(* ---------------------------------------------------------------------- *)
(** Auxiliary map results *)

Section MapProperties.
Generalizable Variables A B.

Axiom map_ext : forall A `{Inhab B} (m1 m2:map A B),
  dom m1 = dom m2 ->
  (forall i, index m1 i -> m1[i] = m2[i]) ->
  m1 = m2.

End MapProperties.


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
    inverts Ha. exists* v'. } 
  { (* array_access *)
    inverts Ha as Ha. inverts Hv as Hl Htr.
    forwards Htra: Htr H. 
    forwards (w'&Hw'&Hπ'): IHHR Htra Ha.
    exists* w'. }
  { (* struct_access *) 
    inverts Ha as; inverts Hv as; 
    try solve [ intros ; false ].
    { (* one of the fields to group *) 
      introv HD1 Hgt HD2 HD3 Hsg Hfs HB Ha Hin.
      rewrite* Hgt in Hin. simpls.
      forwards Hsf: Hsg Hin.
      forwards Heq: read_of_binds H. 
      subst_hyp Heq.
      forwards (w'&Hw'&HR'): IHHR Hsf Ha.
      exists* w'. splits*.
      constructors*; rewrite Hgt; simpls*. 
      constructors*. applys* binds_of_indom_read. }
    { (* struct transformed but another field *) 
      introv HD1 HD2 HD3 Hsg Hfs HB Ha Hor.
      inverts Hor as Hf; simpl in Hf; tryfalse.
      forwards Hf': indom_of_binds H. typeclass.
      forwards Hsf: Hfs Hf Hf'.
      forwards Hv1: read_of_binds H. 
      subst_hyp Hv1.
      forwards (w'&Hw'&HR'): IHHR Hsf Ha.
      exists w'. splits*. constructors*. 
      applys* binds_of_indom_read.
      rewrite HD3. rew_set*.
(*   left*. applys* in_union. 
      left. applys* in_setminus. *) }
    { (* another struct *)
      intros Hn HD Hfs Ha Hor. 
      forwards Hidx: index_of_binds H. typeclass.
      forwards Hsf: Hfs Hidx. 
      forwards Heq: read_of_binds H.
      subst_hyp Heq. 
      forwards (w'&Hw'&HR'): IHHR Hsf Ha.
      exists w'. splits*. constructors*. 
      applys* binds_of_indom_read.
      rewrite <- HD. 
      rewrite* <- index_eq_indom. } }
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
  introv Hv1 Hw Ha HW. gen gt v1' w' π'. induction HW; intros.
  { (* nil *)
    inverts Ha. subst_hyp H. exists* w'. }
  { (* array_access *)
    inverts Ha as Ha. inverts Hv1 as Hl Htr.
    forwards Htra: Htr H.
    forwards (v2'&Hv2'&HW'): IHHW Htra Hw Ha.
    exists (val_array a'[i:=v2']).
    splits; constructors*.
    { (* val_array under tr *)
      rewrite H0. repeat rewrite* length_update. }
    { (* write_accesses of transformed array *) 
      introv Hi0. rewrite read_update_case. 
      { (* HERE, should do [case_if as C] *) case_if*; subst_hyp H0.
        { subst_hyp C. rewrites* LibListZ.read_update_same. }
        { forwards: index_of_update_neq' Hi0 C. 
          rewrites* LibListZ.read_update_neq. } }
      { rewrite index_eq_index_length in *.
        rewrite H0 in Hi0. rewrite length_update in Hi0.
        rewrite* <- Hl. } } }
  { (* struct_access *)
    inverts Ha as; inverts Hv1 as;
    try solve [ intros ; false ].
    { (* one of the fields to group *) 
      introv HD1 Hgt HD2 HD3 Hsg Hfs HB Ha Hin.
      rewrite* Hgt in Hin. simpls.
      forwards Hsf: Hsg Hin.
      forwards Heq: read_of_binds H. 
      subst_hyp Heq.
      forwards (v2'&Hv2'&HW'): IHHW Hsf Hw Ha.
      remember (group_tr_struct_name gt) as T.
      exists (val_struct T s'[fg:=(val_struct Tsg sg[f:=v2'])]).
      splits.
      { substs. 
        applys tr_val_struct_group (sg[f:=v2']); try reflexivity; 
        repeat rewrite dom_update_at_indom; try typeclass;
        try solve [ applys* in_subset ].
        (*Ltac rew_dom_at_core tt := repeat rewrite dom_update_at_indom.
        Tactic Notation "rew_dom_at" := rew_dom_at_core tt.
        Tactic Notation "rew_dom_at" "*" := rew_dom_at; auto_star.*)
        { forwards*: indom_of_binds HB. }
        { introv HD4. repeat rewrite read_update. case_if*. }
        { introv HD4 HD5. repeat rewrite read_update. 
          repeat case_if*; subst; contradiction. }
        { applys* binds_update_same. } }
      { constructors*; subst_hyp Hgt; simpls*.
        constructors*. applys* binds_of_indom_read. } }
    { (* struct transformed but another field *) 
      introv HD1 HD2 HD3 Hsg Hfs HB Ha Hor. 
      inverts Hor as Hf; simpl in Hf; tryfalse.
      forwards Hf': indom_of_binds H. typeclass.
      forwards Hsf: Hfs Hf Hf'.
      forwards Hv1: read_of_binds H. 
      subst_hyp Hv1.
      forwards (v2'&Hv2'&HW'): IHHW Hsf Hw Ha.
      exists (val_struct T s'[f:=v2']). splits.
      { applys* tr_val_struct_group; subst_hyp H0;
        try solve [ rewrite* dom_update_at_indom ].
        { repeat rewrite* dom_update_at_indom.
          rewrite HD3. applys* in_union. left.
          applys* in_setminus. }        
        { introv Hf0. rewrite read_update. case_if*.
          forwards: in_notin_neq Hf0 Hf. false. }
        { introv HD4 HD5. repeat rewrite read_update.
          case_if*. rewrite* dom_update_at_indom in HD5. }
        { applys* binds_update_neq.
          apply in_notin_neq with (S:=dom s1); auto. } }
      { constructors*.  applys* binds_of_indom_read.
        rewrite HD3. applys in_union. left.
        applys* in_setminus. } }
    { (* another struct *)
      intros Hn HD Hfs Ha Hor.
      forwards Hidx: index_of_binds H. typeclass.
      forwards Hsf: Hfs Hidx. 
      forwards Heq: read_of_binds H.
      subst_hyp Heq. 
      forwards (v2'&Hv2'&HW'): IHHW Hsf Hw Ha.
      exists (val_struct T s'[f:=v2']). splits.
      { constructors*; subst_hyp H0.
        { rewrite index_eq_indom in Hidx.
          rewrites* dom_update_at_indom.
          rewrite HD in Hidx.
          rewrites* dom_update_at_indom. }
        { introv Hif0. rewrite read_update. case_if*.
          { subst_hyp C. rewrite* read_update_same. }
          { rewrite* read_update_neq. 
            forwards Hif0': index_of_index_update_neq Hif0 C. auto.
            forwards*: Hfs Hif0'. } } }
      { constructors*. applys* binds_of_indom_read.
        rewrite index_eq_indom in Hidx.
        rewrite* <- HD. } } }
Qed.


(* ---------------------------------------------------------------------- *)
(** Correctness of the transformation *)

Axiom isTrue_var_eq : forall A (v1 v2:A), v1 = v2 -> isTrue (v1 = v2) = true.

Axiom isTrue_var_neq : forall A (v1 v2:A), v1 <> v2 -> isTrue (v1 = v2) = false. 

Theorem red_tr: forall gt t t' v S S' m1 m1' m2,
  tr_trm gt t t' ->
  tr_stack gt S S' ->
  tr_state gt m1 m1' ->
  red S m1 t m2 v -> 
  ~ is_error v -> 
  exists v' m2',
      tr_val gt v v'
  /\  tr_state gt m2 m2'
  /\  red S' m1' t' m2' v'.
Proof.
  introv Ht HS Hm1 HR He. gen t' S' m1'. induction HR; intros;
  try solve [ forwards*: He; unfolds* ].
  { (* var *)
    inverts Ht. inverts HS. 
    forwards: H1 H. inverts H2. exists* x0 m1'. }
  { (* val *)
    inverts Ht. exists* v' m1'. }
  { (* if *)
    inverts Ht as Hb HTrue HFalse. 
    forwards (v'&m2'&Hv'&Hm2'&HR3): IHHR1 Hb HS Hm1.
    introv HN. inverts HN.
    inverts* Hv'.
    forwards* (vr'&m3'&Hvr'&Hm3'&HR4): IHHR2 HS Hm2'. 2: 
    exists* vr' m3'. case_if*. } 
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
    { tests C: (v1 = v2). 
      { forwards: functional_tr_val Ht1 Ht2. subst.
        repeat rewrite* isTrue_var_eq. }
      { forwards*: neq_tr.
        repeat rewrite* isTrue_var_neq. } }
    { repeat constructors*. 
      forwards*: not_is_error_tr Ht1.
      forwards*: not_is_error_tr Ht2. } }
  { (* get *)
    inverts Ht as _ Hp. inverts Hm1 as HD Htrm.
    inverts H0 as Hb Ha. forwards Hi: index_of_binds Hb.
    typeclass. forwards Htrml: Htrm Hi.
    subst_hyp H. inverts Hp as Hp. inverts Hp.
    forwards: read_of_binds Hb. subst_hyp H.
    forwards (w'&Hw'&Ha'): tr_read_accesses Htrml H2 Ha.
    exists w' m1'. splits*. 
    constructors*. constructors*.
    applys* binds_of_indom_read. 
    rewrite <- HD at 1.
    forwards*: indom_of_binds Hb. }
  { (* set *)
    inverts Ht as Hp Ht. inverts Hm1 as HD Htrm.
    inverts H2 as Hb HW. forwards Hi: index_of_binds Hb.
    typeclass. forwards Htrml: Htrm Hi. subst_hyp H.
    inverts Hp as Hp. inverts Hp as Ha.
    subst_hyp H0. inverts Ht as Hv. 
    forwards Heq: read_of_binds Hb. subst_hyp Heq.
    forwards (w'&Hw'&HW'): tr_write_accesses Htrml Hv Ha HW.
    exists val_unit m1'[l:=w']. splits*.
    { constructors. 
      { rewrite index_eq_indom in Hi.
        forwards* HDm1: dom_update_at_indom Hi.
        rewrite HD in Hi at 1.
        forwards* HDm1': dom_update_at_indom Hi.
        rewrite* HDm1. rewrite* HDm1'. }
      { introv Hi'. do 2 rewrites read_update.
        case_if*. 
        forwards* Hi'': index_of_index_update_neq Hi' C. } }
    { constructors*. applys* not_tr_val_error.
      constructors*. applys* binds_of_indom_read.
      rewrite <- HD at 1. forwards*: indom_of_binds Hb. } }
  { (* new *) 
    inverts Ht as _ Ht. inverts Ht as Hv. 
    inverts Hm1 as HD Htrm. subst.
    exists (val_abstract_ptr l0 nil) m1'[l0:=v'].
    splits*.
    { constructors. 
      { unfold state. repeat rewrite dom_update. 
        applys* union_eq.  }
      { introv Hi. rewrite read_update. case_if*.
        { subst_hyp C. rewrite* read_update_same. }
        { rewrite* read_update_neq. 
          forwards* Hi': index_of_index_update_neq Hi C. } } }
    { constructors*. 
      { forwards*: not_is_error_tr v. } 
      { rewrite* <- HD. } } }
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
    forwards*: red_app_not_is_error HR2 He;
    forwards* (v''&m3'&Hv''&Hm3'&HR''): IHHR2;
    exists v'' m3'; splits*;
    try solve [ applys* red_args_1; applys* not_is_val_tr ].
    (* remaining cases are structs *)
    inverts HR''.
    { inverts H10. }
    { applys* red_args_1. applys* red_args_1. 
      apply not_is_val_tr with (gt:=gt) (t1:=t1); auto. }
    { forwards HN: not_is_error_tr He Hv''. forwards*: HN. unfolds*. }
    { forwards HN: not_is_error_tr He Hv''. forwards*: HN. unfolds*. } }
  { (* args_2 *) 
    inverts Ht as Ht1 Ht2.
    forwards* (v'&m2'&Hv'&Hm2'&HR'): IHHR1.
    forwards*: red_app_not_is_error_2 HR2 He.
    forwards* (v''&m3'&Hv''&Hm3'&HR''): IHHR2. 
    exists v'' m3'. splits*. 
    inverts Ht1. applys* red_args_2.
    applys* not_is_val_tr. }
Qed.

End TransformationsProofs.
