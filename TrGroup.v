(**

This file describes transformations of the layout of records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export Semantics LibSet LibMap Typing TLCbuffer.



(* ********************************************************************** *)
(* * Specification of the transformation *)

Module Example.

(* Initial typdefctx *)
Definition pos : map field typ := (\{})["x" := typ_int]["y" := typ_int]["z" := typ_int].
Definition C : typdefctx := (\{})["pos" := (typ_struct pos)].

(* Final typdefctx *)
Definition struct_x : map field typ := (\{})["x" := typ_int].
Definition pos' : map field typ := (\{})["s" := (typ_var "struct_x")]["y" := typ_int]["z" := typ_int].
Definition C' : typdefctx := (\{})["pos" := (typ_struct pos')]["struct_x" := (typ_struct struct_x)].

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

(** Checking if the transformation is acceptable *)

(*Record group_tr_ok (C:typdefctx) (gt:group_tr) := group_tr_ok_make {
  let (Tt,fs,Tg,fg) := gt in 
  group_tr_ok_Tt : Tt \indom C;
}.*)


Inductive group_tr_ok : group_tr -> typdefctx -> Prop :=
  | group_tr_ok_intros : forall Tfs Tt fs fg Tg gt C,
      gt = make_group_tr Tt fs Tg fg ->
      Tt \indom C ->
      C[Tt] = typ_struct Tfs ->
      Tg \notindom C ->
      fs \c dom Tfs ->
      fg \notindom Tfs ->
      (forall Tv,
        Tv \indom C ->
        Tv <> Tt ->
        ~ free_typvar C Tt C[Tv]) ->
      group_tr_ok gt C.

(*Lemma group_tr_ok_Tt : 
   forall Tt fs Tg fg,
   group_tr_ok C gt ->
   gt = make_group_tr Tt fs Tg fg ->
   Tt \indom C.

Lemma group_tr_ok_Tt : 
   forall Tt fs Tg fg,
   group_tr_ok C (make_group_tr Tt fs Tg fg) ->
   Tt \indom C.
*)


(* ********************************************************************** *)
(* * The transformation applied to the different constructs. *)

(** Transformation of typdefctxs: C ~ |C| *)

Inductive tr_struct_map (gt:group_tr) : map field typ -> map field typ ->
                                        map field typ  -> Prop :=
  | tr_struct_map_intro : forall Tfs Tfs' Tfs'' Tt fs Tg fg,
      gt = make_group_tr Tt fs Tg fg ->
      dom Tfs' = (dom Tfs \- fs) \u \{fg} ->
      Tfs'[fg] = typ_var Tg ->
      (forall f,
        f \indom Tfs ->
        f \notin fs ->
        Tfs'[f] = Tfs[f]) ->
      dom Tfs'' = fs ->
      (forall f,
        f \indom Tfs'' ->
        Tfs''[f] = Tfs[f]) ->
      tr_struct_map gt Tfs Tfs' Tfs''.

Inductive tr_typdefctx (gt:group_tr) : typdefctx -> typdefctx -> Prop :=
  | tr_typdefctx_intro : forall Tfs Tfs' Tfs'' Tt fs Tg fg C C',
      gt = make_group_tr Tt fs Tg fg ->
      dom C' = dom C \u \{Tg} ->
      C[Tt] = typ_struct Tfs ->
      C'[Tt] = typ_struct Tfs' ->
      C'[Tg] = typ_struct Tfs'' ->
      (forall T,
        T \indom C ->
        T <> Tt ->
        C'[T] = C[T]) ->
      tr_struct_map gt Tfs Tfs' Tfs'' ->
      tr_typdefctx gt C C'.

(** Transformation of paths: π ~ |π| *)

Inductive tr_accesses (gt:group_tr) : accesses -> accesses -> Prop :=
  | tr_accesses_nil :
      tr_accesses gt nil nil
  | tr_accesses_array : forall π π' T i,
      tr_accesses gt π π' ->
      tr_accesses gt ((access_array T i)::π) ((access_array T i)::π')
  | tr_accesses_field_group : forall Tt fs fg Tg f a0 π a1 a2 π',
      tr_accesses gt π π' ->
      gt = make_group_tr Tt fs Tg fg ->
      f \in fs ->
      a0 = access_field (typ_var Tt) f ->
      a1 = access_field (typ_var Tt) fg ->
      a2 = access_field (typ_var Tg) f ->
      tr_accesses gt (a0::π) (a1::a2::π')
  | tr_accesses_field_other : forall T Tt π π' f,
      tr_accesses gt π π' ->
      Tt = group_tr_struct_name gt ->
      (T <> (typ_var Tt) \/ f \notin (group_tr_fields gt)) ->
      tr_accesses gt ((access_field T f)::π) ((access_field T f)::π').

(** Transformation of values: v ~ |v| *)

Inductive tr_val (gt:group_tr) : val -> val -> Prop :=
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
  | tr_val_array : forall a T a',
      length a = length a' ->
      (forall i,
        index a i ->
        tr_val gt a[i] a'[i]) ->
      tr_val gt (val_array T a) (val_array T a')
  | tr_val_struct_group : forall Tt Tg s s' fg fs sg,
      gt = make_group_tr Tt fs Tg fg ->
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
      s'[fg] = val_struct (typ_var Tg) sg ->
      tr_val gt (val_struct (typ_var Tt) s) (val_struct (typ_var Tt) s')
  | tr_val_struct_other : forall Tt T s s',
      Tt = group_tr_struct_name gt ->
      T <> (typ_var Tt) ->
      dom s = dom s' ->
      (forall f,
        f \indom s ->
        tr_val gt s[f] s'[f]) ->
      tr_val gt (val_struct T s) (val_struct T s').

(* Transformation used in the struct cases to avoid repetition. *)

Inductive tr_struct_op (gt:group_tr) : trm -> trm -> Prop :=
  | tr_struct_op_group : forall fs Tt fg pr Tg f op1 op2 ts,
      pr = prim_struct_access \/ pr = prim_struct_get ->
      gt = make_group_tr Tt fs Tg fg ->
      f \in fs ->
      op1 = pr (typ_var Tt) fg ->
      op2 = pr (typ_var Tg) f ->
      tr_struct_op gt (trm_app (pr (typ_var Tt) f) ts) (trm_app op2 ((trm_app op1 ts)::nil))
  | tr_struct_op_other : forall Tt T pr f ts,
      pr = prim_struct_access \/ pr = prim_struct_get ->
      Tt = group_tr_struct_name gt ->
      (T <> (typ_var Tt) \/ f \notin (group_tr_fields gt)) ->
      tr_struct_op gt (trm_app (pr T f) ts) (trm_app (pr T f) ts).

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
  (* Special case: structs *)
  | tr_trm_struct_op : forall t1' op t1 tr,
      is_struct_op op ->
      tr_trm gt t1 t1' ->
      tr_struct_op gt (trm_app op (t1'::nil)) tr ->
      tr_trm gt (trm_app op (t1::nil)) tr
  (* Args *)
  | tr_trm_args0 : forall op,
      tr_trm gt (trm_app op nil) (trm_app op nil)
  | tr_trm_args1 : forall op t1 t1',
      ~ is_struct_op op ->
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


(* ---------------------------------------------------------------------- *)
(** Hints *)

Hint Resolve TLCbuffer.index_of_index_length.

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
  inverts_head access_field; subst; simpls;
  inverts_head Logic.or; repeat fequals*.
Qed. 

Theorem functional_tr_val : forall gt v v1 v2,
  tr_val gt v v1 ->
  tr_val gt v v2 ->
  v1 = v2.
Proof using.
  introv H1 H2. gen v2. induction H1; intros;
  inverts_head tr_val; fequals*; subst; simpls; tryfalse.
  { fequals. applys* functional_tr_accesses. }
  { applys* eq_of_extens. math. introv Hi.
    asserts: (index a i).
    { rewrite index_eq_index_length in *. rewrite~ H. }
    applys~ H1. }
  { applys read_extens. 
    { inverts_head make_group_tr'. congruence. }
    { introv Hin. tests C: (i = fg).
      { inverts_head make_group_tr'.
        asserts_rewrite~ (s'0[fg0] = val_struct (typ_var Tg0) sg0).
        asserts_rewrite~ (s'[fg0] = val_struct (typ_var Tg0) sg).
        fequals. applys~ read_extens. introv Hk. 
        asserts_rewrite* (dom sg = dom sg0) in *. }
      { inverts_head make_group_tr'.
        asserts_rewrite~ (dom s' = dom s \- dom sg \u '{fg0}) in Hin.
        inverts Hin as Hin; tryfalse. inverts Hin as Hin Hnotin.
        asserts_rewrite* (dom sg = dom sg0) in *. } } }
  { applys read_extens.
    { congruence. }
    { introv Hin. 
      asserts_rewrite* (dom s' = dom s) in *. } }
Qed.

Lemma functional_tr_struct_op : forall gt op t1 tr1 tr2,
  is_struct_op op ->
  tr_struct_op gt (trm_app op (t1 :: nil)) tr1 ->
  tr_struct_op gt (trm_app op (t1 :: nil)) tr2 ->
  tr1 = tr2.
Proof.
  introv Hop Htr1 Htr2. induction Htr1; subst; 
  inverts H; inverts Htr2; inverts_head Logic.or;
  try inverts_head make_group_tr'; simpls;
  try inverts_head prim_struct_access;
  try inverts_head prim_struct_get;
  repeat fequals*.
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
    { forwards~: IHtr_trm t1'0. subst.
      applys* functional_tr_struct_op. }
    { false. } }
Qed.

Theorem functional_tr_stack_item : forall gt i i1 i2,
  tr_stack_item gt i i1 ->
  tr_stack_item gt i i2 ->
  i1 = i2.
Proof.
  introv Hi1 Hi2. 
  inverts Hi1 as H. inverts Hi2 as H'. 
  forwards*: functional_tr_val H H'.
Qed.

Theorem functional_tr_stack : forall gt S S1 S2,
  tr_stack gt S S1 ->
  tr_stack gt S S2 ->
  S1 = S2.
Proof.
  introv HS1 HS2. inverts HS1 as HS1. inverts HS2 as HS2. 
  gen S2. induction HS1; intros.
  { inverts~ HS2. }
  { inverts HS2 as HSy0 HS1'. fequals.
    { forwards*: functional_tr_stack_item H HSy0. }
    { applys~ IHHS1. } }
Qed.

Theorem functional_tr_state : forall gt m m1 m2,
  tr_state gt m m1 ->
  tr_state gt m m2 ->
  m1 = m2.
Proof.
  introv Hm1 Hm2. 
  inverts Hm1 as HD1 Htr1. inverts Hm2 as HD2 Htr2.
  applys read_extens.
  { unfolds state. congruence. }
  { introv Hi. rewrite HD1 in *. 
    forwards Hm1i: Htr1 Hi. forwards Hm2i: Htr2 Hi.
    forwards~: functional_tr_val Hm1i Hm2i. }
Qed.


(* ********************************************************************** *)
(* * Correctness of the transformation *)

Section TransformationsProofs.

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

Lemma is_basic_tr : forall gt v1 v2,
  tr_val gt v1 v2 ->
  is_basic v1 ->
  is_basic v2.
Proof.
  introv Htr Hv1. induction Htr;
  try solve [ inverts Hv1 ];
  constructors~.
Qed.

Lemma not_is_val_tr : forall gt t1 t2,
  tr_trm gt t1 t2 ->
  ~ is_val t1 ->
  ~ is_val t2.
Proof.
  introv Htr Hv. induction Htr; introv HN;
  try solve [ subst ; inverts HN ]. forwards*: Hv.
  inverts_head tr_struct_op; inverts HN.
Qed.

Lemma not_is_error_tr : forall gt v1 v2,
  tr_val gt v1 v2 ->
  ~ is_error v1 ->
  ~ is_error v2.
Proof.
  introv Htr He. induction Htr; introv HN;
  try solve [ subst ; inverts HN ]. 
Qed.

Lemma not_is_uninitialized_tr : forall gt v v',
  tr_val gt v v' -> 
  ~ is_uninitialized v ->
  ~ is_uninitialized v'.
Proof.
  introv Htr Hu. induction Htr; introv HN;
  subst; inverts HN. forwards~: Hu.
Qed.


Lemma tr_accesses_inj : forall C gt π π1 π2,
  group_tr_ok gt C ->
  wf_accesses C π1 ->
  wf_accesses C π2 ->
  tr_accesses gt π1 π ->
  tr_accesses gt π2 π ->
    π1 = π2.
Proof.
  introv Hok Hva1 Hva2 Hπ1 Hπ2. gen C π2. induction Hπ1; intros.
  { inverts Hπ2. auto. }
  { inverts Hπ2; inverts Hva1; inverts Hva2.
    { fequals. applys* IHHπ1. }
    { fequals. } }
  { subst. inverts Hπ2; inverts Hva1; inverts Hva2.
    { fequals. applys* IHHπ1. }
    { simpls. inverts Hok as Hgt. inverts Hgt.
      inverts_head Logic.or; tryfalse. inverts H4.
      { inverts_head make_group_tr'. fequals. }
      { inverts_head wf_accesses. inverts_head wf_typ. 
        false*. } } }
  { inverts Hπ2; inverts Hva1; inverts Hva2.
    { inverts Hok as Hgt. inverts Hgt. inverts_head access_field.
      subst. simpls. inverts_head Logic.or; tryfalse.
      inverts_head wf_typ. inverts Hπ1.
      { inverts H7. inverts_head make_group_tr'. 
        inverts H18. inverts H3; fequals. }
      { simpls. inverts_head Logic.or.
        { inverts H7. inverts H16. false*. }
        { false*. } } }
    { fequals. applys* IHHπ1. } }
Qed.

Lemma tr_val_inj : forall C gt v v1 v2,
  group_tr_ok gt C ->
  wf_val C v1 ->
  wf_val C v2 ->
  tr_val gt v1 v ->
  tr_val gt v2 v ->
  v1 = v2.
Proof.
  introv Hok HV1 HV2 Hv1 Hv2. gen C v2. induction Hv1; intros;
  try solve [ inverts Hv2; repeat fequals*; subst; simpls; tryfalse* ].
  { inverts Hv2 as Hπ. repeat fequals*.
    inverts HV1 as HRφ1. inverts HV2 as HRφ2.
    applys* tr_accesses_inj. }
  { inverts Hv2 as Hl Htra. fequals. 
    applys* eq_of_extens. 
    { congruence. }
    { inverts HV1. inverts HV2. introv Hi. 
      asserts: (index a0 i).
      { rewrite index_eq_index_length in *. rewrite Hl. rewrite~ <- H. }
      applys* H1. } }
  { subst. inverts Hv2 as.
    { introv HDsg0 Hgt Hfg0in HDs' Hsg0f Hs'f Hs'fg0.
      inverts Hgt as HDsg. rewrite <- HDsg in *. 
      fequals. asserts HD: (dom s = dom s0). 
      { rewrite HDs' in *.
        applys~ incl_eq (dom s) (dom s0) (dom sg).
        rew_set in *. intros x. forwards Hiff: H2 x.
        rew_set in *. inverts Hiff as HD1 HD2.
        intuition; subst; tryfalse*. }
      applys* read_extens.
      { introv Hi. inverts HV1 as HV1 HV1sf. inverts HV2 as HV2 HV2sf.
        forwards HV1sf': HV1sf Hi. rewrite HD in Hi. forwards HV2sf': HV2sf Hi.
        rewrite Hs'fg0 in H8. inverts H8. tests: (i \indom sg).
        { applys* H5. }
        { applys* H7. rewrite~ HD. } } }
    { introv HN. simpls. false. } }
  { inverts Hv2 as.
    { intros. subst. simpls. false. }
    { introv Hneq HDs0 Hs'f. fequals. asserts HD: (dom s = dom s0).
      { congruence. }
      applys~ read_extens.
      { introv Hi. inverts HV1 as HV1 HV1sf. inverts HV2 as HV2 HV2sf. 
        rewrite <- HD in HV2sf. applys* H3. applys Hs'f.
        rewrite~ <- HD. } } }
Qed.

Lemma tr_val_inj_cp : forall C gt v1 v2 v1' v2',
  group_tr_ok gt C ->
  wf_val C v1 ->
  wf_val C v2 ->
  tr_val gt v1 v1' ->
  tr_val gt v2 v2' ->
  v1 <> v2 ->
  v1' <> v2'.
Proof.
  introv Hok HTv1 HTv2 Hv1 Hv2 Hneq HN. subst. 
  forwards*: tr_val_inj Hok HTv1 HTv2 Hv1.
Qed.

Lemma not_tr_val_error : forall gt v1 v2,
  tr_val gt v1 v2 ->
  ~ is_error v2.
Proof.
  introv Hv He. unfolds is_error.
  destruct* v2. inverts* Hv.
Qed.

Lemma tr_stack_add : forall gt z v S v' S',
  tr_stack gt S S' ->
  tr_val gt v v' ->
  tr_stack gt (Ctx.add z v S) (Ctx.add z v' S').
Proof.
  introv HS Hv. constructors~. inverts HS.
  unfolds Ctx.add. destruct* z.
  applys~ Forall2_cons. constructors~.
Qed.


(* ---------------------------------------------------------------------- *)
(** The transformation preserves well-founded types. *)

Lemma tr_typdefctx_wf_typ : forall gt C C' T,
  group_tr_ok gt C ->
  tr_typdefctx gt C C' ->
  wf_typ C T ->
  wf_typ C' T.
Proof.
  introv Hok HC HT. induction HT; try solve [ constructors* ].
  inverts Hok as HTt HCTt HTg Hfs Hfg Hfv. 
  inverts HC as Hgt HDC' HCTt0 HC'Tt0 HC'Tg0 HC'T Htrsm.
  inverts Htrsm as Hgt' HDTfs' HTfs'fg0 HTfs'f HTfs''f.
  inverts Hgt. inverts Hgt'. constructors.
  { rewrite HDC'. rew_set~. }
  { tests: (Tv=Tt1).
    { rewrite HC'Tt0. constructors. introv Hfin.
      rewrite HCTt0 in HCTt. inverts HCTt.
      rewrite HCTt0 in IHHT. inverts IHHT as HTfsf.
      tests: (f=fg1).
      { rewrite HTfs'fg0. constructors.
        { rewrite HDC'. rew_set~. }
        rewrite HC'Tg0. constructors~. introv Hfin'.
        rewrite~ HTfs''f. applys~ HTfsf. rew_set in Hfs.
        applys~ Hfs. }
      { tests: (f \indom Tfs'').
        { rewrite HDTfs' in Hfin. rew_set in Hfin.
          inverts Hfin as Hfin; tryfalse. destruct Hfin.
          false*. }
        { asserts Hfin': (f \indom Tfs).
          { rewrite HDTfs' in Hfin.
            rew_set in Hfin. inverts Hfin as Hfin; tryfalse.
            destruct~ Hfin. }
          rewrite~ HTfs'f. } } }
    { rewrite~ HC'T. } }
Qed.


(* ---------------------------------------------------------------------- *)
(** uninitialized is coherent with the transformation *)

Lemma tr_typing_array : forall gt C C' Ta T os,
  group_tr_ok gt C ->
  tr_typdefctx gt C C' ->
  typing_array C Ta T os ->
  typing_array C' Ta T os.
Proof.
  introv Hok HC HTa. gen gt C'. induction HTa; intros;
  try solve [ inverts~ HTa ].
  { constructors~. applys* tr_typdefctx_wf_typ. }
  { inverts HC as HD HCTt HC'Tt HC'Tg HC'T Htrsm.
    tests: (Tv=Tt).
    { inverts HTa as.
      { introv Hwf HTa. rewrite HCTt in HTa. inverts HTa. }
      { introv HDC HTa HTv. rewrite HCTt in HTv. inverts HTv. } }
    { constructors~.
      { rewrite HD. rew_set~. }
      { rewrite~ HC'T. applys* IHHTa. constructors*. } } }
Qed.

Lemma tr_typing_struct : forall Tt fg Tg fs C C' Ts Tfs,
  tr_typdefctx (make_group_tr Tt fs Tg fg) C C' ->
  wf_typdefctx C ->
  ~ free_typvar C Tt Ts ->
  typing_struct C Ts Tfs ->
  typing_struct C' Ts Tfs.
Proof.
  introv HC Hwf Hfv HTs. gen Tt fg Tg fs. induction HTs; intros.
  { constructors~. }
  { inverts HC as Hgt HDC' HCTt HC'Tt HC'Tg HC'T Htrsm.
    inverts Htrsm as Hgt' HDTfs' HTfs'fg HTfs'f HTfs''f.
    inverts Hgt. inverts Hgt'.
    simpls. constructors~.
    { rewrite HDC'. rew_set~. }
    { tests: (Tv=Tt1).
      { false. applys Hfv. constructors~. }
      { rewrite~ HC'T. applys IHHTs.
        { introv HN. applys Hfv. constructors~. eapply HN. }
        { repeat constructors*. } } } }
Qed.

Lemma tr_uninitialized_val_aux : forall gt v v' T C C',
  tr_typdefctx gt C C' ->
  group_tr_ok gt C ->
  wf_typdefctx C ->
  tr_val gt v v' ->
  uninitialized C T v ->
  uninitialized C' T v'.
Proof using.
  introv HC Hok Hwf Hv Hu. gen gt C' v'. induction Hu; intros;
  try solve [ inverts Hv ; constructors~ ].
  { (* val array *)
    inverts Hv as Hl Hai. constructors*.
    2: { rewrite* <- Hl. }
    applys* tr_typing_array.
    introv Hi. asserts: (index a i). 
    { rewrite index_eq_index_length in *. rewrite~ Hl. }
    applys* H2 i. }
  { (* val struct *)
    inverts Hv as; inverts HC as; 
    try solve [ intros ; simpls ; tryfalse ].
    { (* fields grouped *)
      introv Hgt HDC' HCTt0 HC'Tt0 HC'Tg0 HC'T Htrsm.
      introv HDsg Hfg HDs' Htrsgf Htrs'f Hs'fg.
      inverts Htrsm as Hgt' HDTfs' HTfs'fg0 HTfs'f HDTfs''f.
      inverts Hgt. inverts Hgt' as HD.
      constructors; unfolds typdefctx. 
      2:{ rewrite HDTfs'. rewrite HDs'. rewrite HD. 
        inverts H as HTt0 HTs. inverts HTs as.
        { introv HTfs. asserts Heq: (typ_struct Tfs = typ_struct Tfs0).
          { rewrite HTfs. rewrite <- HCTt0. auto. } 
          inverts Heq. rewrite~ <- H0. }
        { introv HTv HTs HN. asserts HN': (typ_var Tv = typ_struct Tfs0).
          { rewrite HN. rewrite <- HCTt0. auto. } 
          inverts HN'. } } 
      { constructors~.
        { inverts H as HTt0 HTs. rewrite HDC' at 1. rew_set~. }
        { rewrite HC'Tt0 at 1. constructors*. } }
      { introv Hfin. rewrite HDTfs' in Hfin. rew_set in Hfin. 
        inverts Hfin as Hfin.
        { inverts Hfin as Hfin Hfnin. admit. }
        { rewrite Hs'fg. rewrite HTfs'fg0. constructors*.
          { constructors. 
            { rewrite HDC' at 1. rew_set~. }
            { rewrite HC'Tg0 at 1. constructors*. } }
          { introv Hfin. inverts H as HTt0 HTs. inverts HTs as.
            { introv HTfs. rewrite HCTt0 in HTfs at 1. inverts HTfs.
              rewrite~ HDTfs''f. applys~ H2.
              { rewrite <- H0 in HDsg. rewrite <- HD in Hfin.
                rew_set in *. applys~ HDsg. }
              { exact Hok. }
              { repeat constructors*. rewrite~ HD.
                introv Hf0in Hf0nin. rewrite HD in Hf0nin.
                applys~ HTfs'f. } 
              { applys~ Htrsgf. rewrite~ HD. } }
            { introv HDTv HTs HTv. rewrite HCTt0 in HTv at 1. 
              inverts HTv. } } } } }
    { (* other struct *)
      introv HDC' HCTt HC'Tt HC'Tg HC'T Htrsm Hneq HDvfs Htrs'f.
      constructors~; unfolds typdefctx.
      2:{ rewrite H0. rewrite~ <- HDvfs. }
      { inverts H.
        { constructors*. }
        { simpls. applys* tr_typing_struct. 
          { constructors*. }
          { unfolds wf_typdefctx. introv HN.
            inverts Hok as Hgt HTt0in HCTt0 HTg0nin Hfs Hfg0in Hfv.
            inverts Hgt. inverts~ HN. forwards~: Hfv Tv. }
          { constructors*. } } }
      { introv Hfin. applys* H2.
        { constructors*. }
        { applys~ Htrs'f. rewrite~ <- H0. } } } }
Qed.


(* This will be proved when the relation is translated to a function. 
   See TrGroupFun.v. *)
Lemma total_tr_val_aux : forall gt v,
  exists v', tr_val gt v v'.
Proof.
Admitted.

(* Lemma for the new case. *)
Lemma tr_uninitialized_val : forall gt v T C C',
  tr_typdefctx gt C C' ->
  group_tr_ok gt C ->
  wf_typdefctx C ->
  uninitialized C T v ->
  exists v',
        tr_val gt v v'
    /\  uninitialized C' T v'.
Proof.
  introv HC Hok Hwf Hu. forwards* (v'&Hv'): total_tr_val_aux gt v.
  exists v'. splits~. applys* tr_uninitialized_val_aux.
Qed.


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
    inverts Ha as Ha.
    { inverts Hv as Hl Htr.
      forwards Htra: Htr H.
      forwards (w'&Hw'&Hπ'): IHHR Htra Ha.
      exists* w'. splits*. constructors~.
      rewrite index_eq_index_length. rewrite~ <- Hl. }
    { false*. } }
  { (* struct_access *)
    inverts Ha as; inverts Hv as;
    try solve [ intros ; false* ].
    { (* one of the fields to group *)
      introv HD1 Hgt HD2 HD3 Hsg Hfs Hsfg Hπ Hin Heq.
      inverts Heq. inverts Hgt. simpls.
      forwards Hsf: Hsg Hin.
      forwards (w'&Hw'&HR'): IHHR Hsf Hπ.
      exists w'. splits~.
      constructors. 
      { rewrite HD3. rew_set~. } 
      rewrite Hsfg. constructors~. }
    { (* absurd case *)
      introv Hneq HDs Htrsf Htrπ Hf0in Heq. simpls.
      inverts Heq. false. }
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
    inverts Hπ as Hπ; tryfalse. inverts Hv1 as Hl Htr.
    forwards Htra: Htr H.
    forwards (v2'&Hv2'&HW'): IHHW Htra Hw Hπ.
    exists (val_array T a'[i:=v2']).
    asserts: (index a' i).
    { rewrite index_eq_index_length in *. rewrite~ <- Hl. }
    splits; constructors*; try rewrite H0.
    { repeat rewrite~ length_update. }
    { introv Hi0. rew_reads; rew_index* in *.
      rewrite index_eq_index_length in *. rewrite~ <- Hl. } }
  { (* struct_access *)
    inverts Hπ as; inverts Hv1 as;
    try solve [ intros ; false ].
    { (* one of the fields to group *)
      introv HD1 Hgt HD2 HD3 Hsg Hfs Hsfg Hπ Hin Heq.
      inverts Heq. inverts Hgt. simpls.
      forwards Hsf: Hsg Hin.
      forwards (v2'&Hv2'&HW'): IHHW Hsf Hw Hπ.
      exists (val_struct (typ_var Tt) s'[fg0:=(val_struct (typ_var Tg0) sg[f0:=v2'])]).
      substs. splits.
      { applys~ tr_val_struct_group (sg[f0:=v2']);
        repeat rewrite dom_update_at_indom in *; eauto.
        { rewrite HD3. rew_set~. }
        { intros. rew_reads*. }
        { intros. rew_reads; intros; subst; eauto; contradiction. }
        { rew_reads~. } }
      { constructors~. rewrite HD3. rew_set~.
        rewrite Hsfg. constructors*. } }
    { (* absurd case *)
      introv HN HDs1 Hs1f1 Htrπ Hf0in Heq. simpls.
      inverts Heq. false. }
    { (* struct transformed but another field *)
      introv HD1 HD2 HD3 Hsg Hfs Hsfg Hπ Hor.
      inverts Hor as Hf; simpl in Hf; tryfalse.
      forwards Hsf: Hfs Hf H.
      forwards (v2'&Hv2'&HW'): IHHW Hsf Hw Hπ.
      exists (val_struct (typ_var Tt) s'[f:=v2']). splits.
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

Hint Constructors wf_trm wf_prim wf_val.

Hint Resolve wf_red.

Hint Extern 1 (wf_val ?v) =>
   match goal with H: red _ _ _ _ v |- _ => applys wf_red H end.

Hint Extern 1 (wf_state ?m2) =>
   match goal with H: red _ _ _ m2 _ |- _ => applys wf_red H end.

Theorem red_tr: forall gt C C' t t' v S S' m1 m1' m2,
  red C S m1 t m2 v ->
  group_tr_ok gt C ->
  tr_typdefctx gt C C' ->
  tr_trm gt t t' ->
  tr_stack gt S S' ->
  tr_state gt m1 m1' ->
  wf_typdefctx C ->
  wf_trm C t ->
  wf_stack C S ->
  wf_state C m1 ->
  ~ is_error v ->
  exists v' m2',
      tr_val gt v v'
  /\  tr_state gt m2 m2'
  /\  red C' S' m1' t' m2' v'.
Proof.
  introv HR Hok HC Ht HS Hm1 HwfC Hwft HwfS Hwfm1.
  introv He. gen gt C' t' S' m1'.
  induction HR; intros; try solve [ forwards*: He; unfolds* ].
  { (* val *)
    inverts Ht as Hv. exists* v' m1'. }
  { (* var *)
    inverts Ht. forwards* (v'&H'&Hv'): stack_lookup_tr HS H. exists* v' m1'. }
  { (* if *)
    inverts Ht as Hb HTrue HFalse. 
    inverts Hwft as Hwft0 Hwft1 Hwft2.
    forwards* (v'&m2'&Hv'&Hm2'&HR3): IHHR1 Hb HS Hm1.
    inverts* Hv'. destruct b;
    forwards* (vr'&m3'&Hvr'&Hm3'&HR4): IHHR2 HS Hm2';
    forwards*: wf_red HR1; exists* vr' m3'. }
  { (* let *)
    inverts Ht as Ht1 Ht2. 
    inverts Hwft as Hwft0 Hwft1.
    forwards* (v'&m2'&Hv'&Hm2'&HR3): IHHR1 Ht1 HS Hm1.
    forwards HS': tr_stack_add z HS Hv'.
    forwards: not_tr_val_error Hv'.
    forwards* (vr'&m3'&Hvr'&Hm3'&HR4): IHHR2 Ht2 HS' Hm2'.
    { applys~ wf_stack_add. applys* wf_red HR1. }
    { applys* wf_red HR1. }
    exists* vr' m3'. }
  { (* binop *)
    inverts Ht as Ht1 Ht2.
    inverts Ht1 as Ht1. inverts Ht2 as Ht2.
    inverts H3;
    try solve [ exists __ m1' ; splits~ ; inverts Ht1 ;
    inverts Ht2 ; repeat constructors~ ].
    { exists __ m1'. splits~.
      forwards: functional_tr_val Ht1 Ht2. subst.
      constructors;
      repeat applys* is_basic_tr;
      repeat applys* not_is_error_tr.
      constructors~. }
    { exists __ m1'. splits~. constructors;
      repeat applys* is_basic_tr;
      repeat applys* not_is_error_tr.
      inverts Hwft as Hwfp Hwft1 Hwft2.
      inverts Hwft1 as Hwft1. 
      inverts Hwft2 as Hwft2.
      forwards*: tr_val_inj_cp v1 v2.
      constructors~. } }
  { (* get *)
    inverts Ht as.
    { introv HN. inverts HN. }
    introv _ Hp. subst.
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
    forwards* (v'&Hv'&Hu): tr_uninitialized_val.
    exists (val_abstract_ptr l nil) m1'[l:=v']. splits~.
    { constructors.
      { unfold state. repeat rewrite~ dom_update.
        fold state. rewrite~ HD. }
      { introv Hin. unfolds state. rew_reads; intros; eauto. } }
    { constructors*. rewrite~ <- HD. applys* tr_typdefctx_wf_typ. } }
  { (* new_array *)
    inverts Ht as.
    { introv HN. inverts HN. }
    introv _ Ht.
    inverts Ht as Hv.
    inverts Hm1 as HD Htrm. subst.
    forwards* (v''&Hv''&Hu): tr_uninitialized_val.
    inverts Hv''.
    exists (val_abstract_ptr l nil) m1'[l:=(val_array (typ_array T None) a')]. splits~.
    { constructors.
      { unfold state. repeat rewrite~ dom_update.
        fold state. rewrite~ HD. }
      { introv Hin. unfolds state. rew_reads; intros; eauto. } }
    { inverts Hv. applys~ red_new_array. rewrite~ <- HD. 
      applys* tr_typdefctx_wf_typ. auto. } }
  { (* struct_access *)
    inverts Ht as; inverts Hm1 as HD Htrm.
    { (* struct op *)
      introv _ Ht Hop. subst.
      inverts Ht as Hv. inverts Hv as Ha.
      inverts Hop as.
      { (* grouped field*)
        introv Hor Hf0in Hpr.
        inverts Hor; tryfalse.
        remember (access_field (typ_var Tt) fg) as a1.
        remember (access_field (typ_var Tg) f) as a2.
        exists (val_abstract_ptr l (π'++(a1::a2::nil))) m1'.
        inverts Hpr. splits*.
        { constructors. applys* tr_accesses_app. }
        { subst. applys* red_args_1. applys* red_struct_access.
          fequals*. rew_list*. } }
      { (* other field *)
        introv Hor Hneq Hpr. 
        inverts Hor; tryfalse. inverts Hpr.
        exists (val_abstract_ptr l (π'++(access_field T f :: nil))) m1'.
        splits; constructors*. applys* tr_accesses_app. } }
    { (* not struct op *)
      introv HN. forwards*: HN. } }
  { (* array_access *)
    inverts Ht as Ht Hti. subst.
    inverts Ht as Hv. inverts Hv as Ha.
    inverts Hti as Hv. inverts Hv.
    inverts Hm1 as HD Htrm.
    exists (val_abstract_ptr l (π'++(access_array T i::nil))) m1'.
    splits; constructors*. applys* tr_accesses_app. }
  { (* struct_get *) 
    inverts Ht as.
    { (* struct op *)
      introv _ Ht Hop. inverts Hop as.
      { (* accessing grouped field *)
        introv Hor Hf0in Hpr. inverts Hor; tryfalse.
        inverts Hpr. inverts Ht as Hv. subst. inverts Hv as; 
        try solve [ intros ; contradiction ].
        introv HDsg Hgt Hfg HDs' Hsf Htrsf Hs'fg. inverts Hgt.
        exists sg[f] m1'.
        splits~.
        { applys~ red_args_1.
          { applys~ red_struct_get. rewrite HDs'. rew_set~. }
          { applys~ red_struct_get. rewrite~ Hs'fg. } } }
      { (* accessing another field *) 
        introv Hor Hneqor Hpr. inverts Hor; tryfalse. subst.
        inverts Ht as Hv. inverts Hpr. inverts Hneqor.  
        { inverts Hv as; try solve [ intros ; contradiction ].
          introv _ HDs Htrsf. exists s'[f] m1'. splits~. constructors~.
          rewrite~ <- HDs. }
        { inverts Hv as. 
          { introv HDsg Hfg HDs' Hsf Htrsf Hs'fg. 
            exists s'[f] m1'. splits~. constructors~.
            rewrite HDs'. rew_set~. }
          { introv Hneq HDs Htrsf. exists s'[f] m1'. splits~.
            constructors~. rewrite~ <- HDs. } } } }
    { (* not struct op *) 
      introv HN. forwards*: HN. } }
  { (* array_get *) 
    inverts Ht as Ht Hti. subst.
    inverts Ht as Hv.
    inverts Hti as Hvi.
    inverts Hv as Hl Hai.
    inverts Hvi. 
    exists a'[i] m1'. 
    splits~. constructors*.
    rewrite index_eq_index_length in *.
    rewrite~ <- Hl. }
  { (* TODO: Clean up these cases. *)
    (* args_1 *)
    inverts Ht; inverts Hwft;
    forwards* (v'&m2'&Hv'&Hm2'&HR'): IHHR1;
    forwards*: not_is_error_args_1 HR2 He.
    { inverts_head tr_struct_op.
      { inverts H8.
        { forwards* (v''&m3'&Hv''&Hm3'&HR''): IHHR2;
          try solve [ repeat constructors~ ; applys* wf_red HR1 ].
          applys* tr_trm_struct_op. constructors*.
          exists v'' m3'; splits*. inverts HR''.
          { applys* red_args_1. applys* red_args_1.
            applys* not_is_val_tr. }
          { forwards*: not_is_error_tr Hv''. } }
        { forwards* (v''&m3'&Hv''&Hm3'&HR''): IHHR2;
          try solve [ repeat constructors~ ; applys* wf_red HR1 ].
          applys* tr_trm_struct_op. constructors*.
          exists v'' m3'; splits*. inverts HR''.
          { applys* red_args_1. applys* red_args_1.
            applys* not_is_val_tr. }
          { forwards*: not_is_error_tr Hv''. } } }
      { inverts H8.
        { forwards* (v''&m3'&Hv''&Hm3'&HR''): IHHR2;
          try solve [ repeat constructors~ ; applys* wf_red HR1 ];
          try solve [ exists v'' m3'; splits* ;
          applys* red_args_1; applys* not_is_val_tr ].
          applys* tr_trm_struct_op. constructors*. }
        { forwards* (v''&m3'&Hv''&Hm3'&HR''): IHHR2;
          try solve [ repeat constructors~ ; applys* wf_red HR1 ];
          try solve [ exists v'' m3'; splits* ;
          applys* red_args_1; applys* not_is_val_tr ].
          applys* tr_trm_struct_op. constructors*.  } } }
    { forwards* (v''&m3'&Hv''&Hm3'&HR''): IHHR2;
      try solve [ repeat constructors~ ; applys* wf_red HR1 ];
      try solve [ exists v'' m3'; splits* ;
      applys* red_args_1; applys* not_is_val_tr ]. }
    { forwards* (v''&m3'&Hv''&Hm3'&HR''): IHHR2;
      try solve [ repeat constructors~ ; applys* wf_red HR1 ];
      try solve [ exists v'' m3'; splits* ;
      applys* red_args_1; applys* not_is_val_tr ]. } }
  { (* args_2 *)
    inverts Ht as Ht1 Ht2. inverts Hwft.
    forwards* (v'&m2'&Hv'&Hm2'&HR'): IHHR1.
    forwards*: not_is_error_args_2 HR2 He.
    forwards* (v''&m3'&Hv''&Hm3'&HR''): IHHR2.
    applys* wf_red HR1.
    repeat constructors*.
    applys* wf_red HR1.
    exists v'' m3'. splits*.
    inverts Ht1. applys* red_args_2.
    applys* not_is_val_tr. }
Qed.

End TransformationsProofs.
