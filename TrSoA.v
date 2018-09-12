(**

This file describes transformations of the layout of records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export Semantics LibSet LibMap LibList TLCbuffer Typing.

Open Scope Z_scope.

(* ********************************************************************** *)
(* * Definition of the transformation *)

(** SoA transformation. Specified by:
    - The typvar of the array of structs.
    - The typvar of said struct.
    - The size of the array. *)

Record soa_tr := make_soa_tr {
  soa_tr_array_name : typvar;
  soa_tr_struct_name : typvar;
  soa_tr_struct_fields : map field typ;
  soa_tr_array_size : size
}.

Notation make_soa_tr' := make_soa_tr.

(** Checking if the transformation is acceptable *)

Inductive soa_tr_ok : soa_tr -> typdefctx -> Prop :=
  | soa_tr_ok_intros : forall Ta Ts Tfs K st C,
      st = make_soa_tr Ta Ts Tfs K ->
      Ta \indom C ->
      Ts \indom C ->
      C[Ta] = typ_array (typ_var Ts) (Some K) ->
      C[Ts] = typ_struct Tfs ->
      K > 0%Z ->
      (forall Tv,
        Tv \indom C ->
        Tv <> Ta ->
        ~ free_typvar C Ta C[Tv]) ->
      (forall Tv,
        Tv \indom C ->
        Tv <> Ts ->
        ~ free_typvar C Ts C[Tv]) ->
      soa_tr_ok st C.


(* ********************************************************************** *)
(* * The transformation applied to the different constructs. *)

(** Transformation of typdefctxs: C ~ |C| *)

Inductive tr_typdefctx (st:soa_tr) : typdefctx -> typdefctx -> Prop :=
  | tr_typdefctx_intro : forall Ta Ts Tfs K Tfs' C C',
      st = make_soa_tr Ta Ts Tfs K ->
      dom C' \u \{Ta} = dom C ->
      Ta \notindom C' ->
      C[Ta] = typ_array (typ_var Ts) (Some K) ->
      C[Ts] = typ_struct Tfs ->
      C'[Ts] = typ_struct Tfs' ->
      dom Tfs = dom Tfs' ->
      (forall f,
        f \indom Tfs ->
        Tfs'[f] = typ_array Tfs[f] (Some K)) ->
      (forall Tv,
        Tv \indom C ->
        Tv <> Ta ->
        C'[Tv] = C[Tv]) ->
      tr_typdefctx st C C'.

(** Transformation of paths: π ~ |π| *)

Inductive tr_accesses (st:soa_tr) : accesses -> accesses -> Prop :=
  | tr_accesses_nil :
      tr_accesses st nil nil
  | tr_accesses_soa : forall Ta Ts Tfs K f i a0 a1 π a0' a1' π',
      tr_accesses st π π' ->
      st = make_soa_tr Ta Ts Tfs K ->
      a0 = access_array (typ_var Ta) i ->
      a1 = access_field (typ_var Ts) f ->
      a0' = access_field (typ_var Ts) f ->
      a1' = access_array (typ_array Tfs[f] (Some K)) i ->
      tr_accesses st (a0::a1::π) (a0'::a1'::π')
  | tr_accesses_array_other : forall π π' T i,
      T <> typ_var (soa_tr_array_name st) ->
      tr_accesses st π π' ->
      tr_accesses st ((access_array T i)::π) ((access_array T i)::π')
  | tr_accesses_field : forall T π π' f,
      T <> typ_var (soa_tr_struct_name st) ->
      tr_accesses st π π' ->
      tr_accesses st ((access_field T f)::π) ((access_field T f)::π').

(** Transformation of values: v ~ |v| *)

Inductive tr_val (st:soa_tr) : val -> val -> Prop :=
  | tr_val_uninitialized :
      tr_val st val_uninitialized val_uninitialized
  | tr_val_unit :
      tr_val st val_unit val_unit
  | tr_val_bool : forall b,
      tr_val st (val_bool b) (val_bool b)
  | tr_val_int : forall i,
      tr_val st (val_int i) (val_int i)
  | tr_val_double : forall d,
      tr_val st (val_double d) (val_double d)
  | tr_val_abstract_ptr : forall l π π',
      tr_accesses st π π' ->
      tr_val st (val_abstract_ptr l π) (val_abstract_ptr l π')
  | tr_val_array_tiling : forall Tfs K Ta a Ts s',
      st = make_soa_tr Ta Ts Tfs K ->
      dom s' = dom Tfs ->
      (forall f a',
        f \indom Tfs ->
        s'[f] = val_array (typ_array Tfs[f] (Some K)) a' ->
        (forall s i,
          index a i ->
          a[i] = val_struct (typ_var Ts) s ->
          tr_val st s[f] a'[i])) ->
      (forall f,
        f \indom Tfs ->
            (exists a', 
                s'[f] = val_array (typ_array Tfs[f] (Some K)) a'
            /\  length a = length a')) ->
      (forall i,
        index a i ->
            exists s,
                a[i] = val_struct (typ_var Ts) s
            /\  dom s = dom Tfs) ->
      tr_val st (val_array (typ_var Ta) a) (val_struct (typ_var Ts) s')
  | tr_val_array_other : forall T a a',
      T <> typ_var (soa_tr_array_name st) ->
      length a = length a' ->
      (forall i,
        index a i ->
        tr_val st a[i] a'[i]) ->
      tr_val st (val_array T a) (val_array T a')
  | tr_val_struct_other : forall T s s',
      T <> typ_var (soa_tr_struct_name st) ->
      dom s = dom s' ->
      (forall f,
        f \indom s ->
        tr_val st s[f] s'[f]) ->
      tr_val st (val_struct T s) (val_struct T s').

(* Transformation used in the struct cases to avoid repetition. *)

Inductive tr_struct_op (st:soa_tr) : trm -> trm -> Prop :=
  | tr_struct_access_soa : forall Ta Ts Tfs K T f t ti ts ta ts' ta',
      st = make_soa_tr Ta Ts Tfs K ->
      (* Initial term is ts: &(t[i]->f) *)
      ta = trm_app (prim_struct_access (typ_var Ts) f) (ts::nil) ->
      ts = trm_app (prim_array_access T) (t::ti::nil) ->
      (* Final term is ta': &(t->f[i]) *)
      ts' = trm_app (prim_array_access (typ_var Ta)) (ta'::ti::nil) ->
      ta' = trm_app (prim_struct_access T f) (t::nil) ->
      (* Result. *)
      tr_struct_op st ta ts'
  | tr_struct_get_soa : forall Ta Ts Tfs K T f t ti ts ta ts' ta',
      st = make_soa_tr Ta Ts Tfs K ->
      (* Initial term is ts: t[i].f *)
      ta = trm_app (prim_struct_get (typ_var Ts) f) (ts::nil) ->
      ts = trm_app (prim_array_get T) (t::ti::nil) ->
      (* Final term is ta': t.f[i] *)
      ts' = trm_app (prim_array_get (typ_var Ta)) (ta'::ti::nil) ->
      ta' = trm_app (prim_struct_get T f) (t::nil) ->
      (* Result. *)
      tr_struct_op st ta ts'
  | tr_struct_access_other : forall Ts T f ts,
      Ts = soa_tr_struct_name st ->
      T <> (typ_var Ts) ->
      tr_struct_op st (trm_app (prim_struct_access T f) ts) (trm_app (prim_struct_access T f) ts)
  | tr_struct_get_other : forall Ts T f ts,
      Ts = soa_tr_struct_name st ->
      T <> (typ_var Ts) ->
      tr_struct_op st (trm_app (prim_struct_get T f) ts) (trm_app (prim_struct_get T f) ts).

(** Transformation of terms: t ~ |t| *)

Inductive tr_trm (st:soa_tr) : trm -> trm -> Prop :=
  | tr_trm_val : forall v v',
      tr_val st v v' ->
      tr_trm st (trm_val v) (trm_val v')
  | tr_trm_var : forall x,
      tr_trm st (trm_var x) (trm_var x)
  | tr_trm_if : forall t1 t2 t3 t1' t2' t3',
      tr_trm st t1 t1' ->
      tr_trm st t2 t2' ->
      tr_trm st t3 t3' ->
      tr_trm st (trm_if t1 t2 t3) (trm_if t1' t2' t3')
  | tr_trm_let : forall x t1 t2 t1' t2',
      tr_trm st t1 t1' ->
      tr_trm st t2 t2' ->
      tr_trm st (trm_let x t1 t2) (trm_let x t1' t2')
  (* new *)
  | tr_trm_new : forall T,
      tr_trm st (trm_app (prim_new T) nil) (trm_app (prim_new T) nil)
  (* Special case: struct + array access *)
  | tr_trm_array : forall t1' op t1 tr,
      is_struct_op op ->
      tr_trm st t1 t1' ->
      tr_struct_op st (trm_app op (t1'::nil)) tr ->
      tr_trm st (trm_app op (t1::nil)) tr
  (* Args *)
  | tr_trm_args1 : forall op t1 t1',
      tr_trm st t1 t1' ->
      tr_trm st (trm_app op (t1::nil)) (trm_app op (t1'::nil))
  | tr_trm_args2 : forall op t1 t1' t2 t2',
      ~ is_array_op op ->
      tr_trm st t1 t1' ->
      tr_trm st t2 t2' ->
      tr_trm st (trm_app op (t1::t2::nil)) (trm_app op (t1'::t2'::nil)).

(** Transformation of stacks: S ~ |S| *)

Inductive tr_stack_item (st:soa_tr) : (var * val) -> (var * val) -> Prop :=
  | tr_stack_item_intro : forall x v v',
      tr_val st v v' -> 
      tr_stack_item st (x, v) (x, v').

Inductive tr_stack (st:soa_tr) : stack -> stack -> Prop :=
  | tr_stack_intro : forall S S',
      LibList.Forall2 (tr_stack_item st) S S' ->
      tr_stack st S S'.

(** Transformation of states: m ~ |m| *)

Inductive tr_state (st:soa_tr) : state -> state -> Prop :=
  | tr_state_intro : forall m m',
      dom m = dom m' ->
      (forall l,
        l \indom m ->
        tr_val st m[l] m'[l]) ->
      tr_state st m m'.


(* ********************************************************************** *)
(* * Preservation of semantics proof. *)

Lemma stack_lookup_tr : forall st S S' x v,
  tr_stack st S S' ->
  Ctx.lookup x S = Some v -> 
    exists v', 
       Ctx.lookup x S' = Some v' 
    /\ tr_val st v v'.
Proof.
  introv HS Hx. inverts HS as HS. induction HS.
  { inverts Hx. }
  { inverts H as Hv. inverts Hx as Hx. case_if in Hx.
    { inverts Hx. exists v'. splits*. unfolds. case_if*. }
    { forwards (v''&Hx'&Hv''): IHHS Hx. exists v''.
      splits*. unfolds. case_if. fold Ctx.lookup. auto. } }
Qed.

Lemma tr_stack_add : forall st z v S v' S',
  tr_stack st S S' ->
  tr_val st v v' ->
  tr_stack st (Ctx.add z v S) (Ctx.add z v' S').
Proof.
  introv HS Hv. constructors~. inverts HS.
  unfolds Ctx.add. destruct* z.
  applys~ Forall2_cons. constructors~.
Qed.

Lemma is_basic_tr : forall st v1 v2,
  tr_val st v1 v2 ->
  is_basic v1 ->
  is_basic v2.
Proof.
  introv Htr Hv1. induction Htr;
  try solve [ inverts Hv1 ];
  constructors~.
Qed.

Lemma not_tr_val_error : forall st v1 v2,
  tr_val st v1 v2 ->
  ~ is_error v2.
Proof.
  introv Hv He. unfolds is_error.
  destruct* v2. inverts Hv.
Qed.

Lemma not_is_val_tr : forall st t1 t2,
  tr_trm st t1 t2 ->
  ~ is_val t1 ->
  ~ is_val t2.
Proof.
  introv Htr Hv. induction Htr; introv HN;
  try solve [ subst ; inverts HN ].
  forwards*: Hv. inverts H0 as; inverts HN.
Qed.

Lemma not_is_error_tr : forall gt v1 v2,
  tr_val gt v1 v2 ->
  ~ is_error v2.
Proof.
  introv Htr. induction Htr; introv HN;
  try solve [ subst ; inverts HN ].
Qed.

Lemma not_is_uninitialized_tr : forall st v v',
  tr_val st v v' -> 
  ~ is_uninitialized v ->
  ~ is_uninitialized v'.
Proof.
introv Htr Hu HN. induction Htr; subst; inverts HN as.
  { applys* Hu. constructors. }
  { introv (f&Hfin&Hus'f).
    applys~ Hu. 
    asserts Hfin': (f \indom Tfs).
    { rewrite~ <- H0. }
    forwards* (a'&Hs'f&Hl): H3 Hfin'.
    rewrite Hs'f in Hus'f.
    inverts Hus'f as (i&Hi&Hua'i).
    asserts Hi': (index a i).
    { rewrite index_eq_index_length in *. rewrite~ Hl. }
    forwards* (s&Hai&HDs): H4 i.
    asserts Hfin'': (f \indom s).
    { rewrite~ HDs. }
    constructors~. exists i. splits~.
    rewrite Hai. constructors.
    exists f. splits~.
    forwards~ Htr: H1 f a' s i.
    tests: (is_uninitialized s[f]); auto.
    false. applys* H2. }
  { introv (i&Hi&Hua'i).
    asserts Hi': (index a i).
    { rewrite index_eq_index_length in *. rewrite~ H0. }
    applys* H2. introv HN. applys~ Hu.
    constructors. exists i. splits~. }
  { introv (f&Hfin&Hus'f).
    asserts Hf': (f \indom s).
    { rewrite~ H0. }
    applys* H2. introv HN. applys~ Hu. constructors.
    exists f. splits~. }
Qed.

(* Functional results. *)

Theorem functional_tr_accesses : forall st π π1 π2,
  tr_accesses st π π1 ->
  tr_accesses st π π2 ->
    π1 = π2.
Proof.
  introv H1 H2. gen π2. induction H1; intros.
  { inverts~ H2. }
  { inverts_head tr_accesses; tryfalse.
    { inverts_head access_array.
      inverts_head access_field.
      inverts_head make_soa_tr'.
      subst. repeat fequals.
      applys~ IHtr_accesses. }
    { subst. simpls. inverts_head access_array. false. } }
  { inverts H2; try solve [ simpls; false ].
    fequals. applys~ IHtr_accesses. }
  { inverts H2; try solve [ simpls; false ].
    fequals. applys~ IHtr_accesses. }
Qed.

Theorem functional_tr_val : forall st v v1 v2,
  tr_val st v v1 ->
  tr_val st v v2 ->
  v1 = v2.
Proof using.
  introv Hv1 Hv2. gen v2. induction Hv1; intros;
  try solve [ inverts~ Hv2 ].
  { inverts Hv2 as Hπ. fequals.
    forwards~: functional_tr_accesses H Hπ. }
  { inverts Hv2 as.
    { introv HD Htr1 Htr2 Htr3. inverts_head make_soa_tr'.
      fequals. applys~ read_extens.
      { congruence. }
      { intros f Hf. asserts Hf': (f \indom Tfs0).
        { rewrite~ H0 in Hf. }
        forwards~ (a'&Hs'0f&Hl): Htr2 f.
        forwards~ (a''&Hs'f&Hl'): H3 f.
        rewrite Hs'0f. rewrite Hs'f.
        fequals. applys~ eq_of_extens.
        { congruence. }
        { introv Hi. asserts Hi': (index a i).
          { rewrite index_eq_index_length in *. rewrite~ Hl'. }
          asserts Hi'': (index a' i).
          { rewrite index_eq_index_length in *. rewrite~ <- Hl. }
          forwards~ (s&Hai&HDs): Htr3 i.
          forwards~ Htr: H1 f a'' s i.
          forwards~ Htr': Htr1 f a' s i.
          forwards~: H2 f a'' s i (a'[i]). } } }
    { introv HN. subst. simpls. false. } }
  { inverts Hv2 as.
    { simpls. false. }
    { introv HT Hl Htr. fequals.
      applys~ eq_of_extens.
      { congruence. }
      { introv Hi. asserts  Hi': (index a i).
        { rewrite index_eq_index_length in *.
          rewrite~ <- H0 in Hi. }
        applys~ H2. } } }
  { inverts Hv2 as HDs Htr. fequals.
    applys~ read_extens.
    { congruence. }
    { intros f Hf. asserts Hf': (f \indom s).
      { rewrite~ <- H0 in Hf. }
      applys~ H2. } }
Qed.

Lemma tr_accesses_inj : forall C st π π1 π2,
  soa_tr_ok st C ->
  wf_accesses C π1 ->
  wf_accesses C π2 ->
  tr_accesses st π1 π ->
  tr_accesses st π2 π ->
    π1 = π2.
Proof.
  introv Hok Hwfπ1 Hwfπ2 Hπ1 Hπ2. gen C π2. induction Hπ1; intros.
  { inverts~ Hπ2. }
  { inverts Hπ2 as.
    { introv Hπ0.
      inverts TEMP.
      inverts_head access_array.
      inverts_head access_field. subst.
      repeat fequals.
      inverts Hwfπ1 as _ Hwfπ1. inverts Hwfπ1 as _ Hwfπ1.
      inverts Hwfπ2 as _ Hwfπ2. inverts Hwfπ2 as _ Hwfπ2.
      applys* IHHπ1. }
    { inverts_head access_array. }
    { introv HN. subst. simpls.
      inverts TEMP. false. } }
  { inverts Hπ2 as.
    { introv _ HN. inverts HN. }
    { introv _ Htr. fequals.
      inverts Hwfπ1 as _ Hwfπ1.
      inverts Hwfπ2 as _ Hwfπ2.
      applys* IHHπ1. } }
  { inverts Hπ2 as.
    { introv _ HN. inverts HN. simpls. false. }
    { introv Hneq Htr. fequals.
      inverts Hwfπ1 as _ Hwfπ1.
      inverts Hwfπ2 as _ Hwfπ2.
      applys* IHHπ1. } }
Qed.

Lemma tr_val_inj : forall C st v v1 v2,
  soa_tr_ok st C ->
  is_basic v1 ->
  is_basic v2 ->
  wf_val C v1 ->
  wf_val C v2 ->
  tr_val st v1 v ->
  tr_val st v2 v ->
  v1 = v2.
Proof.
  introv Hok HBv1 HBv2 Hwfv1 Hwfv2 Hv1 Hv2. gen C v2. induction Hv1; intros;
  try solve [ inverts Hv2; repeat fequals*; subst; simpls; tryfalse* ].
  { inverts Hv2 as Hπ. repeat fequals*.
    inverts Hwfv1 as HRφ1. inverts Hwfv2 as HRφ2.
    applys* tr_accesses_inj. }
Qed.

Lemma tr_val_inj_cp : forall C st v1 v2 v1' v2',
  soa_tr_ok st C ->
  is_basic v1 ->
  is_basic v2 ->
  wf_val C v1 ->
  wf_val C v2 ->
  tr_val st v1 v1' ->
  tr_val st v2 v2' ->
  v1 <> v2 ->
  v1' <> v2'.
Proof.
  introv Hok HBv1 HBv2 HTv1 HTv2 Hv1 Hv2 Hneq HN. subst.
  forwards*: tr_val_inj Hok HTv1 HTv2 Hv1.
Qed.


(* ********************************************************************** *)
(* * Correctness proofs *)

Section TransformationProofs.

Lemma tr_read_accesses : forall st v π v' π' w,
  tr_val st v v' ->
  tr_accesses st π π' ->
  read_accesses v π w ->
  (exists w',
      tr_val st w w'
  /\  read_accesses v' π' w').
Proof.
  introv Hv Ha HR. gen v v' w. induction Ha; intros.
  { (* nil *)
    inverts HR. exists v'. splits~. constructors~. }
  { (* access soa *) 
    subst. inverts HR as Hi HR.
    inverts HR as Hf HR.
    inverts Hv as; try solve [ intros; simpls; false ].
    introv Heq HDs' Htr HEa' HEs.
    inverts Heq.
    forwards~ (s0&Hai&HDs0): HEs i.
    rewrite <- H in Hai. inverts Hai.
    forwards~ (a'&Hs'f&Hl): HEa' f.
    { rewrite~ <- HDs0. }
    forwards* (w'&Hw'&HRw'): IHHa (s0[f]) (a'[i]).
    { forwards* Htr': Htr f a'.
      { rewrite~ <- HDs0. } }
    exists w'. splits~. constructors.
    { rewrite HDs'. rewrite~ <- HDs0. }
    rewrite Hs'f. constructors.
    { rewrite index_eq_index_length in *.
      rewrite~ <- Hl. }
    auto. }
  { (* access other array *)
    inverts HR. inverts Hv as; try solve [ simpls; false ].
    introv Hneq Hl Htr.
    forwards* Htr': Htr.
    forwards* (w'&Hw'&HRw'): IHHa.
    exists w'. splits*. constructors~.
    { rewrite index_eq_index_length in *. rewrite~ <- Hl. } }
  { (* access struct *)
    inverts HR. inverts Hv as Hneq HD Htr.
    forwards* Htr': Htr.
    forwards* (w'&Hw'&HRw'): IHHa.
    exists w'. splits*. constructors~.
    rewrite~ <- HD. }
Qed.

Lemma tr_write_accesses : forall st v1 w π v1' π' w' v2,
  tr_val st v1 v1' ->
  tr_val st w w' ->
  tr_accesses st π π' ->
  write_accesses v1 π w v2 ->
  (exists v2',
        tr_val st v2 v2'
    /\  write_accesses v1' π' w' v2').
Proof.
  introv Hv1 Hw Hπ HW. gen v1 v1' w w' v2. induction Hπ; intros.
  { (* nil *) 
    inverts HW. exists w'. splits*. constructors~. }
  { (* access soa *)
    subst.
    inverts HW as Hi HW.
    inverts HW as Hf HW.
    inverts Hv1 as; try solve [ intros; simpls; false ].
    introv Heq HDs' Htr HEa' HEs.
    inverts Heq.
    forwards~ (s0&Hai&HDs0): HEs i.
    rewrite <- H in Hai. inverts Hai.
    forwards~ (a'&Hs'f&Hl): HEa' f.
    { rewrite~ <- HDs0. }
    forwards* Htr': Htr f a'.
    { rewrite~ <- HDs0. }
    forwards~ (v2'&Htrv2'&HWv2'): IHHπ (s0[f]) (a'[i]) w w' v0.
    exists (val_struct (typ_var Ts0) s'[f:=(val_array (typ_array Tfs0[f] (Some K0)) a'[i:=v2'])]).
    splits.
    { constructors*.
      { rewrite <- HDs'. applys~ dom_update_at_indom.
        rewrite HDs'. rewrite~ <- HDs0. }
      { introv Hf0in Hs'fu Hi0 Ha1iu.
        tests: (f=f0).
        { rew_reads in Hs'fu. inverts Hs'fu.
          tests: (i=i0).
          { rew_reads~ in Ha1iu. intros. inverts Ha1iu. 
            asserts Hi0': (index a' i0).
            { rewrite index_eq_index_length in *. rewrite~ <- Hl. }
            rew_reads~. }
          { asserts Hi0': (index a' i0).
            { rewrite index_update_eq in Hi0. rewrite index_eq_index_length in *.
              rewrite~ <- Hl. }
            asserts Hi0'': (index a1 i0).
            { rewrite index_eq_index_length in *. rewrite~ Hl. }
            rew_reads~. intros. rew_reads~ in Ha1iu. } }
        { rew_reads in Hs'fu. intros.
          tests: (i=i0).
          { rew_reads~ in Ha1iu. intros. inverts Ha1iu.
            rew_reads~. }
          { rewrite index_update_eq in Hi0. 
            rew_reads~ in Ha1iu. } } }
        { introv Hf0in. rew_reads~.
          { introv Heq. subst. exists~ a'[i:=v2']. splits~.
            repeat rewrites~ length_update. }
          { introv Hneq. forwards~ (a'0&Hs'f0&Hl'): HEa' f0.
            exists a'0. splits~. repeat rewrite~ length_update. } }
        { introv Hi0in.
          asserts Hi0': (index a1 i0).
          { rewrite index_eq_index_length in *. 
            rewrite~ length_update in Hi0in. }
          rew_reads~. intros. exists~ s0[f:=v0].
          splits~. rewrite~ dom_update_at_indom. } }
      { constructors~.
        { rewrite HDs'. rewrite~ <- HDs0. }
        rewrite Hs'f. constructors*. 
        { rewrite index_eq_index_length in *. rewrite~ <- Hl. } } }
  { (* access other array *)
    inverts HW as Hi HW.
    inverts Hv1 as; try solve [ intros; subst; simpls; false ].
    introv _ Hl Htr. forwards~ Ha1i: Htr Hi.
    forwards~ (v2'&Hv2'&HWv2'): IHHπ a1[i] a'[i] w w' v.
    exists (val_array T a'[i:=v2']).
    splits.
    { constructors~.
      { repeat rewrite~ length_update. }
      { introv Hi0.
        asserts: (index a1 i0).
        { rewrite index_eq_index_length in *.
          rewrite~ length_update in Hi0. }
        asserts: (index a' i0).
        { rewrite index_eq_index_length in *.
          rewrite length_update in Hi0.
          rewrite~ Hl in Hi0. }
        rew_reads~. } }
    { asserts: (index a' i).
      { rewrite index_eq_index_length in *. rewrite~ <- Hl. }
      constructors*. } }
  { (* access struct *)
    inverts HW as Hf HW.
    inverts Hv1 as; try solve [ intros; subst; simpls; false ].
    introv _ HDs1 Htr. forwards~ Htrs1f: Htr Hf.
    forwards~ (v2'&Hv2'&HWv2'): IHHπ s1[f] s'[f] w w' v.
    exists (val_struct T s'[f:=v2']).
    splits.
    { constructors~.
      { repeat rewrite~ dom_update_at_indom.
        rewrite~ <- HDs1. }
      { introv Hf0. rew_reads~. introv Hneq.
        applys~ Htr. rewrite~ dom_update_at_indom in Hf0. } }
    { asserts: (f \indom s').
      { rewrite~ <- HDs1. }
      constructors*. } }
Qed.

(* ---------------------------------------------------------------------- *)
(** The transformation preserves well-founded types. *)

Lemma tr_typdefctx_wf_typ : forall tt C C' T,
  tr_typdefctx tt C C' ->
  wf_typ C T ->
  wf_typ C' T.
Proof.
  introv HC HT. induction HT; try solve [ constructors* ].
  inverts HC as HDC' HCTa HC'Ta HC'Tt HC'Tv Hos.
  constructors.
  { rewrite HDC'. rew_set~. }
  { tests: (Tv=Ta).
    { rewrite HC'Ta. repeat constructors~.
      { rewrite HDC'. rew_set~. }
      { rewrite HC'Tt. constructors~.
        rewrite HCTa in IHHT.
        inverts~ IHHT. } }
    { rewrite~ HC'Tv. } }
Qed.


(* ---------------------------------------------------------------------- *)
(** uninitialized is coherent with the transformation *)

Lemma tr_typing_struct : forall tt C C' Ts Tfs,
  tr_typdefctx tt C C' ->
  typing_struct C Ts Tfs ->
  typing_struct C' Ts Tfs.
Proof.
  introv HC HTs. induction HTs; intros.
  { constructors~. }
  { inverts HC as HD HCTa HC'Ta HC'Tt HC'Tv _.
    constructors~.
    { rewrite HD. rew_set~. }
    { tests: (Tv=Ta).
      { rewrite HCTa in HTs. inverts HTs. }
      { rewrite~ HC'Tv. } } }
Qed.

Lemma tr_typing_array : forall Tat Tt k C C' Ta T os,
  tr_typdefctx (make_tiling_tr Tat Tt k) C C' ->
  wf_typdefctx C ->
  ~ free_typvar C Tat Ta ->
  typing_array C Ta T os ->
  typing_array C' Ta T os.
Proof.
  introv HC Hwf Hfv HTa. gen Tt Tat k C'. induction HTa; intros.
  { constructors~. applys* tr_typdefctx_wf_typ. }
  { inverts HC as Htt HD HCTa HC'Ta HC'Tt HC'Tv Hos.
    inverts Htt. constructors.
    { rewrite HD. rew_set~. }
    { tests: (Tv=Ta).
      { false. applys~ Hfv. constructors~. }
      { rewrite~ HC'Tv. applys* IHHTa Tt0 Ta K.
        { introv HN. applys~ Hfv. constructors~. }
        { constructors*. } } } }
Qed.


Lemma tr_uninitialized_val_aux : forall tt v v' T C C',
  tr_typdefctx tt C C' ->
  tiling_tr_ok tt C ->
  wf_typdefctx C ->
  tr_val tt v v' ->
  uninitialized C T v ->
  uninitialized C' T v'.
Proof using.
  introv HC Hok Hwf Hv Hu. gen tt C' v'. induction Hu; intros;
  try solve [ inverts Hv ; constructors~ ].
  { (* array *)
    inverts HC as HD HCTa HC'Ta HC'Tt HC'Tv Hos.
    inverts Hv as.
    { (* tiling array *)
      introv Htt Hnb Ha'i Htra. inverts Htt.
      inverts Hok as Htt HTain HCTa' HTt0nin Hnz Hfv.
      inverts Htt. unfolds wf_typdefctx.
      rewrite HCTa in HCTa'. inverts HCTa'.
      inverts H as _ HTCTa.
      rewrite HCTa in HTCTa. inverts HTCTa.
      destruct* os; destruct* os'.
      { (* Fixed-size array. *)
        applys uninitialized_array (Some (length aJ)).
        3:{ introv Hi. forwards* (a''&Ha'i'&Hla''): Ha'i.
            rewrite Ha'i'. applys uninitialized_array (Some K).
            { constructors.
              { rewrite HD. rew_set~. }
              { rewrite HC'Tt. constructors~.
                applys* tr_typdefctx_wf_typ. constructors*. } }
            { introv Heq. inverts~ Heq. }
            { introv Hi0. forwards* Htra'': Htra (i*K + i0)%Z i i0 a''.
              applys* H2.
              { applys* tiled_index_range_i. }
              { constructors*. }
              { constructors*. } } }
        { constructors.
          { rewrite HD. rew_set~. }
          { rewrite HC'Ta. unfolds nb_tiles.
            rewrite Hos. rewrite Hnb.
            forwards* Heq: H0 s. rewrite Heq.
            constructors. constructors.
            { rewrite HD. rew_set~. }
            { rewrite HC'Tt. constructors.
              applys* tr_typdefctx_wf_typ. constructors*. } } }
        { introv Heq. inverts~ Heq. } }
      { (* Variable length array. *)
        applys uninitialized_array.
        { constructors.
          { rewrite HD. rew_set~. }
          { rewrite HC'Ta. repeat constructors~.
            { rewrite HD. rew_set~. }
            { rewrite HC'Tt. constructors~.
              applys* tr_typdefctx_wf_typ. constructors*. } } }
        { introv HN. inverts HN. }
        { introv Hi. forwards* (a''&Ha'i'&Hla''): Ha'i.
          rewrite Ha'i'. constructors.
          { constructors.
            { rewrite HD. rew_set~. }
            { rewrite HC'Tt. constructors~.
              applys* tr_typdefctx_wf_typ. constructors*. } }
          { introv Hn. inverts~ Hn. }
          { introv Hi0. forwards* Htra': Htra (i*K + i0)%Z i i0 a''.
            applys* H2.
            { applys* tiled_index_range_i. }
            { constructors*. }
            { constructors*. } } } } }
    { (* other array *)
      introv Hneq Hla Htra. simpls. constructors.
      2:{ rewrite <- Hla. eapply H0. }
      { inverts H as.
        { introv HwfT. constructors*. 
          applys* tr_typdefctx_wf_typ. constructors*. }
        { introv HTvin HTCTv.
          inverts Hok as Htt HTain HCTa' HTt0nin Hnz Hfv.
          inverts Htt. unfolds wf_typdefctx. constructors*.
          { rewrite HD. rew_set~. }
          { rewrite~ HC'Tv. applys~ tr_typing_array Ta Tt0 K0 C.
            { rewrite HCTa in HCTa'. inverts HCTa'. constructors*. }
            { applys~ Hfv. introv HN. subst. applys~ Hneq. }
            { introv HN. subst. applys~ Hneq. } } } }
      { introv Hi.
        asserts: (index a i).
        { rewrite index_eq_index_length in *. rewrite~ Hla. }
        forwards* Htra': Htra i.
        applys* H2. constructors*. } } }
  { (* struct *)
    inverts Hv as HD Hvfsf. constructors.
    2:{ rewrite~ H0. }
    { applys* tr_typing_struct. }
    { introv Hfin. applys* H2. applys Hvfsf.
      rewrite~ <- H0. } }
Qed.

(* This will be proved when the relation is translated to a function. 
   See TrTilingFun.v. *)
Lemma total_tr_val_aux : forall gt v,
  exists v', tr_val gt v v'.
Proof.
Admitted.

(* Lemma for the new case. *)
Lemma tr_uninitialized_val : forall tt v T C C',
  tr_typdefctx tt C C' ->
  tiling_tr_ok tt C ->
  wf_typdefctx C ->
  uninitialized C T v ->
  exists v',
        tr_val tt v v'
    /\  uninitialized C' T v'.
Proof.
  introv HC Hok Hwf Hu. forwards* (v'&Hv'): total_tr_val_aux tt v.
  exists v'. splits~. applys* tr_uninitialized_val_aux.
Qed.


(* ---------------------------------------------------------------------- *)
(** Path surgery *)

Lemma tr_accesses_app : forall tt π1 π2 π1' π2',
  tr_accesses tt π1 π1' ->
  tr_accesses tt π2 π2' ->
  tr_accesses tt (π1 ++ π2) (π1' ++ π2').
Proof.
  introv Ha1 Ha2. gen π2 π2'. induction Ha1; intros;
  rew_list in *; eauto.
Qed.

