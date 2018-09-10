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
      (forall f a' T,
        f \indom Tfs ->
            s'[f] = val_array (typ_array T (Some K)) a'
        /\  (forall s i,
              index a' i ->
                  a[i] = val_struct (typ_var Ts) s
              /\  tr_val st s[f] a'[i])) ->
      tr_val st (val_array (typ_var Ta) a) (val_struct (typ_var Ts) s')
  | tr_val_array_other : forall T a a',
      T <> typ_var (soa_tr_array_name st) ->
      length a = length a' ->
      (forall i,
        index a i ->
        tr_val st a[i] a'[i]) ->
      tr_val st (val_array T a) (val_array T a')
  | tr_val_struct : forall T s s',
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
    applys~ Hu. forwards* (Hs'f&Htr): H1 f a.
    { rewrite~ H0 in Hfin. } 
    rewrite Hs'f in Hus'f.
    inverts Hus'f. constructors~. }
  { introv (i&Hi&Hua'i).
    asserts Hi': (index a i).
    { rewrite index_eq_index_length in *. rewrite~ H0. }
    applys* H2. introv HN. applys~ Hu.
    constructors. exists i. splits~. }
  { introv (f&Hfin&Hus'f).
    asserts Hf': (f \indom s).
    { rewrite~ H. }
    applys* H1. introv HN. applys~ Hu. constructors.
    exists f. splits~. }
Unshelve. typeclass.
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
Admitted.

Lemma tr_accesses_inj : forall C st π π1 π2,
  soa_tr_ok st C ->
  wf_accesses C π1 ->
  wf_accesses C π2 ->
  tr_accesses st π1 π ->
  tr_accesses st π2 π ->
    π1 = π2.
Proof.
Admitted.

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

