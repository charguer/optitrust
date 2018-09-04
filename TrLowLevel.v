(**

This file describes transformations of the layout of records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export Semantics TypeSoundness.

(* ********************************************************************** *)
(* * Definition of the transformation *)

(** This is a special kind of transformation. We need to define new 
    new semantics. Essentially get and set for the concrete pointer.
    It can be included in the general semantics and just check that no
    concrete pointers are used in the other transformations. *)


(* ---------------------------------------------------------------------- *)

Definition in_block (m:state) (l1:loc) (l:loc) : Prop :=
  exists ws,
      m[l1] = val_words ws
  /\  l1 <= l < l1 + length ws.

Definition disjoint_blocks (m:state) : Prop :=
  forall l1 l2,
    l1 <> l2 ->
    forall l, ~ (in_block m l1 l /\ in_block m l1 l).

(** Transformation of states: m ~ |m| *)

Inductive tr_state (C:typdefctx) (LLC:ll_typdefctx) (α:alpha) (φ:phi) : state -> state -> Prop :=
  | tr_state_intro : forall m m',
      dom m = dom m' ->
      disjoint_blocks m' ->
      (forall l,
        l \indom m ->
          exists lw T,
              typing_val C LLC φ m[l] T
          /\  tr_ll_val C LLC α T m[l] lw
          /\  m'[α[l]] = val_words lw) ->
      tr_state C LLC α φ m m'.

(* ---------------------------------------------------------------------- *)
(* Transformation of a term from high-level to low-level. This is how the code is transformed. *)

Inductive tr_val (C:typdefctx) (LLC:ll_typdefctx) (α:alpha) : val -> val -> Prop :=
  | tr_val_error :
      tr_val C LLC α val_error val_error
  | tr_val_unit :
      tr_val C LLC α val_unit val_unit
  | tr_val_bool : forall b,
      tr_val C LLC α (val_bool b) (val_bool b)
  | tr_val_int : forall i,
      tr_val C LLC α (val_int i) (val_int i)
  | tr_val_double : forall d,
      tr_val C LLC α (val_double d) (val_double d)
  | tr_val_abstract_ptr : forall π l o,
      tr_ll_accesses C LLC π o ->
      tr_val C LLC α (val_abstract_ptr l π) (val_concrete_ptr α[l] o)
  | tr_val_array : forall T a a',
      List.Forall2 (tr_val C LLC α) a a' ->
      tr_val C LLC α (val_array T a) (val_array T a')
  | tr_val_struct : forall Tv s s',
      dom s = dom s' ->
      (forall f,
        index s f ->
        tr_val C LLC α s[f] s'[f]) ->
      tr_val C LLC α (val_struct (typ_var Tv) s) (val_struct (typ_var Tv) s').

(** Transformation of stacks: S ~ |S| *)

Inductive tr_stack_item (C:typdefctx) (LLC:ll_typdefctx) (α:alpha) : (var * val) -> (var * val) -> Prop :=
  | tr_stack_item_intro : forall x v v',
      tr_val C LLC α v v' ->
      tr_stack_item C LLC α (x, v) (x, v').

Inductive tr_stack (C:typdefctx) (LLC:ll_typdefctx) (α:alpha) : stack -> stack -> Prop :=
  | tr_stack_intro : forall S S',
      LibList.Forall2 (tr_stack_item C LLC α) S S' ->
      tr_stack C LLC α S S'.

Lemma stack_lookup_tr : forall C LLC α S S' x v,
  tr_stack C LLC α S S' ->
  Ctx.lookup x S = Some v -> 
    exists v', 
       Ctx.lookup x S' = Some v' 
    /\ tr_val C LLC α v v'.
Proof.
  introv HS Hx. inverts HS as HS. induction HS.
  { inverts Hx. }
  { inverts H as Hv. inverts Hx as Hx. case_if in Hx.
    { inverts Hx. exists v'. splits*. unfolds. case_if*. }
    { forwards (v''&Hx'&Hv''): IHHS Hx. exists v''.
      splits*. unfolds. case_if. fold Ctx.lookup. auto. } }
Qed.

(** Transformation of terms: t ~ |t| *)

Inductive tr_trm (C:typdefctx) (LLC:ll_typdefctx) (α:alpha) : trm -> trm -> Prop :=
  | tr_trm_val : forall v v',
      tr_val C LLC α v v' ->
      tr_trm C LLC α (trm_val v) (trm_val v')
  | tr_trm_var : forall x,
      tr_trm C LLC α (trm_var x) (trm_var x)
  | tr_trm_if : forall t0 t1 t2 t0' t1' t2',
      tr_trm C LLC α t0 t0' ->
      tr_trm C LLC α t1 t1' ->
      tr_trm C LLC α t2 t2' ->
      tr_trm C LLC α (trm_if t0 t1 t2) (trm_if t0' t1' t2')
  | tr_trm_let : forall t0 t1 z t0' t1',
      tr_trm C LLC α t0 t0' ->
      tr_trm C LLC α t1 t1' ->
      tr_trm C LLC α (trm_let z t0 t1) (trm_let z t0' t1')
  | tr_trm_binop : forall t1 t2 op t1' t2', 
      tr_trm C LLC α t1 t1' ->
      tr_trm C LLC α t2 t2' ->
      tr_trm C LLC α (trm_app (prim_binop op) (t1::t2::nil)) (trm_app (prim_binop op) (t1'::t2'::nil))
  | tr_trm_get : forall t1 T t1',
      tr_trm C LLC α t1 t1' ->
      tr_trm C LLC α (trm_app (prim_get T) (t1::nil)) (trm_app (prim_ll_get T) (t1'::nil))
  | tr_trm_set : forall t1 t2 T t1' t2',
      tr_trm C LLC α t1 t1' ->
      tr_trm C LLC α (trm_app (prim_set T) (t1::t2::nil)) (trm_app (prim_ll_set T) (t1'::t2'::nil))
  | tr_trm_new : forall T,
      tr_trm C LLC α (trm_app (prim_new T) nil) (trm_app (prim_ll_new T) nil)
 (*| tr_trm_new_array :
      TODO: prim_new_array is needed here. *)
  | tr_trm_struct_access : forall Tfs t1' o Tv f t1 tr,
      Tv \indom C ->
      typing_struct C (typ_var Tv) Tfs ->
      f \indom Tfs ->
      o = (fields_offsets LLC)[Tv][f] ->
      tr_trm C LLC α t1 t1' ->
      tr = trm_app (prim_ll_access Tfs[f]) (t1'::(trm_val (val_int o))::nil) ->
      tr_trm C LLC α (trm_app (prim_struct_access (typ_var Tv) f) (t1::nil)) tr
  | tr_trm_array_access : forall os t2' n tr T' t1' toff T t1 t2,
      typing_array C T T' os ->
      typ_size (typvar_sizes LLC) T' n ->
      tr_trm C LLC α t1 t1' ->
      tr_trm C LLC α t2 t2' ->
      toff = trm_app (prim_binop binop_mul) (t2'::(trm_val (val_int n))::nil) ->
      tr = trm_app (prim_ll_access T') (t1'::toff::nil) ->
      tr_trm C LLC α (trm_app (prim_array_access T) (t1::t2::nil)) tr
  | tr_trm_struct_get : forall t1 T f t1',
      tr_trm C LLC α t1 t1' ->
      tr_trm C LLC α (trm_app (prim_struct_get T f) (t1::nil)) (trm_app (prim_struct_get T f) (t1'::nil))
  | tr_trm_array_get : forall t1 t2 T t1' t2',
      tr_trm C LLC α t1 t1' ->
      tr_trm C LLC α t2 t2' ->
      tr_trm C LLC α (trm_app (prim_array_get T) (t1::t2::nil)) (trm_app (prim_array_get T) (t1'::t2'::nil)).

(* ---------------------------------------------------------------------- *)
(** Lemmas to prove the correctness of the transformation *)

(* Used throughout. Non-error values can't become errors. *)

Lemma not_is_error_tr : forall C LLC α v1 v2,
  tr_val C LLC α v1 v2 ->
  ~ is_error v1 ->
  ~ is_error v2.
Proof.
  introv Htr He. induction Htr; introv HN;
  try solve [ subst ; inverts* HN ]. 
Qed.

(* Basic values are preserved by the transformation. *)

Lemma is_basic_tr : forall C LLC α v1 v2,
  tr_val C LLC α v1 v2 ->
  is_basic v1 ->
  is_basic v2.
Proof.
  introv Htr Hb. induction Htr;
  try solve [ subst ; inverts* Hb ; unfolds~ ].
Qed.

(* The relation typ_size is a function. *)

Lemma functional_typ_size : forall CS T n1 n2,
  typ_size CS T n1 ->
  typ_size CS T n2 ->
  n1 = n2.
Proof.
  introv Hn1 Hn2. gen n2. induction Hn1; intros;
  try solve [ inverts~ Hn2 ].
  { inverts Hn2 as Hn2. forwards~: IHHn1 Hn2. subst~. }
  { inverts Hn2. subst. asserts: (n = n0).
    { applys~ read_extens.
      { congruence. }
      { introv Hi. rewrite <- H in Hi. applys~ H1. } }
    subst~. }
Qed.

(* The relation tr_ll_accesses (the low-level translation of 
   accesses into offsets) is a function. *)

Lemma functional_tr_ll_accesses : forall C LLC π o1 o2,
  tr_ll_accesses C LLC π o1 ->
  tr_ll_accesses C LLC π o2 ->
  o1 = o2.
Proof.
  introv Ho1 Ho2. gen o2. induction Ho1; intros.
  { inverts~ Ho2. }
  { inverts Ho2 as HTa HTn Hπs.
    forwards~ (HTeq&Hoseq): functional_typing_array H HTa. subst.
    forwards~: functional_typ_size H0 HTn. subst.
    forwards~: IHHo1 Hπs. subst~. }
  { inverts Ho2 as HTs HTvin Hfin Hπs.
    forwards~: IHHo1 Hπs. subst~. }
Qed.

(* The relation tr_val is a function. Used in general and also in the
   equality case of the binop. *)

Lemma functional_tr_val : forall C LLC α v v1 v2,
  is_basic v ->
  tr_val C LLC α v v1 ->
  tr_val C LLC α v v2 ->
  v1 = v2.
Proof.
  introv Hv Hv1 Hv2. gen v2. induction Hv1; intros;
  try solve [ inverts* Hv ];
  try solve [ inverts* Hv2 ].
  { inverts Hv2 as Hπ.
    forwards~: functional_tr_ll_accesses H Hπ. subst~. }
Qed.

(* FALSE. The relation tr_ll_accesses is injective. *)

Lemma tr_ll_accesses_inj : forall C LLC π1 π2 o,
  tr_ll_accesses C LLC π1 o ->
  tr_ll_accesses C LLC π2 o ->
    π1 = π2.
Proof.
  admit.
Qed.

(* FALSE. And tr_val is also injective. At least for sure for basic values. *)

Lemma tr_val_inj : forall C LLC φ α T v v1 v2,
  typing_val C LLC φ v1 T ->
  typing_val C LLC φ v2 T ->
  tr_val C LLC α v1 v ->
  tr_val C LLC α v2 v ->
  v1 = v2.
Proof.
  introv HTv1 HTv2 Hv1 Hv2. gen v2. induction Hv1; intros;
  try solve [ inverts~ Hbv1 ];
  try solve [ inverts~ Hv2 ].
  { inverts Hv2 as Hπ Hα. tests: (l = l1).
    { inverts HTv1. admit. }
    { admit. (* Find contradiction because alpha is always a bijection. *) } }
  { admit. }
  { admit. }
Qed.

Lemma read_phi_length_accesses : forall C T T' π1 π2,
  follow_typ C T π1 T' ->
  follow_typ C T π2 T' ->
  length π1 = length π2.
Proof.
  introv Hπ1 Hπ2. gen π2. induction Hπ1; intros.
  skip. skip. skip.
Qed.

Lemma wf_typ_not_rec : forall C T os,
  wf_typ C T ->
  T <> typ_array T os.
Proof.
  introv Hwf HN. rewrite HN in Hwf. inverts Hwf.
Qed.

Lemma typing_follow_typ_one_way : forall os C T' T,
  typing_array C T T' os ->
  ~ (exists π, follow_typ C T' π T).
Proof.
  introv HTa (π&HN). gen π. induction HTa; intros.
  { inverts HN.
    { inverts H; inverts H2. }
    {  }
    {  } }
Qed.

(* FALSE. Contrapositive of the previous statement. *)

Lemma tr_val_inj_cp : forall C LLC α v1 v2 v1' v2',
  is_basic v1 ->
  is_basic v2 ->
  tr_val C LLC α v1 v1' ->
  tr_val C LLC α v2 v2' ->
  v1 <> v2 ->
  v1' <> v2'.
Proof.
  introv Hbv1 Hbv2 Hv1 Hv2 Hneq HN. subst.
  forwards*: tr_val_inj Hbv1 Hbv2 Hv1 Hv2.
Qed.

(* For the [let] case. *)

Lemma tr_stack_add : forall C LLC α z v S v' S',
  tr_stack C LLC α S S' ->
  tr_val C LLC α v v' ->
  tr_stack C LLC α (Ctx.add z v S) (Ctx.add z v' S').
Proof.
  introv HS Hv. constructors~. inverts HS.
  unfolds Ctx.add. destruct* z.
  applys~ Forall2_cons. constructors~.
Qed.

(*  *)

Lemma typ_size_total : forall C LLC T,
  ll_typdefctx_ok C LLC ->
  wf_typ C T ->
  exists n,
    typ_size (typvar_sizes LLC) T n.
Proof.
  introv Hok HwfT. gen LLC. induction HwfT; intros;
  try solve [ repeat constructors~ ].
  { admit. (* Problem with variable size array. *) }
  { admit. }
  { admit. }
Qed.

(* Note: rn it holds trivially because tr_val is not defined on
   undef. Also, it is necessary to change is_uninitialised to look
   for uninitialised values inside the value. *)
Lemma not_is_undef : forall C LLC α v v',
  tr_val C LLC α v v' ->
  ~ is_uninitialised v ->
  ~ is_undef v'.
Proof.
Admitted.

Lemma tr_read_state : forall C LLC α φ m m' l T lw o w π T',
  typing_val C LLC φ m[l] T ->
  tr_ll_val C LLC α T m[l] lw ->
  tr_ll_accesses C LLC π o ->
  tr_state C LLC α φ  m m' ->
  read_state m l π w ->
  (exists w' lw' n,
      typing_val C LLC φ w T'
  /\  tr_ll_val C LLC α T' w' lw'
  /\  typ_size (typvar_sizes LLC) T' n
  /\  read_ll_state m' α[l] o n lw'
  /\  tr_val C LLC α w w').
Proof.
  
Admitted.


(* ---------------------------------------------------------------------- *)
(** Correctness of the transformation *)

(* Hints *)

Hint Constructors red.
Hint Constructors tr_val.

Hint Resolve refl_extends.
Hint Resolve trans_extends.

Hint Extern 1 (~ is_error ?v) => applys not_is_error_tr.
Hint Extern 1 (is_basic ?v) => applys is_basic_tr.

(* The theorem *)

Theorem red_tr : forall m2 t m1 φ S LLC v C S' m1' t',
  red C LLC S m1 t m2 v ->
  (* Typing assumtions *)
  typing LLC (make_env C Γ φ) t T ->
  (* Not error. TODO: Remove, this comes from typing. *)
  ~ is_error v ->
  (* The transformation. *)
  ll_typdefctx_ok C LLC ->
  tr_trm C LLC α t t' ->
  tr_stack C LLC α S S' ->
  tr_state C LLC α φ m1 m1' ->
  exists v' m2' φ',
      extends φ φ'
  /\  tr_state C LLC α φ' m2 m2'
  /\  tr_val C LLC α v v'
  /\  red C LLC S' m1' t' m2' v'.
Proof.
  introv HR He Hok Ht HS Hm1. gen φ t' S' m1'. induction HR; intros;
  try solve [ forwards*: He; unfolds* ];
  try solve [ inverts Ht ].
  { (* val *)
    inverts Ht. exists* v' m1' φ. }
  { (* var *)
    inverts Ht. forwards~ (v'&HCl&Htr): stack_lookup_tr HS H.
    exists* v' m1' φ. }
  { (* if *)
    inverts Ht as Hb HTrue HFalse.
    forwards* (v'&m2'&φ'&Hφ'&Hm2'&Hv'&HR3): IHHR1 Hb HS Hm1.
    inverts* Hv'.
    destruct b;
    forwards* (vr'&m3'&φ''&Hφ''&Hvr'&Hm3'&HR4): IHHR2 HS Hm2';
    exists* vr' m3' φ''. }
  { (* let *) 
    inverts Ht as Ht0 Ht1.
    forwards* (v'&m2'&φ'&Hφ'&Hm2'&Hv'&HR3): IHHR1 Ht0 HS Hm1.
    forwards HS': tr_stack_add z HS Hv'.
    forwards* (vr'&m3'&φ''&Hφ''&Hm3'&Hvr'&HR4): IHHR2 Ht1 HS' Hm2'.
    exists* vr' m3' φ''. }
  { (* binop *)
    inverts Ht as Ht1 Ht2.
    inverts Ht1 as Hv1. inverts Ht2 as Hv2.
    inverts H3; try solve [ exists __ m1' φ; splits~;
    inverts Hv1; inverts Hv2; repeat constructors~ ].
    { exists (val_bool true) m1' φ. splits~.
      forwards~: functional_tr_val Hv1 Hv2. subst.
      repeat constructors*. }
    { exists (val_bool false) m1' φ. splits~.
      admit. (* FALSE. Injectivity results don't hold. *) } }
  { (* get *)
    subst.
    inverts Ht as Ht.
    inverts Ht as Hv.
    inverts Hv as Hπ.
    inverts Hm1 as HD Hdb Htrm.
    inverts H0 as Hi Ha.
    forwards (lw&T'&HT&Hll&Hm1'αl): Htrm Hi.
    forwards* (w'&lw'&n&HT''&Hvr&Hn&Hlw'&Hw'): tr_read_state m m1' vr T.
    { constructors*. }
    { constructors*. }
    exists w' m1' φ. splits*.
    { constructors*. }
    constructors*.
    { admit. (* Is not undefined. *) } } }
Admitted.
















