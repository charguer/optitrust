(**

This file describes transformations of the layout of records and arrays
on the program.

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
(** Here starts the transformation that will be applied to the programs. *)

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
      (forall l, l \indom m <-> α[l] \indom m') ->
      disjoint_blocks m' ->
      (forall l,
        l \indom m ->
          exists lw T,
              typing_val C φ m[l] T
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

(* Very important lemma. *)

Lemma follow_typ_ll_accesses_inj : forall C LLC T T' o1 o2 π1 π2,
  ll_typdefctx_ok C LLC ->
  wf_typdefctx C ->
  wf_typ C T ->
  wf_accesses C π1 ->
  wf_accesses C π2 ->
  follow_typ C T π1 T' ->
  follow_typ C T π2 T' ->
  tr_ll_accesses C LLC π1 o1 ->
  tr_ll_accesses C LLC π2 o2 ->
  o1 = o2 ->
    π1 = π2.
Proof.
  introv Hok HwfC HwfT Hwfπ1 Hwfπ2 Hπ1 Hπ2 Ho1 Ho2 Heq.
  subst. gen π2 o2 LLC. induction Hπ1; intros.
  { forwards*: wf_typ_follow_accesses. }
  { asserts HF: (follow_typ C Ta (access_array Ta i :: π) Tr).
    { constructors*. }
    inverts Ho1 as HTa Hn Hπ Hige Hnge.
    forwards~ (HeqT&Heqos): functional_typing_array H HTa. subst.
    destruct π2.
    { inverts Hπ2. forwards~ HN: wf_typ_follow_accesses HF. }
    { forwards~ (i'&Heqa): follow_typ_array_access H Hπ2. subst.
      inverts Ho2 as HTa' Hn' Hπ2' Hi'ge Hn'ge Heq.
      forwards~ (HeqT'&Heqos): functional_typing_array HTa HTa'. subst.
      forwards~: functional_typ_size Hn Hn'. subst.
      asserts: (i=i').
      { tests: (π = nil).
        { inverts HF as HTa'' HF. inverts HF. inverts Hπ.
          inverts Hπ2 as HTa''' HF.
          forwards~ (HTeq&Hoseq): functional_typing_array HTa'' HTa'''. subst.
          asserts Hnil: (π2 = nil).
          { inverts Hwfπ2. 
            forwards~: wf_typing_array HTa'' HwfT.
            forwards*: wf_typ_follow_accesses T. }
          subst. inverts Hπ2'.
          admit. (* This is math. *) }
        { clear HTa'. inverts HF as HTa' HF.
          forwards~ (HTeq&Hoseq): functional_typing_array HTa HTa'. subst.
          clear HTa'. inverts Hπ2 as HTa' HF'.
          forwards~ (HTeq&Hoseq): functional_typing_array HTa HTa'. subst.
          forwards~: typ_size_gt_offset Hn HF Hπ.
          tests: (π2 = nil).
          { admit. (* This is maths. *) }
          { forwards~: typ_size_gt_offset Hn HF' Hπ2'.
            admit. (* This is maths. *) } } }
      subst. asserts: (o=o0).
      { applys* Z.add_reg_l. }
      subst. fequals. applys* IHHπ1.
      { forwards~: wf_typing_array HTa HwfT. }
      { inverts~ Hwfπ1. }
      { inverts~ Hwfπ2. }
      { clear HTa'. inverts Hπ2 as HTa'.
        forwards~ (HeqT'&Heqos): functional_typing_array HTa HTa'.
        subst~. } } }
  { asserts HF: (follow_typ C Ts (access_field Ts f :: π) Tr).
    { constructors*. }
    inverts Ho1 as HTs HTvin Hfin Hπ Hoffge.
    remember (fields_offsets LLC) as FOff.
    forwards~ HeqTfs: functional_typing_struct H HTs. subst.
    destruct π2.
    { inverts Hπ2. forwards~ HN: wf_typ_follow_accesses HF. }
    { forwards~ (f'&Heqa): follow_typ_struct_access H Hπ2. subst.
      inverts Ho2 as HTs' _ Hfin' Hπ2' Hoffge' Heq.
      forwards~ HeqTfs: functional_typing_struct H HTs'. subst.
      asserts: (f=f').
      { admit. (* TODO: Quite technical but the arithmetic should work. *) } 
      subst. asserts: (o=o0).
      { applys* Z.add_reg_l. }
      subst. fequals. applys* IHHπ1.
      { forwards~: wf_typing_struct HTs HwfT f'. }
      { inverts~ Hwfπ1. }
      { inverts~ Hwfπ2. }
      { clear HTs'. inverts Hπ2 as HTs'.
        forwards~: functional_typing_struct HTs HTs'. subst~. } } }
Qed.

(* And tr_val is also injective. At least for sure for basic values. *)

Lemma tr_val_inj : forall C LLC φ α T v v1 v2,
  ll_typdefctx_ok C LLC ->
  wf_typdefctx C ->
  wf_phi C φ ->
  wf_typ C T ->
  is_basic v1 ->
  is_basic v2 ->
  typing_val C φ v1 T ->
  typing_val C φ v2 T ->
  tr_val C LLC α v1 v ->
  tr_val C LLC α v2 v ->
  v1 = v2.
Proof.
  introv Hok HC Hφ HT Hbv1 Hbv2 HTv1 HTv2 Hv1 Hv2. gen v2.
  induction Hv1; intros;
  try solve [ inverts~ Hv2 ];
  try solve [ inverts~ Hbv1 ].
  { inverts Hv2 as Hπ Hα. tests: (l = l1).
    { inverts HTv1. fequals. inverts_head read_phi.
      forwards~ Hwfv: typing_val_wf_val HTv2 Hφ HT.
      inverts Hwfv as Hwfπ0.
      inverts HTv2 as HRφ.
      inverts HRφ as Hl1in HF.
      forwards~ Hwfφl1: Hφ Hl1in.
      forwards~ Hwfπ: follow_typ_wf_accesses Hwfφl1 H1.
      applys* follow_typ_ll_accesses_inj. }
    { admit. (* Find contradiction because alpha is a bijection. *) } }
Qed.

(* Contrapositive of the previous statement. *)

Lemma tr_val_inj_cp : forall C LLC φ α T v1 v2 v1' v2',
  ll_typdefctx_ok C LLC ->
  wf_typdefctx C ->
  wf_phi C φ ->
  wf_typ C T ->
  is_basic v1 ->
  is_basic v2 ->
  typing_val C φ v1 T ->
  typing_val C φ v2 T ->
  tr_val C LLC α v1 v1' ->
  tr_val C LLC α v2 v2' ->
  v1 <> v2 ->
  v1' <> v2'.
Proof.
  introv Hok HC Hφ HT Hbv1 Hbv2 HTv1 HTv2 Hv1 Hv2. 
  introv Hneq HN. subst.
  forwards*: tr_val_inj HTv1 HTv2 Hv1 Hv2.
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

(* The typ_size function Type -> nat is well-defined. *)

Lemma typ_size_total : forall C LLC T,
  ll_typdefctx_ok C LLC ->
  wf_typ C T ->
  exists n,
    typ_size (typvar_sizes LLC) T n.
Proof.
  introv Hok HwfT. induction HwfT; intros;
  try solve [ repeat constructors~ ].
  { asserts HK: (exists K, os = Some K).
    { admit. (* TODO: Variable size arrays are problematic. *) }
    forwards~ (K&Heq): HK.
    forwards~ (n&Hn): IHHwfT.
    exists (n * K). subst. constructors~. }
  { asserts HFS: (exists FS, dom FS = dom Tfs /\ 
      forall f, f \indom Tfs -> typ_size (typvar_sizes LLC) Tfs[f] FS[f]).
    { admit. (* TODO: Don't know how to explicitly prove existence. *) }
    destruct HFS as (FS&HDFS&HFS).
    exists __. constructors*. }
  { forwards~ (n&Hn): IHHwfT. exists __. constructors~.
    inverts Hok. simpls. rewrite~ <- H1. }
Qed.

(* Connection between unininitialized values and
   undefined lists of words. *)

Lemma is_undef_uninitialized: forall C LLC α T lw v,
  is_undef (val_words lw) ->
  tr_ll_val C LLC α T v lw ->
  is_uninitialized v.
Proof.
Admitted.

Lemma not_is_undef : forall C LLC α T v lw,
  tr_ll_val C LLC α T v lw ->
  ~ is_uninitialized v ->
  ~ is_undef (val_words lw).
Proof.
  introv Htr Hnu HN. induction Htr;
  try solve [ inverts HN as (Hi&Hx); rew_array in Hx;
  case_if in Hx; rewrite index_eq_index_length in Hi;
  rewrite length_one in Hi; inverts Hi; math ].
  { inverts HN as (Hi&Hx).
    rew_array in Hx.
    repeat case_if in Hx.
    rewrite index_eq_index_length in Hi.
    repeat rewrite length_cons in Hi.
    rewrite length_nil in Hi.
    inverts Hi. math. }
  { inverts HN as (Hi&Hx).
    applys~ Hnu. constructors.
    asserts Hundef: (exists i, index a i /\ is_undef (val_words a'[i])).
    { admit. (* TODO. This is true. An argument about
      indices, arrays and concatenation is needed. *) }
    destruct Hundef as (i'&Hi'&Hundef).
    exists i'. splits~.
    forwards~ Htr: H1 Hi'.
    applys* is_undef_uninitialized. }
  { inverts HN as (Hi&Hx). admit.
    (* TODO: Similar problem as previous case. *) }
Qed.

(* The relation tr_val is well-defined as a function. *)

Lemma tr_val_total : forall C LLC α v,
  exists v', tr_val C LLC α v v'.
Proof.
Admitted.

Lemma functional_tr_ll_val : forall C LLC α T v lw lw',
  tr_ll_val C LLC α T v lw ->
  tr_ll_val C LLC α T v lw' ->
  lw = lw'.
Proof.
  introv Htrlw Htrlw'. gen lw'. induction Htrlw; intros;
  try solve [ inverts~ Htrlw' ].
  { inverts Htrlw' as Htro.
    forwards~: functional_tr_ll_accesses Htro H.
    subst. fequals~. }
  { inverts Htrlw'. asserts: (a' = a'0).
    { applys* eq_of_extens.
      { congruence. }
      { introv Hi. asserts Hi': (index a i).
        { rewrite index_eq_index_length in *. rewrite~ <- H0. }
        forwards* Htra'i: H1 Hi'. } }
      subst~. }
  { inverts Htrlw'. asserts: (s' = s'0).
    { applys* eq_of_extens.
      { congruence. }
      { introv Hi. subst. asserts Hi': (index s'0 i).
        { rewrite index_eq_index_length in *.
          rewrite H16. rewrite~ <- H5. }
        forwards*: functional_typing_struct H2 H13. subst.
        forwards* Htrs'0i: H17 Hi'. } }
      subst~. }
Qed.

Lemma equiv_tr_ll_val : forall C LLC φ α T T' v lw lw',
  typing_val C φ v T ->
  typing_val C φ v T' ->
  tr_ll_val C LLC α T v lw ->
  tr_ll_val C LLC α T' v lw' ->
  lw = lw'.
Proof.
  introv HT HT' HtrT HtrT'. gen LLC α T' lw'.
  induction HT; intros;
  try solve [ inverts HtrT; inverts~ HtrT' ].
  { inverts HtrT as Htro. inverts HtrT' as Htro0.
    forwards~: functional_tr_ll_accesses Htro Htro0.
    subst. fequals. }
  { inverts HtrT. inverts HtrT'.
    asserts: (s' = s'0).
    { applys* eq_of_extens.
      { congruence. }
      { introv Hi.
        forwards* Htrs'i: H12 Hi.
        asserts Hi': (index s'0 i).
        { rewrite index_eq_index_length in *.
          rewrite H17. rewrite~ <- H10. }
        forwards* Htrs'0i: H19 Hi'.
        forwards*: functional_typing_struct H7 H13. subst.
        forwards*: functional_tr_ll_val Htrs'i Htrs'0i. } }
    subst~. }
  { inverts HtrT. inverts HtrT'. 
    asserts: (a' = a'0).
    { applys* eq_of_extens.
      { congruence. }
      { introv Hi. asserts Hi': (index a i).
        { rewrite index_eq_index_length in *. rewrite~ <- H7. }
        forwards* Htra'i: H9 Hi'.
        forwards* Htra'0i: H12 Hi'.
        forwards*: functional_tr_ll_val Htra'i Htra'0i. } }
    subst~. }
Qed.

Lemma equiv_typ_size : forall C LLC φ v T T' n n',
  typing_val C φ v T ->
  typing_val C φ v T' ->
  typ_size (typvar_sizes LLC) T n ->
  typ_size (typvar_sizes LLC) T' n' ->
  n = n'.
Proof.
Admitted.

(* Lemma for the [get] case. *)

Lemma tr_read_state : forall C LLC α φ m m' l T lw o w π T',
  ll_typdefctx_ok C LLC ->
  wf_typ C T ->
  follow_typ C T π T' ->
  typing_val C φ m[l] T ->
  typing_val C φ w T' ->
  tr_ll_val C LLC α T m[l] lw ->
  tr_ll_accesses C LLC π o ->
  tr_state C LLC α φ  m m' ->
  read_state m l π w ->
  (exists w' lw' n,
      tr_ll_val C LLC α T' w' lw'
  /\  typ_size (typvar_sizes LLC) T' n
  /\  read_ll_state m' α[l] o n lw'
  /\  tr_val C LLC α w w').
Proof.
  introv Hok HwfT HF HT HT' Hlw Ho Hm HR.
  gen LLC α m' l m o φ. induction HF; intros.
  { inverts Ho. inverts HR as Hl HR. inverts HR.
    forwards~ (n&Hn): typ_size_total C LLC T.
    forwards~ (v'&Hv'): tr_val_total C LLC α m[l].
    exists v' lw n. splits~.
    { admit. }
    { inverts Hm as HDm Hdb Hm.
      forwards~ (lw'&T''&HT''&Hlw'&Hm'αl): Hm Hl.
      forwards~: equiv_tr_ll_val HT HT'' Hlw Hlw'. subst.
      constructors~.
      { apply~ HDm. }
      { eapply Hm'αl. }
      { asserts Hllw: (length lw' = n).
        { applys* typ_size_length_lw.
          forwards* (n'&Hn'): typ_size_total T''.
          { admit. (* TODO: wf_typ C T'' *) }
          forwards~: equiv_typ_size HT HT'' Hn Hn'. 
          subst~. }
        unfolds. subst. splits; try math.
        rewrite drop_zero. rewrite~ take_full_length. } } }
Admitted.


(* ---------------------------------------------------------------------- *)
(** Correctness of the transformation *)

(* Hints *)

Hint Constructors red redbinop.
Hint Constructors tr_trm tr_val.
Hint Constructors wf_trm wf_val wf_accesses wf_typ.

Hint Resolve wf_red.

Hint Resolve refl_extends.
Hint Resolve trans_extends.

Hint Extern 1 (~ is_error ?v) => applys not_is_error_tr.
Hint Extern 1 (is_basic ?v) => applys is_basic_tr.

(* The theorem *)

(* TODO: There are a few problems with the typing assumption.
   But they should be dealt with in a similar way as those same problems
   in type soundness. For now, I will prove the version without the
   extension of φ. *)

Theorem red_tr_warmup : forall m2 t T Γ m1 φ S LLC v C S' m1' t',
  red C LLC S m1 t m2 v ->
  typing C φ Γ t T ->
  ~ is_error v ->
  (* The transformation. *)
  ll_typdefctx_ok C LLC ->
  tr_trm C LLC α t t' ->
  tr_stack C LLC α S S' ->
  tr_state C LLC α φ m1 m1' ->
  (* Wellformedness. *)
  wf_typdefctx C ->
  wf_trm C t ->
  wf_stack C S ->
  wf_state C m1 ->
  wf_typ C T ->
  wf_gamma C Γ ->
  wf_phi C φ ->
  exists v' m2',
      tr_state C LLC α φ m2 m2'
  /\  tr_val C LLC α v v'
  /\  red C LLC S' m1' t' m2' v'.
Proof.
  introv HR HT He Hok Ht HS Hm1.
  introv HwfC Hwft HwfS Hwfm1 HwfT HwfΓ Hwfφ.
  gen φ Γ T t' S' m1'. induction HR; intros;
  try solve [ forwards*: He; unfolds* ];
  try solve [ inverts Ht ].
  { (* val *)
    inverts Ht. exists* v' m1'. }
  { (* var *)
    inverts Ht. forwards~ (v'&HCl&Htr): stack_lookup_tr HS H.
    exists* v' m1'. }
  { (* if *)
    inverts Ht as Hb HTrue HFalse.
    inverts HT as HT0 HT1 HT2.
    inverts Hwft as Hwft0 Hwft1 Hwft1.
    forwards* (v'&m2'&Hm2'&Hv'&HR3): IHHR1 Hb HS Hm1.
    inverts* Hv'.
    destruct b;
    forwards* (vr'&m3'&Hvr'&Hm3'&HR4): IHHR2 HS Hm2';
    try solve [ forwards*: wf_red HR1 ];
    exists* vr' m3'. }
  { (* let *) 
    inverts Ht as Ht0 Ht1.
    inverts HT as HT1 HT2.
    inverts Hwft as Hwft1 Hwft2.
    forwards* (v'&m2'&Hm2'&Hv'&HR3): IHHR1 Ht0 HS Hm1.
    { applys* wf_typing_typ. }
    forwards HS': tr_stack_add z HS Hv'.
    forwards*: wf_red HR1.
    asserts HwfΓ': (wf_gamma C (Ctx.add z T1 Γ)).
    { applys* wf_gamma_add. applys* wf_typing_typ. }
    forwards* (vr'&m3'&Hm3'&Hvr'&HR4): IHHR2 Ht1 HS' Hm2'.
    { applys* wf_stack_add. }
    exists* vr' m3'. }
  { (* binop *)
    inverts Ht as Ht1 Ht2.
    inverts Ht1 as Hv1. inverts Ht2 as Hv2.
    inverts H3; try solve [ exists __ m1'; splits~;
    inverts Hv1; inverts Hv2; repeat constructors~ ].
    { exists (val_bool true) m1'. splits~.
      forwards~: functional_tr_val Hv1 Hv2. subst.
      repeat constructors*. }
    { exists (val_bool false) m1'. splits~.
      inverts HT as HwfT0 HbT0 Hv1T0 Hv2T0.
      inverts Hv1T0 as Hv1T0. inverts Hv2T0 as Hv2T0.
      forwards*: tr_val_inj_cp Hv1T0 Hv2T0.
      repeat constructors*. } }
  { (* get *)
    subst.
    inverts Ht as Ht.
    inverts Ht as Hv.
    inverts Hv as Hπ.
    inverts Hm1 as HD Hdb Htrm.
    inverts H0 as Hl Ha.
    forwards (lw&T'&HT'&Hll&Hm1'αl): Htrm Hl.
    forwards* (w'&lw'&n&HT''&Hvr&Hn&Hlw'&Hw'): tr_read_state m m1' vr T.
    { constructors*. }
    { constructors*. }
    exists w' m1'. splits*.
    { constructors*. }
    constructors*.
    { introv Hu. unfolds is_undef. destruct* w'. inverts Hw'. } }
  { (* set *)
    admit. }
  { (* new *)
    admit. }
  { (* struct access *)
    admit. }
  { (* array access *)
    admit. }
  { (* struct get *)
    admit. }
  { (* array get *)
    admit. }
  { (* args 1 *)
    admit. }
  { (* args 2 *)
    admit. }
Admitted.
















