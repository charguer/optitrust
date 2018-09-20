(**

This file describes transformations of the layout of records and arrays on
the memory. It includes some important definitions and results that are
used both in Semantics.v and TrLowLevel.v.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)


Set Implicit Arguments.
Require Export Typing.


(* ---------------------------------------------------------------------- *)
(** Size of types *)

(** Used to compute the size of a type. Assuming the size of type variables
    are known. Used throughout in the low-level transformation/semantics. *)

Inductive typ_size (CS:ll_typdefctx_typvar_sizes) : typ -> size -> Prop :=
  | typ_size_unit :
      typ_size CS (typ_unit) 1
  | typ_size_int :
      typ_size CS (typ_int) 1
  | typ_size_double :
      typ_size CS (typ_double) 2
  | typ_size_bool :
      typ_size CS (typ_bool) 1
  | typ_size_ptr : forall T',
      typ_size CS (typ_ptr T') 1
  | typ_size_array : forall n T' k,
      k >= 0 ->
      typ_size CS T' n ->
      typ_size CS (typ_array T' (Some k)) (n*k)
  | typ_size_struct : forall Tfs n (m:monoid_op int) (g:field->size->size),
      dom Tfs = dom n ->
      (forall (f:field),
        f \indom Tfs ->
        typ_size CS Tfs[f] n[f]) ->
      m = monoid_make (fun a b => a + b) 0 ->
      g = (fun k v => v) ->
      typ_size CS (typ_struct Tfs) (fold m g n)
  | typ_size_typvar : forall Tv,
      Tv \indom CS ->
      typ_size CS (typ_var Tv) CS[Tv].


(* ---------------------------------------------------------------------- *)
(** Some previous necessary definitions *)

(** Conversion from lists to maps. *)

Axiom list_to_map : forall A B, list A -> list B -> map A B.

Axiom list_to_map_spec : forall (ks:list field) (vs:list offset),
      length ks = length vs
  /\  forall (i:int),
        index ks i ->
            ks[i] \indom (list_to_map ks vs)
        /\  (list_to_map ks vs)[ks[i]] = vs[i].

(** Function used to argue about the offsets. *)

Inductive prefix_sum : list int -> list int -> int -> Prop :=
  | prefix_sum_nil : forall acc,
      prefix_sum nil nil acc
  | prefix_sum_cons : forall l l' acc i,
      prefix_sum l l' (i + acc) ->
      prefix_sum (i::l) (acc::l') acc.

(** prefix_sum l l' <===> |l| = |l'| *)

Lemma prefix_sum_length : forall l l' n,
  prefix_sum l l' n ->
  length l = length l'.
Proof using.
  introv Hps. induction Hps.
  { rewrite~ length_nil. }
  { repeat rewrite length_cons. rewrite~ IHHps. }
Qed.

(** prefix_sum l l' <===> l'[i] = Σ 0<=j<i l[j] *)

Lemma prefix_sum_spec : forall l l' n,
  prefix_sum l l' n ->
  (forall i,
    index l i ->
    l'[i] = fold_left Z.add n (take i l)).
Proof using.
  introv Hps. induction Hps.
  { introv Hi. false. rewrite index_eq_index_length in Hi.
    rewrite length_nil in Hi. inverts Hi. math. }
  { introv Hi. rewrite read_cons_case. case_if.
    { subst. eauto. }
    { lets Hi': Hi.
      rewrite index_eq_index_length in Hi.
      rewrite int_index_eq in Hi.
      destruct Hi as (Hle&Hlt).
      rewrite length_cons in Hlt.
      rewrite take_cons_pos; try solve [ math ].
      rewrite fold_left_cons.
      asserts Hl: (index (length l) (i0 - 1)).
      { rewrite int_index_eq. math. }
      rewrite <- index_eq_index_length in Hl.
      forwards*: IHHps Hl. } }
Qed.


(* ---------------------------------------------------------------------- *)
(** Type-directed transformation to low-level *)

(** Check that the LLC and the C are coherent. *)

Inductive ll_typdefctx_ok (C:typdefctx) (LLC:ll_typdefctx) : Prop :=
  | low_level_ctx_ok_intros : forall CS CFOrd CFOff,
      LLC = make_ll_typdefctx CS CFOff CFOrd ->
      (* Assumptions regarding size. For basic types and arrays. *)
      dom C = dom CS ->
      (forall Tv,
        0 <= CS[Tv]) ->
      (* Same fields in C and LLC. *)
      (forall Tv Tfs,
        Tv \indom C ->
        typing_struct C (typ_var Tv) Tfs ->
            dom Tfs = dom CFOff[Tv]
        /\  dom Tfs = to_set CFOrd[Tv]) ->
      (* Coherency between the sizes. *)
      (forall Tv Tfs,
        Tv \indom C ->
        typing_struct C (typ_var Tv) Tfs ->
        (exists FT FS FO,
            (* Fields types. *)
            FT = List.map (fun f => Tfs[f]) CFOrd[Tv]
            (* Fields sizes. *)
        /\  List.Forall2 (typ_size CS) FT FS
            (* Fields offsets. *)
        /\  prefix_sum FS FO 0
            (* The relationship. *)
        /\  CS[Tv] = fold_right Z.add 0 FS
        /\  CFOff[Tv] = list_to_map CFOrd[Tv] FO)) ->
      ll_typdefctx_ok C LLC.

(** Given a list of accesses, computes the offset. Used to translate
    pointer values. *)

Inductive tr_ll_accesses (C:typdefctx) (LLC:ll_typdefctx) : accesses -> offset -> Prop :=
  | tr_ll_accesses_nil :
      tr_ll_accesses C LLC nil 0%Z
  | tr_ll_accesses_access_array : forall T T' K πs i n o,
      typing_array C T T' (Some K) ->
      typ_size (typvar_sizes LLC) T' n ->
      tr_ll_accesses C LLC πs o ->
      (0 <= i < K)%Z ->
      tr_ll_accesses C LLC ((access_array T i)::πs) ((i * n) + o)
  | tr_ll_accesses_access_field : forall Tfs FO πs Tv f o,
      typing_struct C (typ_var Tv) Tfs ->
      FO = fields_offsets LLC ->
      Tv \indom FO ->
      f \indom FO[Tv] ->
      tr_ll_accesses C LLC πs o ->
      (0 <= FO[Tv][f])%Z ->
      tr_ll_accesses C LLC ((access_field (typ_var Tv) f)::πs) (FO[Tv][f] + o).

(** Relates values with a list of words. This is how the memory is
    transformed. *)

Inductive tr_ll_val (C:typdefctx) (LLC:ll_typdefctx) (α:alpha) : typ -> val -> list word -> Prop :=
  | tr_ll_val_unit :
      tr_ll_val C LLC α typ_unit val_unit (word_int 0%Z::nil)
  | tr_ll_val_bool : forall b,
      tr_ll_val C LLC α typ_bool (val_bool b) (word_int (if b then 1 else 0)%Z::nil)
  | tr_ll_val_int : forall i,
      tr_ll_val C LLC α typ_int (val_int i) (word_int i::nil)
  | tr_ll_val_double : forall d,
      tr_ll_val C LLC α typ_double (val_double d) (word_int d::word_int d::nil)
  | tr_ll_val_abstract_ptr : forall T π l o,
      tr_ll_accesses C LLC π o ->
      tr_ll_val C LLC α (typ_ptr T) (val_abstract_ptr l π) ((word_int (α[l] + o))::nil)
  | tr_ll_val_array : forall k T a a',
      length a = k ->
      length a' = k ->
      (forall i,
        index a i ->
        tr_ll_val C LLC α T a[i] a'[i]) ->
      tr_ll_val C LLC α (typ_array T (Some k)) (val_array T a) (concat a')
  | tr_ll_val_struct : forall FCOrd Tv Tfs sc st s s',
      FCOrd = fields_order LLC ->
      Tv \indom FCOrd ->
      Tv \indom C ->
      typing_struct C (typ_var Tv) Tfs ->
      sc = List.map (fun f => s[f]) FCOrd[Tv] ->
      st = List.map (fun f => Tfs[f]) FCOrd[Tv] ->
      length s' = length FCOrd[Tv] ->
      (forall i,
        index s' i ->
        tr_ll_val C LLC α st[i] sc[i] s'[i]) ->
      tr_ll_val C LLC α (typ_var Tv) (val_struct (typ_var Tv) s) (concat s').


(* ---------------------------------------------------------------------- *)
(** Semantics of the low-level memory accesses *)

(** l[i:i+n] = l' *)

Definition list_slice {A:Type} (l:list A) (i:int) (n:int) (l':list A) : Prop :=
      l' = take n (drop i l)
  /\  0 <= n
  /\  0 <= i
  /\  i + n <= length l.

(** m(l)[o:o+n] = ws *)

Inductive read_ll_state (m:state) (l:loc) (o:offset) (n:size) (ws':words) : Prop :=
  | read_ll_state_intro : forall ws,
      l \indom m ->
      m[l] = val_words ws ->
      list_slice ws o n ws' ->
      read_ll_state m l o n ws'.

(** l[0:i] ++ l' ++ [i+|l'|:|l|-1] = lr *)

Definition list_slice_update {A:Type} (l:list A) (i:int) (l':list A) (lr:list A) : Prop :=
      lr = (take i l) ++ l' ++ (drop (i + length l') l)
  /\  0 <= i
  /\  i + length l' <= length l.

(** m(l)[0:o] ++ ws ++ m(l)[o+|ws|:|m(l)|-1] = m'(l) *)

Inductive write_ll_state (m:state) (l:loc) (o:offset) (ws':words) (m':state) : Prop :=
  | write_ll_state_intro : forall ws ws'',
      l \indom m ->
      m[l] = val_words ws ->
      list_slice_update ws o ws' ws'' ->
      m' = m[l := (val_words ws'')] ->
      write_ll_state m l o ws' m'.


(* ---------------------------------------------------------------------- *)
(** General results about these predicates. *)

Section LowLevelLemmas.

(** The relation typ_size is a function. *)

Lemma functional_typ_size : forall CS T n1 n2,
  typ_size CS T n1 ->
  typ_size CS T n2 ->
  n1 = n2.
Proof using.
  introv Hn1 Hn2. gen n2. induction Hn1; intros;
  try solve [ inverts~ Hn2 ].
  { inverts Hn2 as Hk Hn2. forwards~: IHHn1 Hn2. subst~. }
  { inverts Hn2. subst. asserts: (n = n0).
    { applys~ read_extens.
      { congruence. }
      { introv Hi. rewrite <- H in Hi. applys~ H1. } }
    subst~. }
Qed.

(** If the low-level context is properly defined then the sizes should 
    be positive. *)

Lemma ll_typdefctx_sizes_pos : forall C LLC CS,
  ll_typdefctx_ok C LLC ->
  CS = typvar_sizes LLC ->
  (forall Tv,
    Tv \indom C ->
    0 <= CS[Tv]).
Proof using.
  introv Hok Heq. inverts~ Hok. simpls. subst~.
Qed.

(** If the low-level context is properly defined all type variables
    should have a size assigned to it. *)

Lemma ll_typdefctx_typvar_sizes_dom : forall C LLC,
  ll_typdefctx_ok C LLC ->
  dom C = dom (typvar_sizes LLC).
Proof using.
  introv Hok. inverts~ Hok.
Qed.

(** Type sizes are positive. *)

Lemma typ_size_pos : forall C LLC T n,
  ll_typdefctx_ok C LLC ->
  typ_size (typvar_sizes LLC) T n ->
  0 <= n.
Proof using.
  introv Hok Hn. induction Hn; intros; try solve [ math ].
  { asserts: (0 <= k).
    { math. }
    applys~ Z.mul_nonneg_nonneg. }
  { admit. (* TODO: Some result on fold should do it. *) }
  { forwards* HCS: ll_typdefctx_sizes_pos Hok.
    rewrite ll_typdefctx_typvar_sizes_dom at 1; eauto. }
Qed.

(** Offsets are always positive. *)

Lemma accesses_offset_gez : forall C LLC π o,
  ll_typdefctx_ok C LLC ->
  tr_ll_accesses C LLC π o ->
  0 <= o.
Proof using.
  introv Hok Hπ. induction Hπ.
  { math. }
  { inverts H1 as Hi HK.
    forwards* Hn: typ_size_pos.
    apply Zle_lt_or_eq in Hi.
    apply Zle_lt_or_eq in Hn.
    apply Zle_lt_or_eq in IHHπ.
    inverts Hi; inverts Hn; inverts IHHπ;
    try solve [ try forwards*: Z.mul_pos_pos i n; math ]. }
  { apply Zle_lt_or_eq in H3.
    apply Zle_lt_or_eq in IHHπ.
    inverts H3; inverts IHHπ; math. }
Qed.

(** The relation tr_ll_accesses (the low-level translation of accesses 
    into offsets) is a function. *)

Lemma functional_tr_ll_accesses : forall C LLC π o1 o2,
  tr_ll_accesses C LLC π o1 ->
  tr_ll_accesses C LLC π o2 ->
  o1 = o2.
Proof using.
  introv Ho1 Ho2. gen o2. induction Ho1; intros.
  { inverts~ Ho2. }
  { inverts Ho2 as HTa HTn Hπs.
    forwards~ (HTeq&Hoseq): functional_typing_array H HTa. subst.
    forwards~: functional_typ_size H0 HTn. subst.
    forwards~: IHHo1 Hπs. subst~. }
  { inverts Ho2 as HTs HTvin Hfin Hπs.
    forwards~: IHHo1 Hπs. subst~. }
Qed.

(** Lemma about length of concatenation. TODO: Move somewhere else. *)

Lemma length_concat_fixed : forall A (l:list (list A)) n,
  (forall i,
    index l i ->
    length l[i] = n) ->
  length (concat l) = n * length l.
Proof using.
  introv Hl. gen A n. induction l; intros.
  { rewrite concat_nil. repeat rewrite~ length_nil. math. }
  { rewrite concat_cons. rewrite length_app.
    rewrite length_cons. forwards Heq: IHl n.
    { introv Hi. forwards~: Hl (i+1).
      { rewrite index_eq_index_length in *.
        rewrite length_cons. rewrite int_index_eq in *. math. }
      rewrite read_cons_case in H. case_if.
      { rewrite index_eq_inbound in Hi. math. }
      { asserts Heq: (i+1-1 = i). { math. }
        rewrite Heq in H. auto. } }
    rewrite Heq. forwards~ Heq': Hl 0.
    { rewrite index_eq_inbound. rewrite length_cons. math. }
    rewrite read_zero in Heq'. rewrite Heq'.
    rewrite Z.mul_add_distr_l. math. }
Qed.

(** Lemma about length of concatenation. TODO: Move somewhere else. *)

Lemma length_concat_variable : forall A (l:list (list A)) (n:list int),
  length l = length n ->
  (forall i,
    index l i ->
    length l[i] = n[i]) ->
  length (concat l) = fold_right Z.add 0 n.
Proof using.
  introv Hl Hn. gen A n. induction l; intros.
  { rewrite concat_nil. rewrite length_nil in *.
    symmetry in Hl. forwards~ Hl': length_zero_inv Hl.
    subst. rewrite~ fold_right_nil. }
  { rewrite concat_cons. rewrite length_app.
    rewrite length_cons in Hl.
    forwards~ (a'&l'&Ha'l'): length_pos_inv_cons n.
    { rewrite <- Hl. math. }
    subst. rewrite length_cons in Hl.
    forwards~ Hlc: IHl l'.
    { math. }
    { introv Hi.
      forwards~ Hlli: Hn (i+1).
      { rewrite index_eq_index_length in *.
        rewrite length_cons.
        rewrite int_index_eq in *. math. }
      { rewrite read_cons_case in Hlli. case_if.
        { rewrite index_eq_inbound in Hi. math. }
        { asserts Heq: (i+1-1 = i). { math. }
          rewrite Heq in Hlli. rewrite Hlli.
          rewrite read_cons_case. case_if.
          rewrite~ Heq. } } }
    rewrite Hlc. forwards~ Hla: Hn 0.
    { rewrite index_eq_inbound.
      rewrite length_cons. math. }
    repeat rewrite read_zero in Hla.
    rewrite Hla. rewrite~ fold_right_cons. }
Qed.

(** Relationship between size of types and the translation of values. *)

Lemma typ_size_length_lw : forall C α v LLC T lw n,
  ll_typdefctx_ok C LLC ->
  tr_ll_val C LLC α T v lw ->
  typ_size (typvar_sizes LLC) T n ->
  length lw = n.
Proof using.
  introv HLLC Htr Hn. gen α v lw. induction Hn; intros;
  try solve [ inverts Htr; rewrite~ length_one ].
  { (* double *)
    inverts Htr. rewrite length_cons. rewrite~ length_one. }
  { (* array *)
    inverts Htr as Hl Htr. rewrite <- Hl.
    applys~ length_concat_fixed. introv Hi'. asserts Hi: (index a i).
    { rewrite index_eq_index_length in *. rewrite~ <- Hl. }
    forwards~ Htrai: Htr Hi. applys* IHHn. }
  { (* struct *)
    inverts Htr as HTvin' HTvin'' HT Hl Htr.
    inverts HLLC as HDC Hpos HDstr Hfields. simpls.
    forwards~ (FT&FS&FO&HFT&HFS&Hps&HCSTv&HCFOffTv): Hfields HTvin'' HT.
    rewrite HCSTv at 1. applys~ length_concat_variable.
    { asserts HlFS: (length FS = length FO).
      { applys* prefix_sum_length. }
      asserts HlFO: (length FO = length CFOrd[Tv]).
      { forwards~ (HR&_): list_to_map_spec CFOrd[Tv] FO. }
      rewrite Hl. rewrite HlFS at 1. rewrite~ <- HlFO. }
    introv Hi. forwards~ Htrs'i: Htr Hi.
    remember CS as CS'.
    remember CFOrd as CFOrd'.
    remember CFOff as CFOff'.
    remember (make_ll_typdefctx CS' CFOff' CFOrd') as LLC'.
    admit. (* TODO: Need to change the approach to prove this. *) }
Qed.

(** If T --π--> T' then |T| >= |T'|. *)

Lemma follow_typ_size : forall C LLC π T T' n n',
  ll_typdefctx_ok C LLC ->
  follow_typ C T π T' ->
  typ_size (typvar_sizes LLC) T n ->
  typ_size (typvar_sizes LLC) T' n' ->
  n' <= n.
Proof using.
  introv Hok Hπ Hn Hn'. gen n n'. induction Hπ; intros.
  { forwards~: functional_typ_size Hn Hn'. subst. math. }
  { inverts Hn; try solve [ inverts H ].
    { asserts Hk: (0%Z < k).
      { admit. (* TODO: More index assumptions needed. *) }
      inverts H.
      forwards* Hn0: IHHπ n0.
      admit. (* TODO: This is maths but can't find the lemma. *) }
    { admit. } }
  { admit. }
Qed.

(** If |T| = n and T --π--> T' and |π| = o then o < n *)

Lemma typ_size_gt_offset : forall T T' C LLC π o n,
  ll_typdefctx_ok C LLC ->
  typ_size (typvar_sizes LLC) T n ->
  follow_typ C T π T' ->
  tr_ll_accesses C LLC π o ->
    π <> nil ->
  o < n.
Proof using.
  introv Hok Hn HF Hπ Hneq. gen n T T'. induction Hπ; intros.
  { false. }
  { inverts HF. subst. induction H.
    { inverts Hn. asserts: (i < k).
      { admit. (* TODO: This should be added in follow_typ. *) }
      forwards*: functional_typ_size T n n1. subst.
      tests: (πs = nil).
      { inverts Hπ.
        admit. (* This is maths. *) }
      { inverts_head typing_array. forwards*: IHHπ n1 T1 T'0.
        admit. (* This is maths. *) } }
    { applys~ IHtyping_array.
      { introv HN. inverts HN. }
      { admit. (* TODO: This should come from ll_typdefctx_ok.
        typ_size (typvar_sizes LLC) C[Tv] (typvar_sizes LLC)[Tv]. *) }
      { inverts_head typing_array. auto. } } }
  { inverts HF.
    forwards* HTfseq: functional_typing_struct C (typ_var Tv) Tfs Tfs0.
    subst. inverts Hn. admit.
    (* TODO: (fields_offsets LLC)[Tv][f] + o <= (typvar_sizes LLC)[Tv]*)  }
Qed.

End LowLevelLemmas.
