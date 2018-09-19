(**

This file describes transformations of the layout of records and arrays on
the memory.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)


Set Implicit Arguments.
Require Export Typing.


(* ---------------------------------------------------------------------- *)
(** Some previous necessary definitions *)

(** Conversion from lists to maps. *)

Axiom list_to_map : forall A B, list A -> list B -> map A B.

Axiom list_to_map_spec : forall ks vs m,
  m = list_to_map ks vs ->
  length ks = length vs ->
  (forall i,
    index ks i ->
    ks[i] \indom m /\ m[ks[i]] = vs[i]). (* reciprocal *)

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
        /\  CS[Tv] = fold_left Z.add 0 FS
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

(** Relationship between size of types and the translation of values. *)

Lemma typ_size_length_lw : forall C α v LLC T lw n,
  ll_typdefctx_ok C LLC ->
  tr_ll_val C LLC α T v lw ->
  typ_size (typvar_sizes LLC) T n ->
  length lw = n.
Proof using.
  (* TODO : Re-do this proof.

  introv HLLC Htr Hn. gen n. induction Htr; intros;
  try solve [ inverts Hn; rewrite~ length_one ].
  { (* double *)
    inverts Hn. rewrite length_cons. rewrite~ length_one. }
  { (* array *)
    gen n k a. induction a'; intros.
    { rewrite length_nil in *. subst. inverts Hn. math. }
    { rewrite concat_cons. rewrite length_app.
      rewrite length_cons in *. asserts Hl: (length a' = k - 1).
      { math. }
      inverts Hn. forwards~ Hcl: IHa' (n0*(k-1)) (k-1) (drop 1 a0).
      { constructors~. }
      { rewrite~ length_drop_nonneg.
        { rewrite~ H. }
        { math. } }
      { introv Hi. forwards~ Hr: H1 (i+1).
        { rewrite index_eq_index_length in *.
          rewrite~ length_drop_nonneg in Hi.
          { rewrite int_index_eq in *. math. }
          { math. } }
        asserts Hge1: (length a0 >= 1).
        { rewrite H. rewrite <- H0. math. }
        destruct a0; try solve [ rewrite length_nil in *; math ].
        asserts H0p1: (1 = 0 + 1). { math. }
        rewrite H0p1.
        rewrites* drop_succ; try math.
        rewrite drop_zero.
        asserts: (i >= 0).
        { rewrite index_eq_index_length in *.
          rewrite int_index_eq in *. math. }
        repeat rewrite read_cons_case in Hr.
        case_if; try math.
        asserts Hipm1: (i + 1 - 1 = i). { math. }
        rewrite~ Hipm1 in Hr. }
      { introv Hi Hn. forwards~ Hl': H2 (i+1) n.
        { rewrite index_eq_index_length in *.
          rewrite int_index_eq in *.
          rewrite length_drop_nonneg in *; math. }
        asserts: (i >= 0).
        { rewrite index_eq_index_length in *.
          rewrite int_index_eq in *. math. }
        rewrite read_cons_case in Hl'. case_if; try math.
        asserts Hipm1: (i + 1 - 1 = i). { math. }
        rewrite~ Hipm1 in Hl'. }
      rewrite Hcl. forwards~: H2 0 n0.
      { rewrite index_eq_index_length. rewrite int_index_eq. math. }
      rewrite read_zero in *. rewrite~ H3.
      rewrite Z.mul_sub_distr_l. math. } }
    { (* struct *) 
      inverts Hn. inverts HLLC as HD HTv HD' HC. simpls.
      forwards* (FT&FS&FO&HFT&HFS&HFO&HCSTv&HCFOff): HC.
      rewrite HCSTv at 1. subst. gen C FS FO CS CFOff CFOrd. 
      induction s'; intros.
      { rewrite length_nil in *. symmetry in H5.
        rewrite length_zero_eq_eq_nil in H5. 
        rewrite H5 in HFS.
        asserts Hm: (List.map (fun f : field => Tfs[f]) nil = nil).
        { unfolds~. }
        rewrite Hm in HFS. inverts HFS. unfolds~. }
      { rewrite concat_cons. rewrite length_app.
        forwards~: H7 0 FS[0].
        { rewrite index_eq_index_length in *.
          rewrite int_index_eq in *.
          rewrite length_cons. math. }
        { destruct (CFOrd[Tv]); try solve [ 
          rewrite length_nil in *; rewrite length_cons in *; math ].
          rewrite List.map_cons in *.
          rewrite <- app_cons_one_r in HFS.
          rewrite <- List_app_eq in HFS.
          forwards* (l1'&l2'&Hl1'&Hl2'&HFSeq): List.Forall2_app_inv_l HFS.
          rewrite read_zero.
          inverts Hl1'. inverts H10.
          rewrite HFSeq.
          rewrite List_app_eq. rew_list.
          rewrite~ read_zero. }
        rewrite read_zero in H. rewrite H.
        asserts Hlc: (length (concat s') = fold_left Z.add 0 (drop 1 FS)).
        { admit. (* TODO: Create new contexts etc to make the numbers work. *) }
        rewrite Hlc. admit. (* TODO: Lemma about fold and drop. *) } }*)
Admitted.

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
  { inverts HF.
    forwards* (HTeq&Hoseq): functional_typing_array C T T' T1 os.
    subst. induction H.
    { inverts Hn. asserts: (i < k).
      { admit. (* TODO: This should be added in follow_typ. *) }
      forwards*: functional_typ_size T n n1. subst.
      tests: (πs = nil).
      { inverts Hπ.
        admit. (* This is maths. *) }
      { forwards*: IHHπ n1 T T'0.
        admit. (* This is maths. *) } }
    { applys~ IHtyping_array. inverts Hn.
      { introv HN. inverts HN. }
      admit. (* TODO: this should be true: 
      typ_size (typvar_sizes LLC) C[Tv] (typvar_sizes LLC)[Tv] *) } }
  { inverts HF.
    forwards* HTfseq: functional_typing_struct C (typ_var Tv) Tfs Tfs0.
    subst. inverts Hn. admit.
    (* TODO: (fields_offsets LLC)[Tv][f] + o <= (typvar_sizes LLC)[Tv]*)  }
Qed.

End LowLevelLemmas.
