(**

This file describes type inference rules of the language described in
Language.v. There are also some results connecting typing and 
well-foundedness.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export TLCbuffer Wellformedness.

Open Scope Z_scope.

(* ********************************************************************** *)
(* * Typing *)

(* Computing the size of a type. Assuming the size of type variables
   are known. Used to transform array accesses. *)

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
  | typ_size_var : forall Tv,
      Tv \indom CS ->
      typ_size CS (typ_var Tv) CS[Tv].


(* ---------------------------------------------------------------------- *)
(** Basic, or comparable, types *)

Inductive basic_typ (C:typdefctx) : typ -> Type :=
  | basic_typ_int :
      basic_typ C typ_int
  | basic_typ_double :
      basic_typ C typ_double
  | basic_typ_bool :
      basic_typ C typ_bool
  | basic_typ_ptr : forall T,
      basic_typ C (typ_ptr T)
  | basic_typ_var : forall Tv,
      basic_typ C C[Tv] ->
      basic_typ C (typ_var Tv).


(* ---------------------------------------------------------------------- *)
(** Typing of arrays and structs *)

Inductive typing_array (C:typdefctx) : typ -> typ -> option size -> Prop :=
  | typing_array_base : forall T os,
      wf_typ C T ->
      typing_array C (typ_array T os) T os
  | typing_array_typvar : forall Tv T os,
      Tv \indom C ->
      typing_array C C[Tv] T os ->
      typing_array C (typ_var Tv) T os.

Inductive typing_struct (C:typdefctx) : typ -> map field typ -> Prop :=
  | typing_struct_base : forall Tfs,
      typing_struct C (typ_struct Tfs) Tfs
  | typing_struct_typvar : forall Tv Tfs,
      Tv \indom C ->
      typing_struct C C[Tv] Tfs ->
      typing_struct C (typ_var Tv) Tfs.


(* ---------------------------------------------------------------------- *)
(** Typing of access paths *)

(** T[π] = T1 *)

Inductive follow_typ (C:typdefctx) : typ -> accesses -> typ -> Prop :=
  | follow_typ_nil : forall T,
      follow_typ C T nil T
  | follow_typ_array : forall T os Ta i π Tr,
      typing_array C Ta T os ->
      follow_typ C T π Tr ->
      follow_typ C Ta ((access_array Ta i)::π) Tr
  | follow_typ_struct : forall Ts Tfs f π Tr,
      typing_struct C Ts Tfs ->
      f \indom Tfs ->
      follow_typ C Tfs[f] π Tr ->
      follow_typ C Ts ((access_field Ts f)::π) Tr.

(** φ(l)..π = T *)

Inductive read_phi (C:typdefctx) (φ:phi) (l:loc) (π:accesses) (T:typ) : Prop :=
  | read_phi_intro :
      l \indom φ ->
      follow_typ C φ[l] π T -> 
      read_phi C φ l π T.


(* ---------------------------------------------------------------------- *)
(** Type-directed transformation to low-level *)

Inductive prefix_sum : list int -> list int -> int -> Prop :=
  | prefix_sum_nil : forall acc,
      prefix_sum nil nil acc
  | prefix_sum_cons : forall l l' acc i,
      prefix_sum l l' (i + acc) ->
      prefix_sum (i::l) (acc::l') acc.

Lemma prefix_sum_example :
  prefix_sum (1::2::3::4::5::6::nil) (0::1::3::6::10::15::nil) 0.
Proof.
  repeat constructors.
Qed.

Lemma prefix_sum_length : forall l l' n,
  prefix_sum l l' n ->
  length l = length l'.
Proof.
  introv Hps. induction Hps.
  { rewrite~ length_nil. }
  { repeat rewrite length_cons. rewrite~ IHHps. }
Qed.

Lemma prefix_sum_spec : forall l l' n,
  prefix_sum l l' n ->
  (forall i,
    index l i ->
    l'[i] = fold_left Z.add n (take i l)).
Proof.
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

Axiom list_to_map : forall A B, list A -> list B -> map A B.

Axiom list_to_map_spec : forall ks vs m,
  m = list_to_map ks vs ->
      length ks = length vs
  /\  forall i,
        index ks i ->
        ks[i] \indom m /\ m[ks[i]] = vs[i].

Inductive ll_typdefctx_ok (C:typdefctx) (LLC:ll_typdefctx) : Prop :=
  | low_level_ctx_ok_intros : forall CS CFOrd CFOff,
      LLC = make_ll_typdefctx CS CFOff CFOrd ->
      (* Same fields in C and LLC. *)
      (forall Tv Tfs,
        Tv \indom C ->
        C[Tv] = typ_struct Tfs ->
            dom Tfs = dom CFOff[Tv]
        /\  dom Tfs = to_set CFOrd[Tv]) ->
      (* Coherency between the sizes. *)
      (forall Tv Tfs,
        Tv \indom C ->
        C[Tv] = typ_struct Tfs ->
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

(* Given a list of accesses, computes the offset. Used to translate
   pointer values. *)

Inductive tr_ll_accesses (C:typdefctx) (LLC:ll_typdefctx) : accesses -> offset -> Prop :=
  | tr_ll_accesses_nil :
      tr_ll_accesses C LLC nil 0%Z
  | tr_ll_accesses_access_array : forall T T' os πs i n o,
      typing_array C T T' os ->
      typ_size (typvar_sizes LLC) T' n ->
      tr_ll_accesses C LLC πs o ->
      (0 <= i)%Z ->
      (0 <= n)%Z ->
      tr_ll_accesses C LLC ((access_array T i)::πs) ((i * n) + o)
  | tr_ll_accesses_access_field : forall Tfs FO πs Tv f o,
      typing_struct C (typ_var Tv) Tfs ->
      FO = fields_offsets LLC ->
      Tv \indom FO ->
      f \indom FO[Tv] ->
      tr_ll_accesses C LLC πs o ->
      (0 <= FO[Tv][f])%Z ->
      tr_ll_accesses C LLC ((access_field (typ_var Tv) f)::πs) (FO[Tv][f] + o).

(* Relates values with a list of words. This is how the memory is transformed. *)

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
      List.Forall2 (tr_ll_val C LLC α T) a a' ->
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
(** Typing of values *)

Inductive typing_val (C:typdefctx) (LLC:ll_typdefctx) (φ:phi) : val -> typ -> Prop :=
  | typing_val_uninitialized : forall T,
      wf_typ C T ->
      typing_val C LLC φ val_uninitialized T
  | typing_val_unit :
      typing_val C LLC φ val_unit typ_unit
  | typing_val_bool : forall b,
      typing_val C LLC φ (val_bool b) typ_bool
  | typing_val_int : forall i,
      typing_val C LLC φ (val_int i) typ_int
  | typing_val_double : forall d,
      typing_val C LLC φ (val_double d) typ_double
  | typing_val_abstract_ptr : forall l π T,
      read_phi C φ l π T ->
      typing_val C LLC φ (val_abstract_ptr l π) (typ_ptr T)
  | typing_val_struct : forall Ts vfs Tfs,
      typing_struct C Ts Tfs ->
      dom Tfs = dom vfs ->
      (forall f,
        f \indom Tfs ->
        f \indom vfs ->
        typing_val C LLC φ vfs[f] Tfs[f]) ->
      typing_val C LLC φ (val_struct Ts vfs) Ts
  | typing_val_array : forall Ta a T os,
      typing_array C Ta T os ->
      (forall n,
        os = Some n ->
        length a = n) ->
      (forall i, 
        index a i -> 
        typing_val C LLC φ a[i] T) ->
      typing_val C LLC φ (val_array Ta a) Ta.


(* ---------------------------------------------------------------------- *)
(** Typing of terms *)

Inductive typing (C:typdefctx) (LLC:ll_typdefctx) (φ:phi) : gamma -> trm -> typ -> Prop :=
  (* Closed values *)
  | typing_trm_val : forall Γ v T,
      typing_val C LLC φ v T ->
      typing C LLC φ Γ (trm_val v) T
  (* Variables *)
  | typing_var : forall Γ x T,
      Ctx.lookup x Γ = Some T ->
      typing C LLC φ Γ x T
  (* Binary operations *)
  | typing_binop_add : forall Γ t1 t2,
      typing C LLC φ Γ t1 typ_int ->
      typing C LLC φ Γ t2 typ_int ->
      typing C LLC φ Γ (trm_app binop_add (t1::t2::nil)) typ_int
  | typing_binop_sub : forall Γ t1 t2,
      typing C LLC φ Γ t1 typ_int ->
      typing C LLC φ Γ t2 typ_int ->
      typing C LLC φ Γ (trm_app binop_sub (t1::t2::nil)) typ_int
  | typing_binop_mul : forall Γ t1 t2,
      typing C LLC φ Γ t1 typ_int ->
      typing C LLC φ Γ t2 typ_int ->
      typing C LLC φ Γ (trm_app binop_mul (t1::t2::nil)) typ_int
  | typing_binop_div : forall Γ t1 t2,
      typing C LLC φ Γ t1 typ_int ->
      typing C LLC φ Γ t2 typ_int ->
      typing C LLC φ Γ (trm_app binop_div (t1::t2::nil)) typ_int
  | typing_binop_mod : forall Γ t1 t2,
      typing C LLC φ Γ t1 typ_int ->
      typing C LLC φ Γ t2 typ_int ->
      typing C LLC φ Γ (trm_app binop_mod (t1::t2::nil)) typ_int
  | typing_binop_eq : forall Γ T t1 t2,
      wf_typ C T ->
      basic_typ C T ->
      typing C LLC φ Γ t1 T ->
      typing C LLC φ Γ t2 T ->
      typing C LLC φ Γ (trm_app binop_eq (t1::t2::nil)) typ_bool
  (* Abstract heap operations *)
  | typing_get : forall Γ T t1,
      typing C LLC φ Γ t1 (typ_ptr T) ->
      typing C LLC φ Γ (trm_app (prim_get T) (t1::nil)) T
  | typing_set : forall Γ T t1 t2,
      typing C LLC φ Γ t1 (typ_ptr T) ->
      typing C LLC φ Γ t2 T ->
      typing C LLC φ Γ (trm_app (prim_set T) (t1::t2::nil)) typ_unit
  | typing_new : forall Γ T, 
      typing C LLC φ Γ (trm_app (prim_new T) nil) (typ_ptr T)
  | typing_new_array : forall Γ t1 T,
      typing C LLC φ Γ t1 typ_int ->
      typing C LLC φ Γ (trm_app (prim_new_array T) (t1::nil)) (typ_ptr (typ_array T None))
  | typing_struct_access : forall Γ Ts t1 Tfs f,
      typing_struct C Ts Tfs ->
      f \indom Tfs ->
      typing C LLC φ Γ t1 (typ_ptr Ts) ->
      typing C LLC φ Γ (trm_app (prim_struct_access Ts f) (t1::nil)) (typ_ptr Tfs[f])
  | typing_array_access : forall Γ os Ta t1 t2 T,
      typing_array C Ta T os ->
      typing C LLC φ Γ t1 (typ_ptr Ta) ->
      typing C LLC φ Γ t2 typ_int ->
      typing C LLC φ Γ (trm_app (prim_array_access Ta) (t1::t2::nil)) (typ_ptr T)
  (* Operations on structs and arrays as values *)
  | typing_struct_get : forall Γ Ts t1 Tfs f,
      typing_struct C Ts Tfs ->
      f \indom Tfs ->
      typing C LLC φ Γ t1 (typ_struct Tfs) ->
      typing C LLC φ Γ (trm_app (prim_struct_get Ts f) (t1::nil)) Tfs[f]
  | typing_array_get : forall Γ os Ta t1 t2 T,
      typing_array C Ta T os ->
      typing C LLC φ Γ t1 (typ_array T os) ->
      typing C LLC φ Γ t2 typ_int ->
      typing C LLC φ Γ (trm_app (prim_array_get Ta) (t1::t2::nil)) T
  (* Other language constructs *)
  | typing_if : forall Γ t0 t1 t2 T,
      typing C LLC φ Γ t0 typ_bool ->
      typing C LLC φ Γ t1 T ->
      typing C LLC φ Γ t2 T ->
      typing C LLC φ Γ (trm_if t0 t1 t2) T
  | typing_let : forall Γ T1 T z t1 t2,
      typing C LLC φ Γ t1 T1 ->
      typing C LLC φ (Ctx.add z T1 Γ) t2 T ->
      typing C LLC φ Γ (trm_let z t1 t2) T.


(* ---------------------------------------------------------------------- *)
(** Typing of the state and the stack *)

Definition state_typing (C:typdefctx) (LLC:ll_typdefctx) (φ:phi) (m:state) : Prop :=
      dom m = dom φ
  /\  (forall l, 
        l \indom m -> 
        typing_val C LLC φ m[l] φ[l]).

Definition stack_typing (C:typdefctx) (LLC:ll_typdefctx) (φ:phi) (Γ:gamma) (S:stack) : Prop := 
  forall x v T,
    Ctx.lookup x S = Some v ->
    Ctx.lookup x Γ = Some T ->
    typing_val C LLC φ v T.


(* ---------------------------------------------------------------------- *)
(** Functional predicates *)

(* Inferring array types is functional *)
Lemma functional_typing_array : forall C Ta T1 T2 k1 k2,
  typing_array C Ta T1 k1 ->
  typing_array C Ta T2 k2 ->
  T1 = T2 /\ k1 = k2.
Proof.
  introv HTa1 HTa2. induction HTa1; inverts* HTa2.
Qed.

(* Inferring struct types is functional *)
Lemma functional_typing_struct : forall C Ts Tfs1 Tfs2,
  typing_struct C Ts Tfs1 ->
  typing_struct C Ts Tfs2 ->
  Tfs1 = Tfs2.
Proof.
  introv HTs1 HTs2. induction HTs1; inverts* HTs2.
Qed.

(* Following accesses in a type is functional *)
Lemma functional_follow_typ : forall C T π T1 T2,
  follow_typ C T π T1 ->
  follow_typ C T π T2 ->
  T1 = T2.
Proof.
  introv HF1 HF2. induction HF1.
  { inverts* HF2. }
  { inverts HF2 as HTa. applys* IHHF1. 
    forwards* (HT&Hos): functional_typing_array H HTa. subst~. }
  { inverts HF2 as HTs. applys* IHHF1. 
    forwards* HTfs: functional_typing_struct H HTs. subst~. }
Qed.

(* Reading from φ is functional *)
Lemma functional_read_phi : forall C φ l π T1 T2,
  read_phi C φ l π T1 ->
  read_phi C φ l π T2 ->
  T1 = T2.
Proof.
  introv HR1 HR2. inverts HR1. inverts HR2.
  applys* functional_follow_typ.
Qed.


(* ---------------------------------------------------------------------- *)
(* Lemmas about the connection of well-foundedness and typing. *)

Lemma wf_typing_array : forall T os C Ta,
  typing_array C Ta T os ->
  wf_typ C Ta ->
  wf_typ C T.
Proof.
  introv HTa HT. induction HTa; inverts~ HT.
Qed.

Lemma wf_typing_struct : forall Tfs C Ts,
  typing_struct C Ts Tfs ->
  wf_typ C Ts ->
  (forall f, 
    f \indom Tfs ->
    wf_typ C Tfs[f]).
Proof.
  introv HTs HT. induction HTs; inverts~ HT.
Qed.

Lemma wf_typing_array_inv : forall T os C Ta,
  typing_array C Ta T os ->
  wf_typ C T ->
  wf_typ C Ta.
Proof.
  introv HTa HT. induction HTa; constructors~.
Qed.

Lemma wf_typing_struct_inv : forall Tfs C Ts,
  typing_struct C Ts Tfs ->
  (forall f, 
    f \indom Tfs ->
    wf_typ C Tfs[f]) ->
  wf_typ C Ts.
Proof.
  introv HTs HT. induction HTs; constructors~.
Qed.

Lemma follow_typ_wf_accesses : forall T T' C π,
  wf_typ C T ->
  follow_typ C T π T' ->
  wf_accesses C π .
Proof.
  introv HT HF. induction HF; constructors~.
  { applys IHHF. applys* wf_typing_array. }
  { applys IHHF. applys* wf_typing_struct. }
Qed.

Lemma follow_typ_wf_typ : forall T T' C π,
  wf_typ C T ->
  follow_typ C T π T' ->
  wf_typ C T'.
Proof.
  introv HT HF. forwards* Hva: follow_typ_wf_accesses. induction HF.
  { auto. }
  { inverts Hva. applys~ IHHF. applys* wf_typing_array. }
  { inverts Hva. applys~ IHHF. applys* wf_typing_struct. }
Qed.

Lemma wf_stack_add : forall C x v S,
  wf_stack C S ->
  wf_val C v ->
  wf_stack C (Ctx.add x v S).
Proof.
  introv HS Hv. unfolds Ctx.add. destruct~ x.
  unfolds. introv HCl. unfold Ctx.lookup in HCl.
  case_if in HCl.
  { inverts~ HCl. }
  { applys~ HS x. }
Qed.
