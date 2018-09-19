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

(** T'..π = T *)

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
(** Typing of values *)

Inductive typing_val (C:typdefctx) (φ:phi) : val -> typ -> Prop :=
  | typing_val_uninitialized : forall T,
      wf_typ C T ->
      typing_val C φ val_uninitialized T
  | typing_val_unit :
      typing_val C φ val_unit typ_unit
  | typing_val_bool : forall b,
      typing_val C φ (val_bool b) typ_bool
  | typing_val_int : forall i,
      typing_val C φ (val_int i) typ_int
  | typing_val_double : forall d,
      typing_val C φ (val_double d) typ_double
  | typing_val_abstract_ptr : forall l π T,
      read_phi C φ l π T ->
      typing_val C φ (val_abstract_ptr l π) (typ_ptr T)
  | typing_val_struct : forall Ts vfs Tfs,
      typing_struct C Ts Tfs ->
      dom Tfs = dom vfs ->
      (forall f,
        f \indom Tfs ->
        f \indom vfs ->
        typing_val C φ vfs[f] Tfs[f]) ->
      typing_val C φ (val_struct Ts vfs) Ts
  | typing_val_array : forall Ta a T os,
      typing_array C Ta T os ->
      (forall n,
        os = Some n ->
        length a = n) ->
      (forall i, 
        index a i -> 
        typing_val C φ a[i] T) ->
      typing_val C φ (val_array Ta a) Ta.


(* ---------------------------------------------------------------------- *)
(** Typing of terms *)

Inductive typing (C:typdefctx) (φ:phi) : gamma -> trm -> typ -> Prop :=
  (* Closed values *)
  | typing_trm_val : forall Γ v T,
      typing_val C φ v T ->
      typing C φ Γ (trm_val v) T
  (* Variables *)
  | typing_var : forall Γ x T,
      Ctx.lookup x Γ = Some T ->
      typing C φ Γ x T
  (* Binary operations *)
  | typing_binop_add : forall Γ t1 t2,
      typing C φ Γ t1 typ_int ->
      typing C φ Γ t2 typ_int ->
      typing C φ Γ (trm_app binop_add (t1::t2::nil)) typ_int
  | typing_binop_sub : forall Γ t1 t2,
      typing C φ Γ t1 typ_int ->
      typing C φ Γ t2 typ_int ->
      typing C φ Γ (trm_app binop_sub (t1::t2::nil)) typ_int
  | typing_binop_mul : forall Γ t1 t2,
      typing C φ Γ t1 typ_int ->
      typing C φ Γ t2 typ_int ->
      typing C φ Γ (trm_app binop_mul (t1::t2::nil)) typ_int
  | typing_binop_div : forall Γ t1 t2,
      typing C φ Γ t1 typ_int ->
      typing C φ Γ t2 typ_int ->
      typing C φ Γ (trm_app binop_div (t1::t2::nil)) typ_int
  | typing_binop_mod : forall Γ t1 t2,
      typing C φ Γ t1 typ_int ->
      typing C φ Γ t2 typ_int ->
      typing C φ Γ (trm_app binop_mod (t1::t2::nil)) typ_int
  | typing_binop_eq : forall Γ T t1 t2,
      wf_typ C T ->
      basic_typ C T ->
      typing C φ Γ t1 T ->
      typing C φ Γ t2 T ->
      typing C φ Γ (trm_app binop_eq (t1::t2::nil)) typ_bool
  (* Abstract heap operations *)
  | typing_get : forall Γ T t1,
      typing C φ Γ t1 (typ_ptr T) ->
      typing C φ Γ (trm_app (prim_get T) (t1::nil)) T
  | typing_set : forall Γ T t1 t2,
      typing C φ Γ t1 (typ_ptr T) ->
      typing C φ Γ t2 T ->
      typing C φ Γ (trm_app (prim_set T) (t1::t2::nil)) typ_unit
  | typing_new : forall Γ T, 
      typing C φ Γ (trm_app (prim_new T) nil) (typ_ptr T)
  | typing_new_array : forall Γ t1 T,
      typing C φ Γ t1 typ_int ->
      typing C φ Γ (trm_app (prim_new_array T) (t1::nil)) (typ_ptr (typ_array T None))
  | typing_struct_access : forall Γ Ts t1 Tfs f,
      typing_struct C Ts Tfs ->
      f \indom Tfs ->
      typing C φ Γ t1 (typ_ptr Ts) ->
      typing C φ Γ (trm_app (prim_struct_access Ts f) (t1::nil)) (typ_ptr Tfs[f])
  | typing_array_access : forall Γ os Ta t1 t2 T,
      typing_array C Ta T os ->
      typing C φ Γ t1 (typ_ptr Ta) ->
      typing C φ Γ t2 typ_int ->
      typing C φ Γ (trm_app (prim_array_access Ta) (t1::t2::nil)) (typ_ptr T)
  (* Operations on structs and arrays as values *)
  | typing_struct_get : forall Γ Ts t1 Tfs f,
      typing_struct C Ts Tfs ->
      f \indom Tfs ->
      typing C φ Γ t1 (typ_struct Tfs) ->
      typing C φ Γ (trm_app (prim_struct_get Ts f) (t1::nil)) Tfs[f]
  | typing_array_get : forall Γ os Ta t1 t2 T,
      typing_array C Ta T os ->
      typing C φ Γ t1 (typ_array T os) ->
      typing C φ Γ t2 typ_int ->
      typing C φ Γ (trm_app (prim_array_get Ta) (t1::t2::nil)) T
  (* Other language constructs *)
  | typing_if : forall Γ t0 t1 t2 T,
      typing C φ Γ t0 typ_bool ->
      typing C φ Γ t1 T ->
      typing C φ Γ t2 T ->
      typing C φ Γ (trm_if t0 t1 t2) T
  | typing_let : forall Γ T1 T z t1 t2,
      typing C φ Γ t1 T1 ->
      typing C φ (Ctx.add z T1 Γ) t2 T ->
      typing C φ Γ (trm_let z t1 t2) T.


(* ---------------------------------------------------------------------- *)
(** Typing of the state and the stack *)

Definition state_typing (C:typdefctx) (φ:phi) (m:state) : Prop :=
      dom m = dom φ
  /\  (forall l, 
        l \indom m -> 
        typing_val C φ m[l] φ[l]).

Definition stack_typing (C:typdefctx) (φ:phi) (Γ:gamma) (S:stack) : Prop := 
  forall x v T,
    Ctx.lookup x S = Some v ->
    Ctx.lookup x Γ = Some T ->
    typing_val C φ v T.


(* ---------------------------------------------------------------------- *)
(** Functional predicates *)

Section FunctionalLemmas.

(** The relation typ_size is a function. *)

Lemma functional_typ_size : forall CS T n1 n2,
  typ_size CS T n1 ->
  typ_size CS T n2 ->
  n1 = n2.
Proof using.
  introv Hn1 Hn2. gen n2. induction Hn1; intros;
  try solve [ inverts~ Hn2 ].
  { inverts Hn2 as Hn2. forwards~: IHHn1 Hn2. subst~. }
  { inverts Hn2. subst. asserts: (n = n0).
    { applys~ read_extens.
      { congruence. }
      { introv Hi. rewrite <- H in Hi. applys~ H1. } }
    subst~. }
Qed.

(** Inferring array types is functional. *)

Lemma functional_typing_array : forall C Ta T1 T2 k1 k2,
  typing_array C Ta T1 k1 ->
  typing_array C Ta T2 k2 ->
  T1 = T2 /\ k1 = k2.
Proof using.
  introv HTa1 HTa2. induction HTa1; inverts* HTa2.
Qed.

(** Inferring struct types is functional. *)

Lemma functional_typing_struct : forall C Ts Tfs1 Tfs2,
  typing_struct C Ts Tfs1 ->
  typing_struct C Ts Tfs2 ->
  Tfs1 = Tfs2.
Proof using.
  introv HTs1 HTs2. induction HTs1; inverts* HTs2.
Qed.

(** Following accesses in a type is functional. **)

Lemma functional_follow_typ : forall C T π T1 T2,
  follow_typ C T π T1 ->
  follow_typ C T π T2 ->
  T1 = T2.
Proof using.
  introv HF1 HF2. induction HF1.
  { inverts* HF2. }
  { inverts HF2 as HTa. applys* IHHF1. 
    forwards* (HT&Hos): functional_typing_array H HTa. subst~. }
  { inverts HF2 as HTs. applys* IHHF1. 
    forwards* HTfs: functional_typing_struct H HTs. subst~. }
Qed.

(** Reading from φ is functional. *)

Lemma functional_read_phi : forall C φ l π T1 T2,
  read_phi C φ l π T1 ->
  read_phi C φ l π T2 ->
  T1 = T2.
Proof using.
  introv HR1 HR2. inverts HR1. inverts HR2.
  applys* functional_follow_typ.
Qed.

End FunctionalLemmas.


(* ---------------------------------------------------------------------- *)
(** Lemmas about the connection of well-foundedness and typing, and other
    nice properties about the typing predicates. *)

Section WellformednessLemmas.

Hint Constructors wf_trm wf_val wf_accesses wf_typ.

(** Path surgery of well-formed accesses. *)

Lemma wf_accesses_app : forall C π1 π2,
  wf_accesses C π1 ->
  wf_accesses C π2 ->
  wf_accesses C (π1 ++ π2).
Proof using.
  introv Ha1 Ha2. gen π2. induction Ha1; intros;
  rew_list in *; eauto; constructors~.
Qed.

(** Unfolding array types preserves well-formedness. *)

Lemma wf_typing_array : forall T os C Ta,
  typing_array C Ta T os ->
  wf_typ C Ta ->
  wf_typ C T.
Proof using.
  introv HTa HT. induction HTa; inverts~ HT.
Qed.

(** Unfolding struct types preserves well-formedness. *)

Lemma wf_typing_struct : forall Tfs C Ts,
  typing_struct C Ts Tfs ->
  wf_typ C Ts ->
  (forall f,
    f \indom Tfs ->
    wf_typ C Tfs[f]).
Proof using.
  introv HTs HT. induction HTs; inverts~ HT.
Qed.

(** Folding array types preserves well-formedness. *)

Lemma wf_typing_array_inv : forall T os C Ta,
  typing_array C Ta T os ->
  wf_typ C T ->
  wf_typ C Ta.
Proof using.
  introv HTa HT. induction HTa; constructors~.
Qed.

(** Folding struct types preserves well-formedness. *)

Lemma wf_typing_struct_inv : forall Tfs C Ts,
  typing_struct C Ts Tfs ->
  (forall f,
    f \indom Tfs ->
    wf_typ C Tfs[f]) ->
  wf_typ C Ts.
Proof using.
  introv HTs HT. induction HTs; constructors~.
Qed.

(** Only well-formed accesses can be followed. *)

Lemma follow_typ_wf_accesses : forall T T' C π,
  wf_typ C T ->
  follow_typ C T π T' ->
  wf_accesses C π .
Proof using.
  introv HT HF. induction HF; constructors~.
  { applys IHHF. applys* wf_typing_array. }
  { applys IHHF. applys* wf_typing_struct. }
Qed.

(** Only well-formed types can be reached following accesses. *)

Lemma follow_typ_wf_typ : forall T T' C π,
  wf_typ C T ->
  follow_typ C T π T' ->
  wf_typ C T'.
Proof using.
  introv HT HF. forwards* Hva: follow_typ_wf_accesses HT HF.
  induction HF.
  { auto. }
  { inverts Hva. applys~ IHHF. applys* wf_typing_array. }
  { inverts Hva. applys~ IHHF. applys* wf_typing_struct. }
Qed.

(** Extending a well-formed stack. *)

Lemma wf_stack_add : forall C x v S,
  wf_stack C S ->
  wf_val C v ->
  wf_stack C (Ctx.add x v S).
Proof using.
  introv HS Hv. unfolds Ctx.add. destruct~ x.
  unfolds. introv HCl. unfold Ctx.lookup in HCl.
  case_if in HCl.
  { inverts~ HCl. }
  { applys~ HS x. }
Qed.

(** Well-formed types are not cyclic. *)

Lemma wf_typ_array_neq : forall C T os,
  wf_typ C T ->
  T <> typ_array T os.
Proof using.
  introv Hwf HN. gen os. induction Hwf; intros;
  try solve [ inverts HN ].
  { inverts HN. applys* IHHwf. }
Qed.

(** In a well-formed type definitions context, the type defined by
    a type variable doesn't contain that type variable. *)

Lemma wf_typdefctx_array_neq : forall C Tv os,
  wf_typdefctx C ->
  Tv \indom C ->
  C[Tv] <> typ_array (typ_var Tv) os.
Proof using.
  introv Hwf HTvin HN. unfolds wf_typdefctx.
  applys* Hwf. rewrite HN. repeat constructors~.
Qed.

(** If an array type unfolds to (Tv, os) then Tv is free in
    teh type. *)

Lemma wf_typvar_array_free : forall C Tv T os,
  wf_typdefctx C ->
  wf_typ C T ->
  Tv \indom C ->
  typing_array C T (typ_var Tv) os ->
  free_typvar C Tv T.
Proof using.
  introv HwfC HwfTv HTvin HTa. gen Tv os. induction HwfTv; intros;
  try solve [ inverts HTa ].
  { constructors. inverts HTa. constructors~. }
  { tests: (Tv=Tv0); constructors~. inverts HTa.
    applys* IHHwfTv. }
Qed.

(** A well-formed T does not unfold to (T, os). *)

Lemma wf_typ_array_not_rec : forall C T os,
  wf_typdefctx C ->
  wf_typ C T ->
  ~ typing_array C T T os.
Proof using.
  introv HwfC HwfT HN. gen os. induction HwfT; intros;
  try solve [ inverts HN ].
  { inverts HN as Hwfa Heq. inverts Hwfa.
    forwards*: wf_typ_array_neq Heq. }
  { inverts HN as HTvin HTa. unfolds wf_typdefctx.
    applys* HwfC. applys* wf_typvar_array_free. }
Qed.

(** Extending the accesses to an array type. *)

Lemma follow_typ_array_extended_access : forall C T Ta os i π T',
  typing_array C Ta T' os ->
  follow_typ C T π Ta ->
  follow_typ C T (π ++ (access_array Ta i::nil)) T'.
Proof using.
  introv HTa HF. gen i. induction HF; intros;
  try solve [ rew_list ; repeat constructors* ].
Qed.

(** Extending the accesses to a struct type. *)

Lemma follow_typ_struct_extended_access : forall C T Ts Tfs f π,
  typing_struct C Ts Tfs ->
  f \indom Tfs ->
  follow_typ C T π Ts ->
  follow_typ C T (π ++ (access_field Ts f::nil)) Tfs[f].
Proof using.
  introv HTs Hfin HF. gen f. induction HF; intros;
  try solve [ rew_list ; repeat constructors* ].
Qed.

(** If following some accesses a certain type variable can be reached, then
    this type variable is free. *)

Lemma follow_typ_typvar_free : forall π C Tv T,
  wf_typdefctx C ->
  wf_typ C T ->
  follow_typ C T π (typ_var Tv) ->
  free_typvar C Tv T.
Proof using.
  introv HwfC HwfT HF. gen π Tv. induction HwfT; intros;
  try solve [ inverts HF;
  try inverts_head typing_array; try inverts_head typing_struct ].
  { constructors. inverts HF as.
    { introv HTa HF. inverts HTa. applys* IHHwfT. }
    { introv HTs Hfin HF. inverts HTs. } }
  { constructors. inverts HF as.
    { introv HTa HF. inverts HTa. }
    { introv HTs Hfin HF. inverts HTs. exists* f. } }
  { tests: (Tv=Tv0).
    { constructors~. }
    { inverts HF as; tryfalse.
      { introv HTa HF. constructors~. inverts HTa.
        applys~ IHHwfT (access_array C[Tv] 0::π0).
        constructors*. }
      { introv HTs Hfin HF. constructors~. inverts HTs.
        applys~ IHHwfT (access_field C[Tv] f::π0).
        constructors*. } } }
Qed.

(** Array type unfolding preserves free variables. *)

Lemma typing_array_keeps_free_var : forall T os C Tv Ta,
  typing_array C Ta T os ->
  free_typvar C Tv T ->
  free_typvar C Tv Ta.
Proof using.
  introv HTa HT. gen Tv. induction HTa; intros.
  { constructors~. }
  { tests: (Tv=Tv0); constructors~. }
Qed.

(** Struct type unfolding preserves free variables. *)

Lemma typing_struct_keeps_free_var : forall Ts C Tv Tfs f,
  typing_struct C Ts Tfs ->
  f \indom Tfs ->
  free_typvar C Tv Tfs[f] ->
  free_typvar C Tv Ts.
Proof using.
  introv HTs Hfin HT. gen Tv. induction HTs; intros.
  { constructors. exists~ f. }
  { tests: (Tv=Tv0); constructors~. }
Qed.

(** If T..π = T then π = nil. *)

Lemma wf_typ_follow_accesses : forall C T π,
  wf_typdefctx C ->
  wf_typ C T ->
  wf_accesses C π ->
  follow_typ C T π T ->
    π = nil.
Proof using.
  introv HwfC HwfT Hwfπ Hπ. gen π. induction HwfT; intros;
  try solve [ inverts~ Hπ; inverts H ].
  { inverts~ Hπ.
    { false. inverts H. 
      asserts HTa: (typing_array C (typ_array T0 os0) T0 os0).
      { constructors*. } 
      forwards* HN: follow_typ_array_extended_access i HTa H0.
      asserts Hwfapp: (wf_accesses C (π0 & access_array (typ_array T0 os0) i)).
      { inverts Hwfπ. applys~ wf_accesses_app. }
      apply IHHwfT in HN. applys~ last_eq_nil_inv HN. auto. }
    { inverts Hwfπ. inverts_head typing_struct. } }
  { inverts~ Hπ.
    { inverts Hwfπ. inverts_head typing_array. }
    { false. inverts H1.
      asserts HTs: (typing_struct C (typ_struct Tfs0) Tfs0).
      { constructors*. }
      forwards* HN: follow_typ_struct_extended_access HTs H2 H3.
      asserts Hwfapp: (wf_accesses C (π0 & access_field (typ_struct Tfs0) f)).
      { inverts Hwfπ. applys~ wf_accesses_app. }
      apply H0 in HN. applys~ last_eq_nil_inv HN. auto. auto. } }
  { inverts~ Hπ.
    { false. inverts H0.
      forwards~: wf_typing_array H4 HwfT.
      forwards*: follow_typ_typvar_free H1.
      unfolds wf_typdefctx. applys* HwfC.
      applys* typing_array_keeps_free_var. }
    { false. inverts H0.
      forwards~: wf_typing_struct H5 HwfT f.
      forwards*: follow_typ_typvar_free H2.
      unfolds wf_typdefctx. applys* HwfC.
      applys* typing_struct_keeps_free_var. } }
Qed.

(** If a value has a type, then it's well-formed. *)

Lemma typing_val_wf_val : forall C φ v T,
  typing_val C φ v T ->
  wf_phi C φ ->
  wf_typ C T ->
  wf_val C v.
Proof using.
  introv HTv Hφ HT. induction HTv; try solve [ constructors~ ].
  { constructors. unfolds wf_phi. inverts H as Hlin HF.
    forwards HwfT: Hφ Hlin. forwards*: follow_typ_wf_accesses HF. }
  { constructors~. introv Hfin. lets Hfin': Hfin.
    rewrite <- H0 in Hfin'. applys~ H2.
    applys* wf_typing_struct. }
  { constructors~. introv Hi. applys~ H2.
    applys* wf_typing_array. }
Qed.

(** Errors can't be typed. *)

Lemma typing_val_not_is_error : forall C φ v T,
  typing_val C φ v T ->
  ~ is_error v.
Proof using.
  introv HTv HN. inverts HTv; unfolds* is_error.
Qed.

(** A type can't be a struct and an array at the same time. *)

Lemma array_xor_struct : forall C T' os T Tfs,
  typing_array C T T' os ->
  typing_struct C T Tfs ->
  False.
Proof using.
  introv HTa HTs. gen Tfs. induction HTa; intros.
  { inverts HTs. }
  { inverts HTs. applys* IHHTa. }
Qed.

(** Connection between arrays and follow_typ allowed paths. *)

Lemma follow_typ_array_access : forall C T os a π Tr Ta,
  typing_array C Ta T os ->
  follow_typ C Ta (a::π) Tr ->
  exists i, a = access_array Ta i.
Proof using.
  introv HTa HF. gen a π Tr. induction HTa; intros.
  { inverts HF as.
    { introv HTa HF. inverts HTa. exists~ i. }
    { introv HN. inverts HN. } }
  { inverts HF as.
    { introv HTa' HF. exists~ i. }
    { introv HN. inverts HN as _ HN.
      false. applys* array_xor_struct. } }
Qed.

(** Connection between structs and follow_typ allowed paths. *)

Lemma follow_typ_struct_access : forall C Tfs a π Tr Ts,
  typing_struct C Ts Tfs ->
  follow_typ C Ts (a::π) Tr ->
  exists f, a = access_field Ts f.
Proof using.
  introv HTs HF. gen a π Tr. induction HTs; intros.
  { inverts HF as.
    { introv HN. inverts HN. }
    { introv HTs Hfin HF. exists~ f. } }
  { inverts HF as.
    { introv HN. inverts HN as _ HN.
      false. applys* array_xor_struct. }
    { introv HTs' Hfin HF. exists~ f. } }
Qed.

(** Extending a well-formed gamma. *)

Lemma wf_gamma_add : forall C z T Γ,
  wf_gamma C Γ ->
  wf_typ C T ->
  wf_gamma C (Ctx.add z T Γ).
Proof using.
  introv HwfΓ HwfT. unfolds. introv HCl.
  unfolds Ctx.lookup. unfolds Ctx.add.
  destruct* z. case_if.
  { inverts~ HCl. }
  { folds Ctx.lookup. applys* HwfΓ. }
Qed.

(** [typing_val] preserves well-formedness. *)

Lemma wf_typing_val_typ : forall C φ v T,
  typing_val C φ v T ->
  wf_typdefctx C ->
  wf_phi C φ ->
  wf_val C v ->
  wf_typ C T.
Proof using.
  introv HT HwfC Hwfφ Hwfv. induction HT;
  try solve [ eauto; constructors* ].
  { constructors. inverts_head read_phi.
    forwards~ Hφl: follow_typ_wf_typ φ[l] T C π. }
  { inverts~ Hwfv. }
  { inverts~ Hwfv. }
Qed.

(** [typing] preserves well-formedness. *)

Lemma wf_typing_typ : forall C φ Γ t T,
  typing C φ Γ t T ->
  wf_typdefctx C ->
  wf_phi C φ ->
  wf_gamma C Γ ->
  wf_trm C t ->
  wf_typ C T.
Proof using.
  introv HT HwfC Hwfφ HwfΓ Hwft. induction HT;
  try solve [ constructors* ];
  try solve [ inverts Hwft as Hwfprim; inverts~ Hwfprim ].
  { inverts Hwft. forwards~: wf_typing_val_typ H. }
  { unfolds wf_gamma. applys* HwfΓ. }
  { constructors. inverts Hwft as Hwfprim. 
    inverts Hwfprim as HwfTs.
    forwards~: wf_typing_struct H HwfTs H0. }
  { constructors. inverts Hwft as Hwfprim.
    inverts Hwfprim as HwfTa.
    forwards~: wf_typing_array H HwfTa. }
  { inverts Hwft as Hwfprim.
    inverts Hwfprim as HwfTs.
    forwards~: wf_typing_struct H HwfTs H0. }
  { inverts Hwft as Hwfprim.
    inverts Hwfprim as HwfTa.
    forwards~: wf_typing_array H HwfTa. }
  { inverts Hwft as Hwft1 Hwft2.
    forwards~: IHHT1. applys~ IHHT2.
    applys~ wf_gamma_add. }
Qed.

End WellformednessLemmas.
