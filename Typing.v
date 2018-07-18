(**

This file describes transformations of the layout of records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export TLCbuffer Semantics LibSet LibMap.


(* ********************************************************************** *)
(* * Typing *)

(* ---------------------------------------------------------------------- *)
(** Typing of state and stack *)

(** Type of the state *)

Definition phi := map loc typ.

(** Type of a stack *)

Definition gamma := Ctx.ctx typ.

(** Full typing environment *)

Record env := make_env {
  env_typdefctx : typdefctx;
  env_phi : phi;
  env_gamma : gamma
}.

Notation "'make_env''" := make_env.

Definition env_add_binding E z X :=
  match E with
  | make_env C φ Γ => make_env C φ (Ctx.add z X Γ)
  end.


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
  | typing_val_array : forall Ta a T n,
      typing_array C Ta T n ->
      length a = n ->
      (forall i, 
        index a i -> 
        typing_val C φ a[i] T) -> 
      typing_val C φ (val_array Ta a) Ta.


(* ---------------------------------------------------------------------- *)
(** Typing of terms *)

Inductive typing : env -> trm -> typ -> Prop :=
  (* Closed values *)
  | typing_trm_val : forall E v T,
      typing_val (env_typdefctx E) (env_phi E) v T ->
      typing E (trm_val v) T
  (* Variables *)
  | typing_var : forall E x T,
      Ctx.lookup x (env_gamma E) = Some T ->
      typing E x T
  (* Binary operations *)
  | typing_binop_add : forall E t1 t2,
      typing E t1 typ_int ->
      typing E t2 typ_int ->
      typing E (trm_app binop_add (t1::t2::nil)) typ_int
  | typing_binop_sub : forall E t1 t2,
      typing E t1 typ_int ->
      typing E t2 typ_int ->
      typing E (trm_app binop_sub (t1::t2::nil)) typ_int
  | typing_binop_eq : forall E t1 t2,
      typing E t1 typ_int ->
      typing E t2 typ_int ->
      typing E (trm_app binop_eq (t1::t2::nil)) typ_bool
  (* Abstract heap operations *)
  | typing_get : forall E T t1,
      typing E t1 (typ_ptr T) ->
      typing E (trm_app (prim_get T) (t1::nil)) T
  | typing_set : forall E T t1 t2,
      typing E t1 (typ_ptr T) ->
      typing E t2 T ->
      typing E (trm_app (prim_set T) (t1::t2::nil)) typ_unit
  | typing_new : forall E T, 
      typing E (trm_app (prim_new T) nil) (typ_ptr T)
  | typing_new_array : forall E t1 T k, (* TODO: This is a problem. *)
      typing E t1 typ_int ->
      typing E (trm_app (prim_new_array T) (t1::nil)) (typ_ptr (typ_array T k))
  | typing_struct_access : forall E Ts t1 Tfs f,
      typing_struct (env_typdefctx E) Ts Tfs ->
      f \indom Tfs ->
      typing E t1 (typ_ptr Ts) ->
      typing E (trm_app (prim_struct_access Ts f) (t1::nil)) (typ_ptr Tfs[f])
  | typing_array_access : forall os E Ta t1 t2 T,
      typing_array (env_typdefctx E) Ta T os ->
      typing E t1 (typ_ptr Ta) ->
      typing E t2 typ_int ->
      typing E (trm_app (prim_array_access Ta) (t1::t2::nil)) (typ_ptr T)
  (* Operations on structs and arrays as values *)
  | typing_struct_get : forall E Ts t1 Tfs f,
      typing_struct (env_typdefctx E) Ts Tfs ->
      f \indom Tfs ->
      typing E t1 (typ_struct Tfs) ->
      typing E (trm_app (prim_struct_get Ts f) (t1::nil)) Tfs[f]
  | typing_array_get : forall os E Ta t1 t2 T,
      typing_array (env_typdefctx E) Ta T os ->
      typing E t1 (typ_array T os) ->
      typing E t2 typ_int ->
      typing E (trm_app (prim_array_get Ta) (t1::t2::nil)) T
  (* Other language constructs *)
  | typing_if : forall E t0 t1 t2 T,
      typing E t0 typ_bool ->
      typing E t1 T ->
      typing E t2 T ->
      typing E (trm_if t0 t1 t2) T
  | typing_let : forall T1 T z t1 t2 E,
      typing E t1 T1 ->
      typing (env_add_binding E z T1) t2 T ->
      typing E (trm_let z t1 t2) T.


(* ---------------------------------------------------------------------- *)
(** Typing of the state and the stack *)

Definition state_typing (C:typdefctx) (φ:phi) (m:state) : Prop :=
      dom m = dom φ
  /\  (forall l, l \indom m -> typing_val C φ m[l] φ[l]).

Definition stack_typing (C:typdefctx) (φ:phi) (Γ:gamma) (S:stack) : Prop := 
  forall x v T,
    Ctx.lookup x S = Some v ->
    Ctx.lookup x Γ = Some T ->
    typing_val C φ v T.


(* ---------------------------------------------------------------------- *)
(** Typdefctx well-foundedness *)

(* Types that can be defined in the typdefctx. *)

Inductive typdefinable (C:typdefctx) : typ -> Prop :=
  | typdefinable_int :
      typdefinable C typ_int
  | typdefinable_aux_double :
      typdefinable C typ_double
  | typdefinable_bool :
      typdefinable C typ_bool
  | typdefinable_prt : forall T,
      typdefinable C T ->
      typdefinable C (typ_ptr T)
  | typdefinable_array_fixed : forall T k, 
      typdefinable C T ->
      typdefinable C (typ_array T k)
  | typdefinable_struct : forall Tfs,
       (forall f,
        f \indom Tfs ->
        typdefinable C Tfs[f]) ->
      typdefinable C (typ_struct Tfs)
  | typdefinable_typvar : forall T Tv,
      Ctx.lookup Tv C = Some T ->
      typdefinable C (typ_var Tv).

Inductive typdefctx_wf : typdefctx -> Prop :=
  | typdefctx_wf_nil :
      typdefctx_wf nil
  | typdefctx_wf_cons : forall Tv Td C,
      Ctx.fresh Tv C ->
      typdefinable C Td ->
      typdefctx_wf C ->
      typdefctx_wf (Ctx.add Tv Td C).

Lemma typdefctx_lookup : forall C Tv Td1 Td2,
  typdefctx_wf C ->
  Ctx.lookup Tv C = Some Td1 ->
  Ctx.lookup Tv C = Some Td2 ->
  Td1 = Td2.
Proof.
  introv HC HTd1 HTd2. induction HC.
  { inverts HTd1. }
  { tests: (Tv=Tv0); inverts HTd1 as HTd1; inverts HTd2 as HTd2; case_if*.
    { inverts HTd1. inverts HTd2. auto. }
    { rewrite var_eq_spec in C1. rewrite istrue_isTrue_eq in *. false. } }
Qed.


(* ---------------------------------------------------------------------- *)
(** Functional predicates *)

(* Inferring array types is functional *)
Lemma functional_typing_array : forall C Ta T1 T2 k1 k2,
  typdefctx_wf C ->
  typing_array C Ta T1 k1 ->
  typing_array C Ta T2 k2 ->
  T1 = T2 /\ k1 = k2.
Proof.
  introv HC HTa1 HTa2. induction HTa1; inverts* HTa2.
  forwards*: typdefctx_lookup C Tv Td Td0. subst.
  applys~ IHHTa1.
Qed.

(* Inferring struct types is functional *)
Lemma functional_typing_struct : forall C Ts Tfs1 Tfs2,
  typdefctx_wf C ->
  typing_struct C Ts Tfs1 ->
  typing_struct C Ts Tfs2 ->
  Tfs1 = Tfs2.
Proof.
  introv HC HTs1 HTs2. induction HTs1; inverts* HTs2.
  forwards*: typdefctx_lookup C Tv Td Td0. subst.
  applys~ IHHTs1.
Qed.

(* Types are well-formed *)
Lemma functional_follow_typ : forall C T π T1 T2,
  typdefctx_wf C ->
  follow_typ C T π T1 ->
  follow_typ C T π T2 ->
  T1 = T2.
Proof.
  introv HC HF1 HF2. induction HF1.
  { inverts* HF2. }
  { inverts HF2 as HTa. applys* IHHF1. 
    forwards* (HT&Hos): functional_typing_array H HTa. subst~. }
  { inverts HF2 as HTs. applys* IHHF1. 
    forwards* HTfs: functional_typing_struct H HTs. subst~. }
Qed.

(* φ is well-formed *)
Lemma read_phi_inj : forall C φ l π T1 T2,
  typdefctx_wf C ->
  read_phi C φ l π T1 ->
  read_phi C φ l π T2 ->
  T1 = T2.
Proof.
  introv HC H1 H2. inverts H1. inverts H2.
  applys* functional_follow_typ.
Qed.
