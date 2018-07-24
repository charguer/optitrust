(**

This file describes transformations of the layout of records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export TLCbuffer Language.


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
(** Typing of arrays and structs *)

Inductive typing_array (C:typdefctx) : typ -> typ -> option size -> Prop :=
  | typing_array_base : forall T os,
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
  | typing_new_array : forall E t1 T,
      typing E t1 typ_int ->
      typing E (trm_app (prim_new_array T) (t1::nil)) (typ_ptr (typ_array T None))
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

(* typvar appears in the type. *)
Inductive free_typvar (C:typdefctx) (Tv:typvar) : typ -> Prop :=  
  | free_typvar_typvar_eq :
      free_typvar C Tv (typ_var Tv)
  | free_typvar_typvar_other : forall Tv',
      Tv <> Tv' ->
      Tv' \indom C -> 
      free_typvar C Tv C[Tv'] ->
      free_typvar C Tv (typ_var Tv')
  | free_typvar_ptr : forall T,
      free_typvar C Tv T ->
      free_typvar C Tv (typ_ptr T)
  | free_typvar_array : forall T os,
      free_typvar C Tv T ->
      free_typvar C Tv (typ_array T os)
  | free_typvar_struct : forall Tfs,
      (exists f,
        f \indom Tfs /\
        free_typvar C Tv Tfs[f]) ->
      free_typvar C Tv (typ_struct Tfs).

Definition wf_typdefctx (C:typdefctx) : Prop :=
  forall Tv,
    Tv \indom C ->
    ~ free_typvar C Tv C[Tv].


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

(* Types are well-formed *)
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

(* φ is well-formed *)
Lemma read_phi_inj : forall C φ l π T1 T2,
  read_phi C φ l π T1 ->
  read_phi C φ l π T2 ->
  T1 = T2.
Proof.
  introv HR1 HR2. inverts HR1. inverts HR2.
  applys* functional_follow_typ.
Qed.


(* ---------------------------------------------------------------------- *)
(** Validity of types *)

Inductive valid_typ (C:typdefctx) : typ -> Prop :=
  | valid_typ_unit :
      valid_typ C typ_unit
  | valid_typ_int :
      valid_typ C typ_int
  | valid_typ_double :
      valid_typ C typ_double
  | valid_typ_bool :
      valid_typ C typ_bool
  | valid_typ_ptr : forall T,
      valid_typ C T ->
      valid_typ C (typ_ptr T)
  | valid_typ_array : forall T os,
      valid_typ C T ->
      valid_typ C (typ_array T os)
  | valid_typ_struct : forall Tfs,
      (forall f,
        f \indom Tfs ->
        valid_typ C Tfs[f]) ->
      valid_typ C (typ_struct Tfs)
  | valid_typ_var : forall Tv,
      Tv \indom C ->
      valid_typ C C[Tv] ->
      valid_typ C (typ_var Tv).

Definition valid_phi (C:typdefctx) (φ:phi) : Prop :=
  forall l,
    l \indom φ ->
    valid_typ C φ[l].

Inductive valid_accesses (C:typdefctx) : accesses -> Prop :=
  | valid_accesses_nil :
      valid_accesses C nil 
  | valid_accesses_cons_array : forall T i π,
      valid_typ C T ->
      valid_accesses C π ->
      valid_accesses C ((access_array T i)::π)
  | valid_accesses_cons_field : forall T f π,
      valid_typ C T ->
      valid_accesses C π ->
      valid_accesses C ((access_field T f)::π).


(* Lemmas about the connection of validity and typing. *)

Lemma valid_typing_array : forall T os C Ta,
  typing_array C Ta T os ->
  valid_typ C Ta ->
  valid_typ C T.
Proof.
  introv HTa HT. induction HTa; inverts~ HT.
Qed.

Lemma valid_typing_struct : forall Tfs C Ts,
  typing_struct C Ts Tfs ->
  valid_typ C Ts ->
  (forall f, 
    f \indom Tfs ->
    valid_typ C Tfs[f]).
Proof.
  introv HTs HT. induction HTs; inverts~ HT.
Qed.

Lemma valid_typing_array_inv : forall T os C Ta,
  typing_array C Ta T os ->
  valid_typ C T ->
  valid_typ C Ta.
Proof.
  introv HTa HT. induction HTa; constructors~.
Qed.

Lemma valid_typing_struct_inv : forall Tfs C Ts,
  typing_struct C Ts Tfs ->
  (forall f, 
    f \indom Tfs ->
    valid_typ C Tfs[f]) ->
  valid_typ C Ts.
Proof.
  introv HTs HT. induction HTs; constructors~.
Qed.

Lemma follow_typ_valid_accesses : forall T T' C π,
  valid_typ C T ->
  follow_typ C T π T' ->
  valid_accesses C π .
Proof.
  introv HT HF. induction HF; constructors~.
  { applys IHHF. applys* valid_typing_array. }
  { applys IHHF. applys* valid_typing_struct. }
Qed.

Lemma follow_typ_valid_typ : forall T T' C π,
  valid_typ C T ->
  follow_typ C T π T' ->
  valid_accesses C π  ->
  valid_typ C T'.
Proof.
  introv HT HF Hva. induction HF.   
  { auto. }
  { inverts Hva. applys~ IHHF. applys* valid_typing_array. }
  { inverts Hva. applys~ IHHF. applys* valid_typing_struct. }
Qed.

Lemma typing_valid_typ : forall φ v C T,
  valid_phi C φ ->
  typing_val C φ v T ->
  valid_typ C T.
Proof.
  introv Hφ HT. induction HT; try solve [ constructors~ ].
  { admit. (*uninitalised should only accept valid types*) }
  { constructors. inverts H. 
    applys* follow_typ_valid_typ.
    applys* follow_typ_valid_accesses. }
  { applys* valid_typing_struct_inv. 
    introv Hfin. applys~ H2. rewrite~ <- H0. }
  { admit. (*empty arrays can have unacceptable types*) }
Qed.
