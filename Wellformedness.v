(**

This file describes what it means for the different constructs in the
language to be well-founded.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export TLCbuffer Language.


(* ---------------------------------------------------------------------- *)
(** Well-foundedness of types *)

Inductive wf_typ (C:typdefctx) : typ -> Prop :=
  | wf_typ_unit :
      wf_typ C typ_unit
  | wf_typ_int :
      wf_typ C typ_int
  | wf_typ_double :
      wf_typ C typ_double
  | wf_typ_bool :
      wf_typ C typ_bool
  | wf_typ_ptr : forall T,
      T <> typ_ptr T ->
      wf_typ C T ->
      wf_typ C (typ_ptr T)
  | wf_typ_array : forall T os,
      T <> typ_array T os ->
      wf_typ C T ->
      wf_typ C (typ_array T os)
  | wf_typ_struct : forall Tfs,
      (forall f,
        f \indom Tfs ->
        wf_typ C Tfs[f]) ->
      wf_typ C (typ_struct Tfs)
  | wf_typ_var : forall Tv,
      Tv \indom C ->
      wf_typ C C[Tv] ->
      wf_typ C (typ_var Tv).


(* ---------------------------------------------------------------------- *)
(** Well-foundedness  of values and terms *)

(* A value is well-founded if all of the types that appear in it are. *)

Inductive wf_accesses (C:typdefctx) : accesses -> Prop :=
  | wf_accesses_nil :
      wf_accesses C nil 
  | wf_accesses_cons_array : forall T i π,
      wf_typ C T ->
      wf_accesses C π ->
      wf_accesses C ((access_array T i)::π)
  | wf_accesses_cons_field : forall T f π,
      wf_typ C T ->
      wf_accesses C π ->
      wf_accesses C ((access_field T f)::π).

Inductive wf_val (C:typdefctx) : val -> Prop :=
  | wf_val_error :
      wf_val C val_error
  | wf_val_uninitialized :
      wf_val C val_uninitialized
  | wf_val_unit :
      wf_val C val_unit
  | wf_val_bool : forall b,
      wf_val C (val_bool b)
  | wf_val_int : forall i,
      wf_val C (val_int i)
  | wf_val_double : forall d,
      wf_val C (val_double d)
  | wf_val_abstract_ptr : forall l π,
      wf_accesses C π ->
      wf_val C (val_abstract_ptr l π)
  | wf_val_concrete_ptr : forall l o,
      wf_val C (val_concrete_ptr l o)
  | wf_val_array : forall Ta a,
      wf_typ C Ta ->
      (forall i,
        index a i ->
        wf_val C a[i]) ->
      wf_val C (val_array Ta a)
  | wf_val_struct : forall Ts s,
      wf_typ C Ts ->
      (forall f,
        f \indom s ->
        wf_val C s[f]) ->
      wf_val C (val_struct Ts s).

(* A term is well-founded if all of the types that appear in it are. *)

Inductive wf_prim (C:typdefctx) : prim -> Prop :=
  | wf_prim_binop : forall bop,
      wf_prim C (prim_binop bop)
  | wf_prim_get : forall T,
      wf_typ C T ->
      wf_prim C (prim_get T)
  | wf_prim_set : forall T,
      wf_typ C T ->
      wf_prim C (prim_set T)
  | wf_prim_new : forall T,
      wf_typ C T ->
      wf_prim C (prim_new T)
  | wf_prim_new_array : forall T,
      wf_typ C T ->
      wf_prim C (prim_new_array T)
  | wf_prim_struct_access : forall T f,
      wf_typ C T ->
      wf_prim C (prim_struct_access T f)
  | wf_prim_array_access : forall T,
      wf_typ C T ->
      wf_prim C (prim_array_access T)
  | wf_prim_struct_get : forall T f,
      wf_typ C T ->
      wf_prim C (prim_struct_get T f)
  | wf_prim_array_get : forall T,
      wf_typ C T ->
      wf_prim C (prim_array_get T)
  | wf_prim_ll_get : forall T,
      wf_typ C T ->
      wf_prim C (prim_ll_get T)
  | wf_prim_ll_set : forall T,
      wf_typ C T ->
      wf_prim C (prim_ll_set T)
  | wf_prim_ll_access : forall T,
      wf_typ C T ->
      wf_prim C (prim_ll_access T)
  | wf_prim_ll_new : forall T,
      wf_typ C T ->
      wf_prim C (prim_ll_new T).

Inductive wf_trm (C:typdefctx) : trm -> Prop :=
  | wf_trm_val : forall v,
      wf_val C v ->
      wf_trm C (trm_val v)
  | wf_trm_var : forall x,
      wf_trm C (trm_var x)
  | wf_trm_if : forall t0 t1 t2,
      wf_trm C t0 ->
      wf_trm C t1 ->
      wf_trm C t2 ->
      wf_trm C (trm_if t0 t1 t2)
  | wf_trm_let : forall x t0 t1,
      wf_trm C t0 ->
      wf_trm C t1 ->
      wf_trm C (trm_let x t0 t1)
  | wf_trm_app_args_0 : forall op,
      wf_prim C op ->
      wf_trm C (trm_app op nil)
  | wf_trm_app_args_1 : forall op t1,
      wf_prim C op ->
      wf_trm C t1 ->
      wf_trm C (trm_app op (t1::nil))
  | wf_trm_app_args_2 : forall op t1 t2,
      wf_prim C op ->
      wf_trm C t1 ->
      wf_trm C t2 ->
      wf_trm C (trm_app op (t1::t2::nil)).


(* ---------------------------------------------------------------------- *)
(** Well-foundedness of stack and state *)

(* Well-founded stack *)

Definition wf_stack (C:typdefctx) (S:stack) : Prop :=
  forall x v,
    Ctx.lookup x S = Some v ->
    wf_val C v.

(* Well-founded state *)

Definition wf_state (C:typdefctx) (m:state) : Prop :=
  forall l,
    l \indom m ->
    wf_val C m[l].

(* Well-founded gamma *)

Definition wf_gamma (C:typdefctx) (Γ:gamma) : Prop :=
  forall x T,
    Ctx.lookup x Γ = Some T ->
    wf_typ C T.

(* Well-founded phi *)

Definition wf_phi (C:typdefctx) (φ:phi) : Prop :=
  forall l,
    l \indom φ ->
    wf_typ C φ[l].


(* ---------------------------------------------------------------------- *)
(** Typdefctx well-foundedness *)

(* Type variable appears in the type. *)

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


