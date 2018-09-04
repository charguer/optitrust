(**

This file describes the syntax of an imperative lambda 
calculus with records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export Bind TLCbuffer.
Require Export LibString LibCore LibLogic LibReflect 
  LibOption LibRelation LibLogic LibOperation LibEpsilon 
  LibMonoid LibSet LibContainer LibListZ LibMap.

Open Scope set_scope.
Open Scope container_scope.

(* ********************************************************************** *)
(* * Syntax *)

(* ---------------------------------------------------------------------- *)
(** Representation of locations and fields *)

(** [loc] describes base pointers to an allocated block. *)

Definition loc := int.

Definition null : loc := 0%Z.

Definition field := var.

Definition size := int.

Definition offset := int.

Definition typvar := var.

(* Representation of low level memory blocks. *)

Inductive word : Type :=
  | word_undef : word
  | word_int : int -> word.

Definition words := list word.

(* ---------------------------------------------------------------------- *)
(** Grammar of types *)

Inductive typ : Type :=
  | typ_unit : typ
  | typ_int : typ
  | typ_double : typ
  | typ_bool : typ
  | typ_ptr : typ -> typ
  | typ_array : typ -> option size -> typ
  | typ_struct : map field typ -> typ
  | typ_fun : list typ -> typ -> typ
  | typ_var : typvar -> typ.

(* Type definitions context *)

Definition typdefctx := map typvar typ.

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

(* Contex holding low-level information about structs and their fields. *)

Definition ll_typdefctx_typvar_sizes := map typvar size.
Definition ll_typdefctx_fields_offsets := map typvar (map field offset).
Definition ll_typdefctx_fields_order := map typvar (list field).

Record ll_typdefctx := make_ll_typdefctx {
  typvar_sizes   : ll_typdefctx_typvar_sizes;
  fields_offsets : ll_typdefctx_fields_offsets;
  fields_order   : ll_typdefctx_fields_order }.

Notation "'make_ll_typdefctx''" := make_ll_typdefctx.

(* Alpha *)

Definition alpha := map loc loc.


(* ---------------------------------------------------------------------- *)
(** Syntax of the source language *)

Inductive access : Type :=
  | access_array : typ -> int -> access
  | access_field : typ -> field -> access.

Definition accesses := list access.

Inductive val : Type :=
  | val_error : val
  | val_unit : val
  | val_uninitialized : val
  | val_bool : bool -> val
  | val_int : int -> val
  | val_double : int -> val
  | val_abstract_ptr : loc -> accesses -> val
  | val_concrete_ptr : loc -> offset -> val
  | val_array : typ -> list val -> val
  | val_struct : typ -> map field val -> val
  | val_words : list word -> val.

Inductive binop : Type :=
  | binop_eq : binop
  | binop_sub : binop
  | binop_add : binop
  | binop_mul : binop
  | binop_div : binop
  | binop_mod : binop
  | binop_ptr_add : binop.

Inductive prim : Type :=
  | prim_binop : binop -> prim
  | prim_get : typ -> prim
  | prim_set : typ -> prim
  | prim_new : typ -> prim
  | prim_new_array : typ -> prim
  | prim_struct_access : typ -> field -> prim
  | prim_array_access : typ -> prim
  | prim_struct_get : typ -> field -> prim
  | prim_array_get : typ -> prim
  | prim_ll_get : typ -> prim
  | prim_ll_set : typ -> prim
  | prim_ll_new : typ -> prim
  | prim_ll_access : typ -> prim.

Inductive trm : Type :=
  | trm_var : var -> trm
  | trm_val : val -> trm
  | trm_if : trm -> trm -> trm -> trm
  | trm_let : bind -> trm -> trm -> trm
  | trm_app : prim -> list trm -> trm
  | trm_while : trm -> trm -> trm
  | trm_for : var -> val -> val -> trm -> trm.

(** Sequence is a special case of let bindings *)

Notation trm_seq := (trm_let bind_anon).


(* ---------------------------------------------------------------------- *)
(** State and stack *)

(** Representation of the state *)

Definition state := map loc val.

(** Representation of the stack *)

Definition stack := Ctx.ctx val.


(* ---------------------------------------------------------------------- *)
(** Inhabited types, values and terms *)

(** The type of values is inhabited *)

Global Instance Inhab_word : Inhab word.
Proof using. apply (Inhab_of_val word_undef). Qed.

Global Instance Inhab_val : Inhab val.
Proof using. apply (Inhab_of_val val_unit). Qed.

Global Instance Inhab_trm : Inhab trm.
Proof using. apply (Inhab_of_val (trm_val val_unit)). Qed.

Global Instance Inhab_typ : Inhab typ.
Proof using. apply (Inhab_of_val typ_unit). Qed.

Hint Extern 1 (Inhab val) => apply Inhab_val.

Hint Extern 1 (Inhab typ) => apply Inhab_typ.


(* ---------------------------------------------------------------------- *)
(** Coercions *)

Coercion prim_binop : binop >-> prim.
Coercion trm_val : val >-> trm.
Coercion trm_var : var >-> trm.
Coercion trm_app : prim >-> Funclass.


(* ---------------------------------------------------------------------- *)
(** Implicit types *)

Implicit Types t : trm.
Implicit Types v : val.
Implicit Types l : loc.
Implicit Types b : bool.
Implicit Types x : var.
Implicit Types z : bind.
Implicit Types vs : list val.
Implicit Types ts : list trm.


(* ---------------------------------------------------------------------- *)
(** Auxiliary predicates for the semantics and transformations *)

Definition is_val (t:trm) :=
  match t with
  | trm_val _ => True
  | _ => False
  end.

Definition is_basic (v:val) :=
  match v with
  | val_error => True
  | val_unit => True
  | val_uninitialized => True
  | val_bool _ => True
  | val_int _ => True
  | val_double _ => True
  | val_abstract_ptr _ _ => True
  | val_concrete_ptr _ _ => True
  | _ => False
  end.

Definition is_error (v:val) :=
  match v with
  | val_error => True
  | _ => False
  end.

Inductive is_uninitialized : val -> Prop :=
  | is_uninitialized_val_uninitialized :
      is_uninitialized val_uninitialized
  | is_uninitialized_array : forall T a,
      (exists i, index a i /\ is_uninitialized a[i]) ->
      is_uninitialized (val_array T a)
  | is_uninitialized_struct : forall T s,
      (exists f, f \indom s /\ is_uninitialized s[f]) ->
      is_uninitialized (val_struct T s).

Definition is_undef (v:val) :=
  match v with
  | val_words ws => exists i, index ws i /\ ws[i] = word_undef
  | _ => False
  end.

Definition is_bool (v:val) :=
  match v with
  | val_bool _ => True
  | _ => False
  end.

Definition is_ptr (v:val) :=
  match v with
  | val_abstract_ptr _ _ => True
  | _ => False
  end.

Definition is_int (v:val) :=
  match v with
  | val_int _ => True
  | _ => False
  end.

Definition is_struct (v:val) :=
  match v with
  | val_struct _ _ => True
  | _ => False
  end.

Definition is_array (v:val) :=
  match v with
  | val_array _ _ => True
  | _ => False
  end.

Definition is_struct_op (op:prim) :=
  match op with
  | prim_struct_access _ _ => True
  | prim_struct_get _ _ => True
  | _ => False
  end.

Definition is_array_op (op:prim) :=
  match op with
  | prim_array_access _ => True
  | prim_array_get _ => True
  | _ => False
  end.
