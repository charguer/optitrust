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
  LibMonoid LibSet LibContainer LibList LibListZ LibMap.

Open Scope set_scope.
Open Scope container_scope.


(* ********************************************************************** *)
(* * Syntax *)

(* ---------------------------------------------------------------------- *)
(** Representation of locations and fields *)

(** [loc] describes base pointers to an allocated block. *)

Definition loc := nat. 

Definition null : loc := 0%nat.

Definition field := var.

Definition size := nat.

Definition offset := nat.

Definition typvar := var.


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


(* ---------------------------------------------------------------------- *)
(** Size of types *)

Section TypeSizes.
Open Scope nat_scope.

Definition typdefctx_size := (map typvar size).

Definition typdefctx_offset := (map typvar (map field offset)).

(*Fixpoint sizeof (T:typ) : size := 
  match T with
  | typ_unit => 0
  | typ_bool => 1
  | typ_int => 1
  | typ_double => 2
  | typ_ptr _ => 1
  | typ_array T' (Some n) => (n * (sizeof T'))
  | typ_struct m => fold (monoid_make plus 0) (fun f T' => sizeof T') m
  | typ_fun _ _ => 1
  | _ => 0
  end.*)

End TypeSizes.


(* ---------------------------------------------------------------------- *)
(** Syntax of the source language *)

Inductive access : Type :=
  | access_array : typ -> int -> access
  | access_field : typ -> field -> access.

Definition accesses := list access. 

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
  | prim_array_get : typ -> prim.

Inductive basic_val : Type :=
  | val_error : basic_val
  | val_unit : basic_val
  | val_uninitialized : basic_val
  | val_bool : bool -> basic_val
  | val_int : int -> basic_val
  | val_double : int -> basic_val
  | val_abstract_ptr : loc -> accesses -> basic_val
  | val_concrete_ptr : loc -> offset -> basic_val.

Inductive val : Type :=
  | val_basic : basic_val -> val
  | val_array : typ -> list val -> val
  | val_struct : typ -> map field val -> val.

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

Global Instance Inhab_basic_val : Inhab basic_val.
Proof using. apply (Inhab_of_val val_unit). Qed.

Global Instance Inhab_val : Inhab val.
Proof using. apply (Inhab_of_val (val_basic val_unit)). Qed.

Global Instance Inhab_trm : Inhab trm.
Proof using. apply (Inhab_of_val (trm_val (val_basic val_unit))). Qed.

Global Instance Inhab_typ : Inhab typ.
Proof using. apply (Inhab_of_val typ_unit). Qed.

Hint Extern 1 (Inhab val) => apply Inhab_val.

Hint Extern 1 (Inhab typ) => apply Inhab_typ.


(* ---------------------------------------------------------------------- *)
(** Coercions *)

Coercion prim_binop : binop >-> prim.
Coercion val_basic : basic_val >-> val.
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
  | trm_val v => True
  | _ => False
  end.

Definition is_basic (v:val) :=
  match v with
  | val_basic vb => True
  | _ => False
  end.

Definition is_error (v:val) :=
  match v with
  | val_basic val_error => True
  | _ => False
  end.

Definition is_uninitialized (v:val) :=
  match v with
  | val_basic val_uninitialized => True
  | _ => False
  end.

Definition is_bool (v:val) :=
  match v with
  | val_basic (val_bool b) => True
  | _ => False
  end.

Definition is_ptr (v:val) :=
  match v with
  | val_basic (val_abstract_ptr l π) => True
  | _ => False
  end.

Definition is_int (v:val) :=
  match v with
  | val_basic (val_int i) => True
  | _ => False
  end.

Definition is_struct (v:val) :=
  match v with
  | val_struct T s => True
  | _ => False
  end.

Definition is_array (v:val) :=
  match v with
  | val_array T a => True
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
