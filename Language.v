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

Notation typvar := var.


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

Definition typdefctx := map typvar typ.


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

(** TODO: Change this! Probably use Flocq? *)
Definition double := int.

Inductive val : Type :=
  | val_error : val
  | val_unit : val
  | val_uninitialized : val
  | val_bool : bool -> val
  | val_int : int -> val
  | val_double : double -> val
  | val_abstract_ptr : loc -> accesses -> val
  | val_concrete_ptr : loc -> offset -> val
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

(** Shorthand [vars], [vals] and [trms] for lists of items. *)

Definition vals : Type := list val.
Definition trms : Type := list trm.


(* ---------------------------------------------------------------------- *)
(** Inhabited types *)

(** The type of values is inhabited *)

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
Coercion val_int : Z >-> val.
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
Implicit Types vs : vals.
Implicit Types ts : trms.


(* ---------------------------------------------------------------------- *)
(** Auxiliary predicates for the semantics *)

Definition is_val (t:trm) :=
  match t with
  | trm_val v => True
  | _ => False
  end.

Definition is_error (v:val) :=
  match v with
  | val_error => True
  | _ => False
  end.

Definition is_uninitialized (v:val) :=
  match v with
  | val_uninitialized => True
  | _ => False
  end.

Definition is_bool (v:val) :=
  match v with
  | val_bool b => True
  | _ => False
  end.

Definition is_ptr (v:val) :=
  match v with
  | val_abstract_ptr l π => True
  | _ => False
  end.

Definition is_int (v:val) :=
  match v with
  | val_int i => True
  | _ => False
  end.

Definition is_struct (v:val) :=
  match v with
  | val_struct T m => True
  | _ => False
  end.

Definition is_array (v:val) :=
  match v with
  | val_array T a => True
  | _ => False
  end.


(* ---------------------------------------------------------------------- *)
(** State and stack *)

(** Representation of the state *)

Definition state := map loc val.

(** Representation of the stack *)

Definition stack := Ctx.ctx val.
