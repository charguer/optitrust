(**

This file describes the syntax and semantics of an imperative lambda 
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


(* ********************************************************************** *)
(* * Semantics *)

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


(* ---------------------------------------------------------------------- *)
(** Semantics of binary operations *)

Inductive redbinop : binop -> val -> val -> val -> Prop :=
  | redbinop_add : forall n1 n2,
      redbinop binop_add (val_int n1) (val_int n2) (val_int (n1 + n2))
  | redbinop_sub : forall n1 n2,
      redbinop binop_sub (val_int n1) (val_int n2) (val_int (n1 - n2))
  | redbinop_eq_true : forall v1 v2,
      v1 = v2 ->
      redbinop binop_eq v1 v2 (val_bool true)
  | redbinop_eq_false : forall v1 v2,
      v1 <> v2 ->
      redbinop binop_eq v1 v2 (val_bool false).


(* ---------------------------------------------------------------------- *)
(** Uninitialized values construction *)

Inductive uninitialized (C:typdefctx) : typ -> val -> Prop :=
  | uninitialized_bool : 
      uninitialized C typ_bool val_uninitialized
  | uninitialized_int :
      uninitialized C typ_int val_uninitialized
  | uninitialized_double :
      uninitialized C typ_double val_uninitialized
  | uninitialized_ptr : forall T,
      uninitialized C (typ_ptr T) val_uninitialized
  | uninitialized_array : forall T Ta os a,
      Ta = typ_array T os ->
      (forall n, 
        os = Some n -> 
        length a = n) ->
      (forall i,
        index a i ->
        uninitialized C T a[i]) ->
      uninitialized C Ta (val_array Ta a)
  | uninitialized_struct : forall Ts Tfs vfs,
      Ts = typ_struct Tfs ->
      dom Tfs = dom vfs ->
      (forall f,
        f \indom Tfs ->
        uninitialized C Tfs[f] vfs[f]) ->
      uninitialized C Ts (val_struct Ts vfs)
  | uninitialized_typvar_array : forall T Tv v a,
      Tv = typ_var T ->
      T \indom C ->
      uninitialized C C[T] (val_array C[T] a) ->
      uninitialized C Tv (val_array Tv a)
  | uninitialized_typvar_struct : forall T Tv v m,
      Tv = typ_var T ->
      T \indom C ->
      uninitialized C C[T] (val_struct C[T] m) ->
      uninitialized C Tv (val_struct Tv m).


(* ---------------------------------------------------------------------- *)
(** Semantics of memory accesses *)

(** v[π] = w *)

Inductive read_accesses : val -> accesses -> val -> Prop :=
  | read_accesses_nil : forall v,
      read_accesses v nil v
  | read_accesses_array : forall a (i:Z) T π v,
      index a i -> 
      read_accesses a[i] π v ->
      read_accesses (val_array T a) ((access_array T i)::π) v
  | read_accesses_struct : forall T s f π v2,
      f \indom s ->
      read_accesses s[f] π v2 ->
      read_accesses (val_struct T s) ((access_field T f)::π) v2.

(** m(l)[π] = v *)

Inductive read_state (m:state) (l:loc) (π:accesses) (v:val) : Prop :=
  | read_state_intro :
      l \indom m ->
      read_accesses m[l] π v ->
      read_state m l π v.

(** v[π := w] = v' *)

Inductive write_accesses : val -> accesses -> val -> val -> Prop :=
  | write_accesses_nil : forall v w,
      write_accesses v nil w w
  | write_accesses_array : forall v1 v2 a1 i T π w a2,
      index a1 i -> 
      write_accesses a1[i] π w v2 ->
      a2 = update a1 i v2 ->
      write_accesses (val_array T a1) ((access_array T i)::π) w (val_array T a2)
  | write_accesses_struct : forall T s1 s2 f π w v2,
      f \indom s1 ->
      write_accesses s1[f] π w v2 ->
      s2 = s1[f := v2] ->
      write_accesses (val_struct T s1) ((access_field T f)::π) w (val_struct T s2).

(** m[l := m(l)[π := w]] = m' *)

Inductive write_state (m:state) (l:loc) (π:accesses) (w:val) (m':state) : Prop :=
  | write_mem_intro : forall v2,
      l \indom m ->
      write_accesses m[l] π w v2 ->
      m' = m[l := v2] ->
      write_state m l π w m'.


(* ---------------------------------------------------------------------- *)
(** Big-step evaluation *)

(** <C, S, m, t> // <m', v> *)

Inductive red (C:typdefctx) :  stack -> state -> trm -> state -> val -> Prop :=
  (* Basic language constructs *)
  | red_val : forall S m v,
      red C S m v m v
  | red_var : forall S m x v,
      Ctx.lookup x S = Some v ->
      red C S m (trm_var x) m v
  | red_if : forall m2 b S m1 t0 t1 t2 m3 vr,
      red C S m1 t0 m2 (val_bool b) ->
      red C S m2 (if b then t1 else t2) m3 vr ->
      red C S m1 (trm_if t0 t1 t2) m3 vr
  | red_let : forall m2 v1 S m1 z t1 t2 m3 vr,
      red C S m1 t1 m2 v1 ->
      ~ is_error v1 ->
      red C (Ctx.add z v1 S) m2 t2 m3 vr ->
      red C S m1 (trm_let z t1 t2) m3 vr
  (* Binary operations *)
  | red_binop : forall S (op:binop) m v1 v2 vr,
      ~ is_error v1 ->
      ~ is_error v2 ->
      redbinop op v1 v2 vr ->
      red C S m (trm_app op ((trm_val v1)::(trm_val v2)::nil)) m vr
  (* Operations on the abstract heap *) 
  | red_get : forall l π S T v1 m vr,
      v1 = val_abstract_ptr l π ->
      read_state m l π vr ->
      ~ is_uninitialized vr ->
      red C S m (trm_app (prim_get T) ((trm_val v1)::nil)) m vr
  | red_set : forall l π  S m1 T v1 v2 m2 vr,
      v1 = val_abstract_ptr l π ->
      vr = val_unit ->
      ~ is_error v2 ->
      write_state m1 l π v2 m2 ->
      red C S m1 (trm_app (prim_set T) ((trm_val v1)::(trm_val v2)::nil)) m2 vr
  | red_new : forall l v S m1 T m2 vr,
      vr = val_abstract_ptr l nil ->
      l <> null ->
      l \notindom m1 ->
      uninitialized C T v -> 
      m2 = m1[l := v] ->
      red C S m1 (trm_app (prim_new T) nil) m2 vr
  | red_new_array : forall l (n:int) (k:nat) v S m1 T v1 m2 vr,
      v1 = val_int n ->
      vr = val_abstract_ptr l nil ->
      l <> null ->
      l \notindom m1 ->
      n = k ->
      uninitialized C (typ_array T (Some k)) v -> 
      m2 = m1[l := v] ->
      red C S m1 (trm_app (prim_new_array T) ((trm_val v1)::nil)) m2 vr
  | red_struct_access : forall l π S T f v1 m vr,
      v1 = val_abstract_ptr l π ->
      vr = val_abstract_ptr l (π ++ ((access_field T f)::nil)) ->
      red C S m (trm_app (prim_struct_access T f) ((trm_val v1)::nil)) m vr
  | red_array_access : forall l π i S T v1 v2 m vr,
      v1 = val_abstract_ptr l π ->
      v2 = val_int i ->
      vr = val_abstract_ptr l (π++(access_array T i)::nil) ->
      red C S m (trm_app (prim_array_access T) ((trm_val v1)::(trm_val v2)::nil)) m vr
  (* Operations on composed values *)
  | red_struct_get : forall s f S T v1 m vr,
      v1 = val_struct T s ->
      vr = s[f] ->
      f \indom s ->
      red C S m (trm_app (prim_struct_get T f) ((trm_val v1)::nil)) m vr
  | red_array_get : forall a i S T v1 v2 m vr,
      v1 = val_array T a ->
      v2 = val_int i ->
      vr = a[i] ->
      index a i ->
      red C S m (trm_app (prim_array_get T) ((trm_val v1)::(trm_val v2)::nil)) m vr
  (* Arguments *) 
  | red_args_1 : forall v1 m2 S m1 op t1 ts m3 vr,
      ~ is_val t1 ->
      red C S m1 t1 m2 v1 ->
      red C S m2 (trm_app op ((trm_val v1)::ts)) m3 vr ->
      red C S m1 (trm_app op (t1::ts)) m3 vr
  | red_args_2 : forall m2 v2 S m1 op v1 t2 ts m3 vr,
      ~ is_val t2 ->
      red C S m1 t2 m2 v2 ->
      red C S m2 (trm_app op ((trm_val v1)::(trm_val v2)::ts)) m3 vr ->
      red C S m1 (trm_app op ((trm_val v1)::t2::ts)) m3 vr
  (* Error cases *)
  | red_var_error : forall S x m,
      Ctx.lookup x S = None ->
      red C S m (trm_var x) m val_error
  | red_if_error_not_a_bool : forall v0 S m1 t0 t1 t2 m2,
      red C S m1 t0 m2 v0 ->
      ~ is_bool v0 ->
      red C S m1 (trm_if t0 t1 t2) m2 val_error
  | red_let_error_let : forall S m1 z t1 t2 m2,
      red C S m1 t1 m2 val_error ->
      red C S m1 (trm_let z t1 t2) m2 val_error
  | red_binop_error : forall S (op:binop) v1 v2 m,
      ~ (exists v, redbinop op v1 v2 v) ->
      red C S m (trm_app op ((trm_val v1)::(trm_val v2)::nil)) m val_error
  | red_get_error_not_a_ptr : forall S T v1 m,
      ~ is_ptr v1 ->
      red C S m (trm_app (prim_get T) ((trm_val v1)::nil)) m val_error
  | red_get_error_bad_address : forall l π S T v1 m,
      v1 = val_abstract_ptr l π ->
      ~ (exists w, read_state m l π w) ->
      red C S m (trm_app (prim_get T) ((trm_val v1)::nil)) m val_error
  | red_set_error_not_a_ptr : forall S T v1 v2 m,
      ~ is_ptr v1 ->
      red C S m (trm_app (prim_set T) ((trm_val v1)::(trm_val v2)::nil)) m val_error
  | red_set_error_bad_address : forall l π  S T v1 v2 m,
      v1 = val_abstract_ptr l π ->
      ~ (exists m', write_state m l π v2 m') ->
      red C S m (trm_app (prim_set T) ((trm_val v1)::(trm_val v2)::nil)) m val_error
  | red_new_error : forall S T m,
      ~ (exists v, uninitialized C T v) ->
      red C S m (trm_app (prim_new T) nil) m val_error
  | red_new_array_error : forall (n:int) (k:nat) S m1 T v1 m2,
      v1 = val_int n ->
      n = k ->
      ~ (exists v, uninitialized C (typ_array T (Some k)) v) -> 
      red C S m1 (trm_app (prim_new_array T) ((trm_val v1)::nil)) m2 val_error
  | red_struct_access_error_not_a_ptr : forall S T f v1 m,
      ~ is_ptr v1 ->
      red C S m (trm_app (prim_struct_access T f) ((trm_val v1)::nil)) m val_error
  | red_array_access_error_not_a_ptr : forall S T v1 v2 m,
      ~ is_ptr v1 ->
      red C S m (trm_app (prim_array_access T) ((trm_val v1)::(trm_val v2)::nil)) m val_error
  | red_array_access_error_not_an_int : forall S T v1 v2 m,
      ~ is_int v2 ->
      red C S m (trm_app (prim_array_access T) ((trm_val v1)::(trm_val v2)::nil)) m val_error
  | red_struct_get_error_not_a_struct : forall S T f v1 m,
      ~ is_struct v1 ->
      red C S m (trm_app (prim_struct_get T f) ((trm_val v1)::nil)) m val_error
  | red_struct_get_error_invalid_field : forall s S T f v1 m,
      v1 = val_struct T s ->
      f \notindom s ->
      red C S m (trm_app (prim_struct_get T f) ((trm_val v1)::nil)) m val_error
  | red_array_get_error_not_an_array : forall S T v1 v2 m,
      ~ is_array v1 ->
      red C S m (trm_app (prim_array_get T) ((trm_val v1)::(trm_val v2)::nil)) m val_error
  | red_array_get_error_not_an_int : forall S T v1 v2 m,
      ~ is_int v2 ->
      red C S m (trm_app (prim_array_get T) ((trm_val v1)::(trm_val v2)::nil)) m val_error
  | red_array_get_error_out_of_bounds : forall a i S T v1 v2 m,
      v1 = val_array T a ->
      v2 = val_int i ->
      ~ (index a i) ->
      red C S m (trm_app (prim_array_get T) ((trm_val v1)::(trm_val v2)::nil)) m val_error
  | red_args_1_error : forall S m1 op t1 ts m2,
      red C S m1 t1 m2 val_error ->
      red C S m1 (trm_app op (t1::ts)) m2 val_error
  | red_args_2_error : forall S m1 op v1 t2 ts m2,
      ~ is_error v1 ->
      red C S m1 t2 m2 val_error ->
      red C S m1 (trm_app op ((trm_val v1)::t2::ts)) m2 val_error.

(* Derived *)

Lemma red_seq : forall C S m1 m2 m3 t1 t2 r1 r,
  red C S m1 t1 m2 r1 ->
  ~ is_error r1 ->
  red C S m2 t2 m3 r ->
  red C S m1 (trm_seq t1 t2) m3 r.
Proof using. intros. applys* red_let. Qed.


(* ********************************************************************** *)
(* * Lemmas about the semantics *)

(* ---------------------------------------------------------------------- *)
(** Lemmas about accesses *)

Lemma read_write_accesses_same : forall v1 v2 π w,
  write_accesses v1 π w v2 ->
  read_accesses v2 π w.
Proof.
  introv H. induction H; subst; constructors*; rew_reads~.
Qed.

Hint Extern 1 (?j \in dom (?m[?i:=?v])) => applys @indom_update.


Lemma read_write_state_same : forall m m' l π w,
  write_state m l π w m' ->
  read_state m' l π w.
Proof.
  introv H. induction H. subst. constructors*.
  { applys* read_write_accesses_same. rew_reads*. }
Qed.


(* ---------------------------------------------------------------------- *)
(** Lemmas about the stack structure *)

Lemma ctx_lookup_add_inv {A:Type} : forall C (z1 z2:var) (w1 w2:A),
  Ctx.lookup z1 (Ctx.add z2 w1 C) = Some w2 ->
      (z1 = z2 /\ w1 = w2)
  \/  (z1 <> z2 /\ Ctx.lookup z1 C = Some w2).
Proof.
  introv H. simpls. rewrite var_eq_spec in *. case_if*. { inverts* H. }
Qed.


(* ---------------------------------------------------------------------- *)
(** Lemmas about the completeness of the reduction rules *)

Lemma red_complete : forall C S m1 t, 
  exists v m2, red C S m1 t m2 v.
Proof.
Admitted.


(* TODO:
   - rew_reads improve.
   - dom_prove new tactic. 
   - map on maps. 
   - automation for red_complete. *)

