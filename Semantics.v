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

Global Opaque field loc size offset typvar.


(* ---------------------------------------------------------------------- *)
(** Grammar of types *)

Inductive typ : Type :=
  | typ_unit : typ
  | typ_int : typ
  | typ_double : typ
  | typ_bool : typ
  | typ_ptr : typ -> typ
  | typ_array : typ -> option size -> typ
  | typ_struct : typvar -> typ
  | typ_fun : list typ -> typ -> typ.

Definition typdef_struct := map field typ.

Definition typdefctx := map typvar typdef_struct.


(* ---------------------------------------------------------------------- *)
(** Size of types *)

Definition typdefctx_size := map typvar size.

Definition typdefctx_offset := map typvar (map field offset).

Section TypeSizes.
Open Scope nat_scope.

Fixpoint sizeof (S:typdefctx_size) (T:typ) : nat := 
  match T with
  | typ_unit => 0%nat
  | typ_bool => 1%nat
  | typ_int => 1%nat
  | typ_double => 2%nat
  | typ_ptr _ => 1%nat
  | typ_array T' (Some n) => (n * (sizeof S T'))%nat
  | typ_struct X => S[X]
  | typ_fun _ _ => 1%nat
  | _ => 0%nat
  end.

Definition sizeof_typdef_struct (S:typdefctx_size) (m:typdef_struct) : nat :=
  fold (monoid_make plus 0%nat) (fun f T => sizeof S T) m.

Definition wf_typctx_size (C:typdefctx) (S:typdefctx_size) : Prop :=
  forall X, X \indom C -> 
     X \indom S 
  /\ S[X] = sizeof_typdef_struct S C[X].

End TypeSizes.


(* ---------------------------------------------------------------------- *)
(** Syntax of the source language *)

Inductive access : Type :=
  | access_array : int -> access
  | access_field : typvar -> field -> access.

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
  | prim_struct_access : typvar -> field -> prim
  | prim_array_access : typ -> prim.

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
  | val_prim : prim -> val
  | val_array : list val -> val
  | val_struct : typvar -> map field val -> val. 

Inductive trm : Type :=
  | trm_var : var -> trm
  | trm_val : val -> trm
  | trm_if : trm -> trm -> trm -> trm
  | trm_let : bind -> trm -> trm -> trm
  | trm_app : prim -> list trm -> trm.
  (*
  | trm_while : trm -> trm -> trm
  | trm_for : var -> trm -> trm -> trm -> trm
  *)

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

Global Instance Inhab_typdef_struct : Inhab typdef_struct.
Proof using. apply (Inhab_of_val \{}). Qed.

Hint Extern 1 (Inhab val) => apply Inhab_val.

Hint Extern 1 (Inhab typ) => apply Inhab_typ.

(* ---------------------------------------------------------------------- *)
(** Coercions *)

Coercion prim_binop : binop >-> prim.
Coercion val_prim : prim >-> val.
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
  v = val_error.

Definition is_uninitialized (v:val) :=
  v = val_uninitialized.

Definition is_val_bool (v:val) :=
  match v with
  | val_bool b => True
  | _ => False
  end.

Definition is_ptr (t:trm) :=
  match t with
  | trm_val (val_abstract_ptr l π) => True
  | _ => False
  end.

Definition is_int (t:trm) :=
  match t with
  | trm_val (val_int i) => True
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
  | redbinop_eq : forall v1 v2,
      ~ is_error v1 ->
      ~ is_error v2 ->
      redbinop binop_eq v1 v2 (val_bool (isTrue (v1 = v2))).


(* ---------------------------------------------------------------------- *)
(** Uninitialized values construction *)

Inductive uninitialized_val (C:typdefctx) : typ -> val -> Prop :=
  | uninitialized_val_bool : 
      uninitialized_val C typ_bool val_uninitialized
  | uninitialized_val_int :
      uninitialized_val C typ_int val_uninitialized
  | uninitialized_val_double :
      uninitialized_val C typ_double val_uninitialized
  | uninitialized_ptr : forall T,
      uninitialized_val C (typ_ptr T) val_uninitialized
  | uninitialized_val_array : forall T (n:nat) a,
      length a = n ->
      (forall i, 
        index a i -> 
        uninitialized_val C T a[i]) ->
      uninitialized_val C (typ_array T (Some n)) (val_array a)
  | uninitialized_val_struct : forall T Tfs vfs,
      Tfs = C[T] ->
      dom Tfs = dom vfs ->
      (forall f,
        index Tfs f ->
        uninitialized_val C Tfs[f] vfs[f]) ->
      uninitialized_val C (typ_struct T) (val_struct T vfs).


(* ---------------------------------------------------------------------- *)
(** Semantics of memory accesses *)

(** v[π] = w *)

Inductive read_accesses : val -> accesses -> val -> Prop :=
  | read_accesses_nil : forall v,
      read_accesses v nil v
  | read_accesses_array : forall a (i:Z) π v,
<<<<<<< HEAD
      index a i -> 
      read_accesses a[i] π v ->
      read_accesses (val_array a) ((access_array i)::π) v
  | read_accesses_struct : forall T s f π v,
      index s f ->
      read_accesses s[f] π v ->
      read_accesses (val_struct T s) ((access_field T f)::π) v.
=======
      read_accesses (a[i]) π v ->
      index a i -> 
      read_accesses (val_array a) ((access_array i)::π) v
  | read_accesses_struct : forall T s f π v2,
      f \indom s ->
      read_accesses s[f] π v2 ->
      read_accesses (val_struct T s) ((access_field T f)::π) v2.
>>>>>>> f6ec174ae2c9450028d227fe016852cd9d6933ff

(** m(l)[π] = v *)

Inductive read_state (m:state) (l:loc) (π:accesses) (v:val) : Prop :=
  | read_state_intro :
<<<<<<< HEAD
      index m l ->
=======
      l \indom m ->
>>>>>>> f6ec174ae2c9450028d227fe016852cd9d6933ff
      read_accesses m[l] π v ->
      read_state m l π v.

(** v[π := w] = v' *)

Inductive write_accesses : val -> accesses -> val -> val -> Prop :=
<<<<<<< HEAD
  | write_accesses_nil : forall v w,
      write_accesses v nil w w
  | write_accesses_array : forall v1 v2 a1 i π w a2,
      index a1 i -> 
      write_accesses a1[i] π w v2 ->
      a2 = update a1 i v2 ->
      write_accesses (val_array a1) ((access_array i)::π) w (val_array a2)
  | write_accesses_struct : forall T s1 s2 f π w v,
      index s1 f ->
      write_accesses s1[f] π w v ->
      s2 = s1[f := v] ->
=======
  | write_accesses_nil : forall v1 v2 w,
      v2 = w ->
      write_accesses v1 nil w v2
  | write_accesses_array : forall v1 v2 a1 (i:Z) π w a2,
      write_accesses (a1[i]) π w v2 ->
      index a1 i -> 
      a2 = update a1 (i:Z) v2 ->
      write_accesses (val_array a1) ((access_array i)::π) w (val_array a2)
  | write_accesses_struct : forall T s1 s2 f π w v2,
      f \indom s1 ->
      write_accesses s1[f] π w v2 ->
      s2 = s1[f := v2] ->
>>>>>>> f6ec174ae2c9450028d227fe016852cd9d6933ff
      write_accesses (val_struct T s1) ((access_field T f)::π) w (val_struct T s2).

(** m[l := m(l)[π := w]] = m' *)

Inductive write_state (m:state) (l:loc) (π:accesses) (w:val) (m':state) : Prop :=
<<<<<<< HEAD
  | write_mem_intro : forall v, 
      index m l ->
      write_accesses m[l] π w v ->
      m' = m[l := v] ->
=======
  | write_mem_intro : forall v2,
      l \indom m ->
      write_accesses m[l] π w v2 ->
      m' = m[l := v2] ->
>>>>>>> f6ec174ae2c9450028d227fe016852cd9d6933ff
      write_state m l π w m'.


(* ---------------------------------------------------------------------- *)
(** Big-step evaluation *)

(** <C, S, m, t> // <m', v> *)

Inductive red (C:typdefctx) : stack -> state -> trm -> state -> val -> Prop :=
  | red_var : forall S m v x,
      Ctx.lookup x S = Some v ->
      red C S m (trm_var x) m v
  | red_val : forall S m v,
      red C S m v m v
  | red_if : forall S m1 m2 m3 b r t0 t1 t2,
      red C S m1 t0 m2 (val_bool b) ->
      red C S m2 (if b then t1 else t2) m3 r ->
      red C S m1 (trm_if t0 t1 t2) m3 r
  | red_let : forall S m1 m2 m3 z t1 t2 v1 r,
      red C S m1 t1 m2 v1 ->
      ~ is_error v1 ->
      red C (Ctx.add z v1 S) m2 t2 m3 r ->
      red C S m1 (trm_let z t1 t2) m3 r
  (* Binary operations *)
  | red_binop : forall S (op:binop) m v1 v2 v,
      redbinop op v1 v2 v ->
      red C S m (trm_app op ((trm_val v1)::(trm_val v2)::nil)) m v
  (* Operations on the abstract heap *) 
  | red_get : forall l π S T (p:trm) m w,
      p = val_abstract_ptr l π ->
      read_state m l π w ->
      ~ is_uninitialized w ->
      red C S m (trm_app (prim_get T) (p::nil)) m w
  | red_set : forall (v:val) l π  S m1 T (p:trm) (t:trm) m2,
      p = val_abstract_ptr l π ->
      t = trm_val v ->
      ~ is_error v ->
      write_state m1 l π v m2 ->
      red C S m1 (trm_app (prim_set T) (p::t::nil)) m2 val_unit
  | red_new : forall l v S m1 T m2 l,
      l <> null ->
      l \notindom m1 ->
      uninitialized_val C T v -> 
      m2 = m1[l := v] ->
      red C S m1 (trm_app (prim_new T) nil) m2 (val_abstract_ptr l nil)
  | red_new_array : forall l (v:val) S m1 T m2 l (n:int) (k:nat),
      l <> null ->
      l \notindom m1 ->
      n = k ->
      uninitialized_val C (typ_array T (Some k)) v -> 
      m2 = m1[l := v] ->
      red C S m1 (trm_app (prim_new_array T) ((trm_val (val_int n))::nil)) m2 (val_abstract_ptr l nil)
  | red_struct_access : forall S m t l f π T v vr,
      t = val_abstract_ptr l π ->
      vr = val_abstract_ptr l (π++((access_field T f)::nil)) ->
      red C S m (trm_app (prim_struct_access T f) (t::nil)) m vr
  | red_array_access : forall S m t l i π T vr ti,
      t = val_abstract_ptr l π ->
      ti = trm_val (val_int i) ->
      vr = val_abstract_ptr l (π++(access_array i)::nil) ->
      red C S m (trm_app (prim_array_access T) (t::ti::nil)) m vr
  (* Arguments *) 
  | red_args_1 : forall v1 m2 S m1 op t1 m3 v2 ts,
      ~ is_val t1 ->
      red C S m1 t1 m2 v1 ->
      red C S m2 (trm_app op ((trm_val v1)::ts)) m3 v2 ->
      red C S m1 (trm_app op (t1::ts)) m3 v2
  | red_args_2 : forall m2 v2 S m1 op v1 t2 m3 v3 ts,
      ~ is_val t2 ->
      red C S m1 t2 m2 v2 ->
      red C S m2 (trm_app op ((trm_val v1)::(trm_val v2)::ts)) m3 v3 ->
      red C S m1 (trm_app op ((trm_val v1)::t2::ts)) m3 v3

  (* Error cases *)
  | red_var_error :  forall S m x,
      Ctx.lookup x S = None ->
      red C S m (trm_var x) m val_error
  | red_if_error_not_a_bool : forall S m1 m2 t0 t1 t2 v0,
      red C S m1 t0 m2 v0 ->
      ~ is_val_bool v0 ->
      red C S m1 (trm_if t0 t1 t2) m2 val_error
  | red_let_error_let : forall S m1 m2 z t1 t2 v1,
      red C S m1 t1 m2 val_error ->
      red C S m1 (trm_let z t1 t2) m2 val_error
  | red_binop_error : forall S (op:binop) m v1 v2,
      ~ (exists v, redbinop op v1 v2 v) ->
      red C S m (trm_app op ((trm_val v1)::(trm_val v2)::nil)) m val_error
  | red_get_error_not_a_ptr : forall S T (p:trm) m,
      ~ is_ptr p ->
      red C S m (trm_app (prim_get T) (p::nil)) m val_error
  | red_get_error_bad_address : forall l π S T (p:trm) m,
      p = val_abstract_ptr l π ->
      ~ (exists w, read_state m l π w) ->
      red C S m (trm_app (prim_get T) (p::nil)) m val_error
  | red_set_error_not_a_ptr : forall S m T (p:trm) (t:trm),
      ~ is_ptr p ->
      red C S m (trm_app (prim_set T) (p::t::nil)) m val_error
  | red_set_error_bad_address : forall (v:val) l π  S m T (p:trm) (t:trm),
      p = val_abstract_ptr l π ->
      t = trm_val v ->
      ~ (exists m', write_state m l π v m') ->
      red C S m (trm_app (prim_set T) (p::t::nil)) m val_error
  | red_new_error : forall l (v:val) S m1 T m2 l,
      ~ (exists v, uninitialized_val C T v) ->
      red C S m1 (trm_app (prim_new T) (nil)) m2 val_error
  | red_new_array_error : forall l (v:val) S m1 T m2 l (k:nat) (n:int),
      n = k ->
      ~ (exists v, uninitialized_val C (typ_array T (Some k)) v) -> 
      red C S m1 (trm_app (prim_new_array T) ((trm_val (val_int n))::nil)) m2 val_error
  | red_struct_access_error_not_a_ptr : forall S m t f T,
      ~ is_ptr t ->
      red C S m (trm_app (prim_struct_access T f) (t::nil)) m val_error
  | red_array_access_error_not_a_ptr : forall S m t T ti,
      ~ is_ptr t ->
      red C S m (trm_app (prim_array_access T) (t::ti::nil)) m val_error
  | red_array_access_error_not_an_int : forall S m t T ti,
      ~ is_int ti ->
      red C S m (trm_app (prim_array_access T) (t::ti::nil)) m val_error
  | red_args_1_error : forall v1 m2 S m1 op t1 v2 ts,
      red C S m1 t1 m2 val_error ->
      red C S m1 (trm_app op (t1::ts)) m2 val_error
  | red_args_2_error : forall m2 v2 S m1 op v1 t2 v3 ts,
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
<<<<<<< HEAD
  introv H. induction H; try subst; constructors*.
  { applys* index_update. } 
  { rewrite* LibListZ.read_update_same. } 
  { applys* index_update_same. }
  { rewrite* read_update_same. }
=======
  introv H. induction H; subst.
  { constructors*. }
  { constructors*. rew_reads~. }
  { constructors*. rew_reads~. }
>>>>>>> f6ec174ae2c9450028d227fe016852cd9d6933ff
Qed.

Hint Extern 1 (?j \in dom (?m[?i:=?v])) => applys @indom_update.


Lemma read_write_state_same : forall m m' l π w,
  write_state m l π w m' ->
  read_state m' l π w.
Proof.
<<<<<<< HEAD
  introv H. induction H. constructors*.
  { subst m'. applys* index_update_same. }
  { subst m'. applys* read_write_accesses_same.
    rewrite* read_update_same. }
=======
  introv H. induction H. subst. constructors*.
  { applys* read_write_accesses_same. rew_reads*. }
>>>>>>> f6ec174ae2c9450028d227fe016852cd9d6933ff
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
