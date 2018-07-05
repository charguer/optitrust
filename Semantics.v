(**

This file describes the syntax and semantics of a lambda calculus
with mutable heap. The language includes recursive functions, and a
couple of primitive functions. Records and arrays operations are
encoded using pointer arithmetics, and using the [alloc] operation
which allocates at once a requested number of consecutive memory cells.

Author: Arthur Charguéraud.
License: MIT.

*)

Set Implicit Arguments.
Require Export Bind TLCbuffer.
Require Export LibString LibCore LibLogic LibReflect 
  LibOption LibRelation LibLogic LibOperation LibEpsilon 
  LibMonoid LibSet LibContainer LibList LibListZ LibMap.

Local Open Scope set_scope.

(* ********************************************************************** *)
(* * Source language syntax *)


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
  | typ_array : typ -> size -> typ
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
  | typ_array T' n => (n * (sizeof S T'))%nat
  | typ_struct X => S[X]
  | typ_fun _ _ => 1%nat
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
  | prim_struct_access : typvar -> field -> prim
  | prim_array_access : typ -> prim.

(** TODO: Change this! Probably use Flocq? *)
Definition double := int.

Inductive val : Type :=
  | val_error : val
  | val_unit : val
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

(** The type of values is inhabited *)

Global Instance Inhab_val : Inhab val.
Proof using. apply (Inhab_of_val val_unit). Qed.

Global Instance Inhab_typ : Inhab typ.
Proof using. apply (Inhab_of_val typ_unit). Qed.

Global Instance Inhab_typdef_struct : Inhab typdef_struct.
Proof using. apply (Inhab_of_val \{}). Qed.

(** Shorthand [vars], [vals] and [trms] for lists of items. *)

Definition vals : Type := list val.
Definition trms : Type := list trm.


(* ---------------------------------------------------------------------- *)
(** Coercions *)

Coercion prim_binop : binop >-> prim.
Coercion val_prim : prim >-> val.
Coercion val_int : Z >-> val.
Coercion trm_val : val >-> trm.
Coercion trm_var : var >-> trm.
Coercion trm_app : prim >-> Funclass.


(* ********************************************************************** *)
(* * Source language semantics *)

(* ---------------------------------------------------------------------- *)
(** Big-step evaluation *)

Implicit Types t : trm.
Implicit Types v : val.
Implicit Types l : loc.
Implicit Types b : bool.
Implicit Types x : var.
Implicit Types z : bind.
Implicit Types vs : vals.
Implicit Types ts : trms.

Definition state := map loc val.

Definition stack := Ctx.ctx val.

Section Red.

Definition is_val (t:trm) :=
  match t with
  | trm_val v => True
  | _ => False
  end.

Definition is_error (v:val) :=
  v = val_error.

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

Inductive redbinop : binop -> val -> val -> val -> Prop :=
  | redbinop_add : forall n1 n2,
      redbinop binop_add (val_int n1) (val_int n2) (val_int (n1 + n2))
  | redbinop_sub : forall n1 n2,
      redbinop binop_sub (val_int n1) (val_int n2) (val_int (n1 - n2))
  | redbinop_eq : forall v1 v2,
      redbinop binop_eq v1 v2 (val_bool (isTrue (v1 = v2))).

Open Scope nat_scope.
Open Scope list_scope.

(** v[π] = w *)
Inductive read_accesses : val -> accesses -> val -> Prop :=
  | read_accesses_nil : forall v,
      read_accesses v nil v
  | read_accesses_array : forall v1 a (i:Z) π v2,
      index a i -> 
      read_accesses (a[i]) π v2 ->
      read_accesses (val_array a) ((access_array i)::π) v2
  | read_accesses_struct : forall T v1 s f π v2,
      binds s f v1 ->
      read_accesses v1 π v2 ->
      read_accesses (val_struct T s) ((access_field T f)::π) v2.

(** m(l)[π] = v *)
Inductive read_state (m:state) (l:loc) (π:accesses) (v:val) : Prop :=
  | read_state_intro : forall v1, 
      binds m l v1 ->
      read_accesses v1 π v ->
      read_state m l π v.

(** v[π := w] = v' *)
Inductive write_accesses : val -> accesses -> val -> val -> Prop :=
  | write_accesses_nil : forall v1 v2 w,
      v2 = w ->
      write_accesses v1 nil w v2
  | write_accesses_array : forall v1 v2 a1 (i:Z) π w a2,
      index a1 i -> 
      write_accesses (a1[i]) π w v2 ->
      a2 = update a1 (i:Z) v2 ->
      write_accesses (val_array a1) ((access_array i)::π) w (val_array a2)
  | write_accesses_struct : forall T v1 s1 s2 f π w v2,
      binds s1 f v1 ->
      write_accesses v1 π w v2 ->
      s2 = s1[f := v2] ->
      write_accesses (val_struct T s1) ((access_field T f)::π) w (val_struct T s2).

(** m[l := m(l)[π := w]] = m' *)
Inductive write_state (m:state) (l:loc) (π:accesses) (w:val) (m':state) : Prop :=
  | write_mem_intro : forall v1 v2, 
      binds m l v1 ->
      write_accesses v1 π w v2 ->
      m' = m[l := v2] ->
      write_state m l π w m'.

(** Some lemmas *)

(** We need the paths to be disjoint, not just different. *)
(*Lemma read_write_accesses_neq : forall v1 v2 π π' w1 w2,
  read_accesses v1 π w1 ->
  write_accesses v1 π' w2 v2 ->
  p <> p' ->
  read_accesses v2 π w1.
Proof.
Admitted.

Lemma read_write_state_neq : forall m l π w m' l' π' v,
  read_state m l' π' v ->
  write_state m l π w m' ->
  (l <> l' \/ π <> π') ->
  read_state m' l' π' v.
Proof.
  introv Hr Hw Hneq. gen m l π.
Admitted. *)

Lemma read_write_accesses_same : forall v1 v2 π w,
  write_accesses v1 π w v2 ->
  read_accesses v2 π w.
Proof.
  introv H. induction H; try subst; constructors*.
  { applys* index_update. } 
  { rewrite* LibListZ.read_update_same. } 
  { applys* binds_update_same. }
Qed.

Lemma read_write_state_same : forall m m' l π w,
  write_state m l π w m' ->
  read_state m' l π w.
Proof.
  introv H. induction H. constructors*.
  { subst m'. applys* binds_update_same. }
  { applys* read_write_accesses_same. }
Qed.

(** <S, m, t> // <m', v> *)
Inductive red : stack -> state -> trm -> state -> val -> Prop :=
  | red_var : forall S m v x,
      Ctx.lookup x S = Some v ->
      red S m (trm_var x) m v
  | red_val : forall S m v,
      red S m v m v
  | red_if : forall S m1 m2 m3 b r t0 t1 t2,
      red S m1 t0 m2 (val_bool b) ->
      red S m2 (if b then t1 else t2) m3 r ->
      red S m1 (trm_if t0 t1 t2) m3 r
  | red_let : forall S m1 m2 m3 z t1 t2 v1 r,
      red S m1 t1 m2 v1 ->
      ~ is_error v1 ->
      red (Ctx.add z v1 S) m2 t2 m3 r ->
      red S m1 (trm_let z t1 t2) m3 r
  (* Binary operations *)
  | red_binop : forall S (op:binop) m v1 v2 v,
      redbinop op v1 v2 v ->
      red S m (trm_app op ((trm_val v1)::(trm_val v2)::nil)) m v
  (* Operations on the abstract heap *) 
  | red_get : forall l π S T (p:trm) m w,
      p = val_abstract_ptr l π ->
      read_state m l π w ->
      red S m (trm_app (prim_get T) (p::nil)) m w
  | red_set : forall (v:val) l π  S m1 T (p:trm) (t:trm) m2,
      p = val_abstract_ptr l π ->
      t = trm_val v ->
      ~ is_error v ->
      write_state m1 l π v m2 ->
      red S m1 (trm_app (prim_set T) (p::t::nil)) m2 val_unit
  | red_new : forall l (v:val) S m1 T m2 l,
      l <> null ->
      l \notindom m1 ->
      m2 = m1[l := v] ->
      red S m1 (trm_app (prim_new T) ((trm_val v)::nil)) m2 (val_abstract_ptr l nil)
  | red_struct_access : forall S m t l f π T v vr,
      t = val_abstract_ptr l π ->
      vr = val_abstract_ptr l (π++((access_field T f)::nil)) ->
      red S m (trm_app (prim_struct_access T f) (t::nil)) m vr
  | red_array_access : forall S m t l i π T vr ti,
      t = val_abstract_ptr l π ->
      ti = trm_val (val_int i) ->
      vr = val_abstract_ptr l (π++(access_array i)::nil) ->
      red S m (trm_app (prim_array_access T) (t::ti::nil)) m vr
  (* Arguments *) 
  | red_args_1 : forall v1 m2 S m1 op t1 m3 v2 ts,
      ~ is_val t1 ->
      red S m1 t1 m2 v1 ->
      red S m2 (trm_app op ((trm_val v1)::ts)) m3 v2 ->
      red S m1 (trm_app op (t1::ts)) m3 v2
  | red_args_2 : forall m2 v2 S m1 op v1 t2 m3 v3 ts,
      ~ is_val t2 ->
      red S m1 t2 m2 v2 ->
      red S m2 (trm_app op ((trm_val v1)::(trm_val v2)::ts)) m3 v3 ->
      red S m1 (trm_app op ((trm_val v1)::t2::ts)) m3 v3
  (* Error cases *)
  | red_var_error :  forall S m x,
      Ctx.lookup x S = None ->
      red S m (trm_var x) m val_error
  | red_if_error_not_a_bool : forall S m1 m2 t0 t1 t2 v0,
      red S m1 t0 m2 v0 ->
      ~ is_val_bool v0 ->
      red S m1 (trm_if t0 t1 t2) m2 val_error
  | red_let_error_let : forall S m1 m2 z t1 t2 v1,
      red S m1 t1 m2 val_error ->
      red S m1 (trm_let z t1 t2) m2 val_error
  | red_binop_error : forall S (op:binop) m v1 v2,
      ~ (exists v, redbinop op v1 v2 v) ->
      red S m (trm_app op ((trm_val v1)::(trm_val v2)::nil)) m val_error
  | red_get_error_not_a_ptr : forall S T (p:trm) m,
      ~ is_ptr p ->
      red S m (trm_app (prim_get T) (p::nil)) m val_error
  | red_get_error_bad_address : forall l π S T (p:trm) m,
      p = val_abstract_ptr l π ->
      ~ (exists w, read_state m l π w) ->
      red S m (trm_app (prim_get T) (p::nil)) m val_error
  | red_set_error_not_a_ptr : forall S m T (p:trm) (t:trm),
      ~ is_ptr p ->
      red S m (trm_app (prim_set T) (p::t::nil)) m val_error
  | red_set_error_bad_address : forall (v:val) l π  S m T (p:trm) (t:trm),
      p = val_abstract_ptr l π ->
      t = trm_val v ->
      ~ (exists m', write_state m l π v m') ->
      red S m (trm_app (prim_set T) (p::t::nil)) m val_error
  | red_struct_access_error_not_a_ptr : forall S m t f T,
      ~ is_ptr t ->
      red S m (trm_app (prim_struct_access T f) (t::nil)) m val_error
  | red_array_access_error_not_a_ptr : forall S m t T ti,
      ~ is_ptr t ->
      red S m (trm_app (prim_array_access T) (t::ti::nil)) m val_error
  | red_array_access_error_not_an_int : forall S m t T ti,
      ~ is_int ti ->
      red S m (trm_app (prim_array_access T) (t::ti::nil)) m val_error
  | red_args_1_error : forall v1 m2 S m1 op t1 v2 ts,
      red S m1 t1 m2 val_error ->
      red S m1 (trm_app op (t1::ts)) m2 val_error
  | red_args_2_error : forall m2 v2 S m1 op v1 t2 v3 ts,
      ~ is_error v1 ->
      red S m1 t2 m2 val_error ->
      red S m1 (trm_app op ((trm_val v1)::t2::ts)) m2 val_error.

End Red.

(* Derived *)

Lemma red_seq : forall S m1 m2 m3 t1 t2 r1 r,
  red S m1 t1 m2 r1 ->
  ~ is_error r1 ->
  red S m2 t2 m3 r ->
  ~ is_error r -> 
  red S m1 (trm_seq t1 t2) m3 r.
Proof using. intros. applys* red_let. Qed.


(* ---------------------------------------------------------------------- *)
(** Type inference rules *)

Definition gamma := Ctx.ctx typ.
Definition phi := map loc typ.

Definition typing_field (C:typdefctx) (S:typvar) (f:field) (T:typ) : Prop :=
  exists s, binds C S s /\ binds s f T.

(** T[π] = T1 *)
Inductive follow_typ (C:typdefctx) : typ -> accesses -> typ -> Prop :=
  | follow_typ_nil : forall T,
      follow_typ C T nil T
  | follow_typ_array : forall T π Tr i n,
      follow_typ C T π Tr ->
      (0 <= i < n)%nat ->
      follow_typ C (typ_array T n) ((access_array i)::π) Tr
  | follow_typ_struct : forall T f Tf π Tr,
      typing_field C T f Tf ->
      follow_typ C Tf π Tr ->
      follow_typ C (typ_struct T) ((access_field T f)::π) Tr.

(** φ(l)..π = T *)
Inductive read_phi (C:typdefctx) (φ:phi) (l:loc) (π:accesses) (T:typ) : Prop :=
  | read_phi_intro : forall T1, 
      binds φ l T1 -> 
      follow_typ C T1 π T -> 
      read_phi C φ l π T.

Record env := make_env { 
  env_typdefctx : typdefctx;
  env_phi : phi;
  env_gamma : gamma
}.

Definition env_add_binding E z X :=
  match E with
  | make_env C φ Γ => make_env C φ (Ctx.add z X Γ)
  end. 

Notation "'make_env''" := make_env.

(* c and phi *)
Inductive typing_val : typdefctx -> phi -> val -> typ -> Prop :=
  | typing_val_unit : forall C φ,
      typing_val C φ val_unit typ_unit
  | typing_val_bool : forall C φ b,
      typing_val C φ (val_bool b) typ_bool
  | typing_val_int : forall C φ i,
      typing_val C φ (val_int i) typ_int
  | typing_val_double : forall C φ d,
      typing_val C φ (val_double d) typ_double
  | typing_val_struct : forall C φ mt mv T,
      binds C T mt ->
      dom mt = dom mv ->
      (forall f v Tv, 
          binds mt f Tv -> 
          binds mv f v ->
          typing_val C φ v Tv) ->
      typing_val C φ (val_struct T mv) (typ_struct T)
  | typing_val_array : forall C φ a T (n:nat),
      (forall i, index a i -> typing_val C φ a[i] T) -> 
      length a = n ->
      typing_val C φ (val_array a) (typ_array T n)
  | typing_val_abstract_ptr : forall C φ l π T,
      read_phi C φ l π T ->
      typing_val C φ (val_abstract_ptr l π) (typ_ptr T).

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
  | typing_binop_add : forall E v1 v2,
      typing E v1 typ_int ->
      typing E v2 typ_int ->
      typing E (trm_app binop_add ((trm_val v1)::(trm_val v2)::nil)) typ_int
  | typing_binop_sub : forall E v1 v2,
      typing E v1 typ_int ->
      typing E v2 typ_int ->
      typing E (trm_app binop_sub ((trm_val v1)::(trm_val v2)::nil)) typ_int
  | typing_binop_eq : forall E v1 v2,
      typing E v1 typ_int ->
      typing E v2 typ_int ->
      typing E (trm_app binop_eq ((trm_val v1)::(trm_val v2)::nil)) typ_bool
  (* Abstract heap operations *)
  | typing_get : forall E T p,
      typing E p (typ_ptr T) ->
      typing E (trm_app (prim_get T) (p::nil)) T
  | typing_set : forall E p t T,
      typing E p (typ_ptr T) ->
      typing E t T ->
      typing E (trm_app (prim_set T) (p::t::nil)) typ_unit
  | typing_new : forall E t T l, 
      typing E t T ->
      typing E (trm_app (prim_new T) (t::nil)) (typ_ptr T)
  | typing_struct_access : forall E Fs f T T1 t,
      binds (env_typdefctx E) T Fs ->
      binds Fs f T1 ->
      typing E t (typ_ptr (typ_struct T)) ->
      typing E (trm_app (prim_struct_access T f) (t::nil)) (typ_ptr T1)
  | typing_array_access : forall E t A i n,
      typing E t (typ_ptr (typ_array A n)) ->
      typing E i typ_int ->
      typing E (trm_app (prim_array_access A) (t::i::nil)) (typ_ptr A)
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

(* Lemma : fmap_dom m \c fmap_dom φ <-> (forall l, fmap_indom m l -> fmap_indom φ l) *)

Open Scope container_scope.

Definition state_typing (C:typdefctx) (φ:phi) (m:state) : Prop :=
      dom φ \c dom m
  /\  (forall l T, binds φ l T ->
         exists v, binds m l v
               /\  typing_val C φ v T).

Definition stack_typing (C:typdefctx) (φ:phi) (Γ:gamma) (S:stack) : Prop := 
  forall x v T,
    Ctx.lookup x S = Some v ->
    Ctx.lookup x Γ = Some T ->
    typing_val C φ v T.

(* Lemma for stack_typing_ctx_add *)
Lemma ctx_lookup_add_inv {A:Type} : forall C (z1 z2:var) (w1 w2:A),
  Ctx.lookup z1 (Ctx.add z2 w1 C) = Some w2 ->
      (z1 = z2 /\ w1 = w2)
  \/  (z1 <> z2 /\ Ctx.lookup z1 C = Some w2).
Proof.
  introv H. simpls. rewrite var_eq_spec in *. case_if*. { inverts* H. }
Qed.

(* Lemma for let case *)
Lemma stack_typing_ctx_add : forall C φ z T Γ v S,
  stack_typing C φ Γ S ->
  typing_val C φ v T ->
  stack_typing C φ (Ctx.add z T Γ) (Ctx.add z v S).
Proof.
  introv HS HT. unfolds* stack_typing. introv HS1 HT1.
  destruct z.
  { simpls. forwards*: HS. }
  { simpls. rewrite var_eq_spec in *. case_if.
    { inverts* HS1; inverts* HT1. }
    { forwards*: HS. } }
Qed.

Hint Constructors typing_val redbinop. 

(* DEPRECATED
Ltac binds_inj := 
  match goal with H1: binds ?m ?a ?b, H2: binds ?m ?a ?b' |- _=>
    let HTEMP := fresh in
    forwards HTEMP: binds_inj H2 H1; [typeclass | subst_hyp HTEMP; clear H2] end. 
*)

Ltac exploit_functional P P_functional := 
  match goal with H1: ?F ?x1, H2: ?F ?x2 |- _=>
  match get_head F with P =>
    let HTEMP := fresh in
    forwards HTEMP: P_functional H2 H1; [typeclass | subst_hyp HTEMP; clear H2] end end.


Ltac binds_inj := exploit_functional constr:(@binds) constr:(@binds_inj).
(* TODO: rename [inj] to [functional] everywhere, e.g. 

   Ltac binds_functional :=
      exploit_functional constr:(@binds) constr:(@binds_functional).
*)

(* Todo several functional, you can do:

   Ltac exploit_functional_all := 
      repeat binds_functional;
      repeat tr_val_functional;
      repeat tr_trm_functional.

*)

Lemma typing_field_inj : forall C S f T1 T2,
  typing_field C S f T1 ->
  typing_field C S f T2 ->
  T1 = T2.
Proof.
  introv H1 H2. inverts H1 as H1. inverts H2 as H2.
  inverts H1. inverts H2. binds_inj. binds_inj. auto.
Qed.

(* Lemma for typing_val_get *)
Lemma typing_val_follow : forall T1 w1 π C φ w2 T2,
  typing_val C φ w1 T1 ->
  follow_typ C T1 π T2 ->
  read_accesses w1 π w2 ->
  typing_val C φ w2 T2.
Proof.
  introv HT HF HR. gen π. induction HT; intros;
   try solve [ inverts HR; inverts HF; constructors* ].
  { inverts HF as; inverts HR as; try constructors*.
    introv HB1 HR HB2 HF. inverts HB2. inverts H3. binds_inj. eauto. (* IH *) }
  { inverts HF as; inverts HR as; try constructors*.
    introv HN1 HR HT Hi. eauto. (* IH *) }
Qed.

(* Lemma for get case *)
Lemma typing_val_get : forall m l π C φ w T,
  state_typing C φ m ->
  read_state m l π w ->
  read_phi C φ l π T ->
  typing_val C φ w T.
Proof.
  introv (HD&HT) HS HP. inverts HS as Hv1 HR.
  inverts HP as HT1 HF. forwards (v&Hv&Tv): (rm HT) HT1.
  binds_inj. applys* typing_val_follow T1 v1.
Qed.

(* Types are well-formed *)
Lemma follow_typ_inj : forall C T π T1 T2,
  follow_typ C T π T1 ->
  follow_typ C T π T2 ->
  T1 = T2.
Proof.
  introv HF1 HF2. induction HF1; inverts* HF2.
  { applys IHHF1. forwards: typing_field_inj H H3. subst*. }
Qed.

(* φ is well-formed *)
Lemma read_phi_inj : forall C φ l π T1 T2,
  read_phi C φ l π T1 ->
  read_phi C φ l π T2 ->
  T1 = T2.
Proof.
  introv H1 H2. inverts H1. inverts H2.
  binds_inj. applys* follow_typ_inj.
Qed.

Hint Extern 1 (Inhab val) => apply Inhab_val.

(* Lemma for state_typing_set *)
Lemma typing_val_after_write : forall v1 w π T2 C φ v2 T1,
  write_accesses v1 π w v2 ->
  typing_val C φ v1 T1 ->
  follow_typ C T1 π T2 ->
  typing_val C φ w T2 ->
  typing_val C φ v2 T1.
Proof.
  introv HW HT1 HF HT2. gen T1. induction HW; intros.
  { subst. inverts* HF. }
  { inverts HF. inverts HT1. subst a2. constructors. 
    { intros. rewrite index_update_eq in *. 
      rewrite* read_update_case. case_if*. }
    { rewrite* length_update. } }
  { inverts HF. inverts HT1. subst s2. constructors*.
    { unfold state. rewrite* dom_update_at_index. 
      applys* index_of_binds. }
    { intros f' v' Tv'. rewrite binds_update_eq. 
      tests Cf: (f = f'); case_if*. intros. 
      subst*. applys* IHHW. inverts H6 as H6. inverts H6.
      do 2 binds_inj. auto. } }
Qed.


(* Lemma for set case *)
Lemma state_typing_set : forall T m1 l π v C φ m2,
  state_typing C φ m1 ->
  write_state m1 l π v m2 ->
  typing_val C φ (val_abstract_ptr l π) (typ_ptr T) ->
  typing_val C φ v T ->
  state_typing C φ m2.
Proof.
  introv HS HW HTp HTv. 
  inverts HS as HD HT. 
  inverts HW as Hv1 HWA.
  inverts HTp as HP. 
  unfolds. split.
  { unfold state. rewrite* dom_update_at_index. applys* index_of_binds. }
  { intros l' T' HB'. forwards (v'&HB''&HT''): HT HB'. 
    tests Cl: (l' = l).
    { exists v2. rewrite binds_update_eq; case_if. split~.
      binds_inj. inverts HP. binds_inj. applys* typing_val_after_write. }
    { esplit.  rewrite binds_update_eq; case_if. split*. } }
Qed.

Theorem type_soundess_warmup : forall C φ m t v T Γ S m',
  red S m t m' v -> 
  typing (make_env C φ Γ) t T ->
  state_typing C φ m ->
  stack_typing C φ Γ S ->
        typing_val C φ v T
    /\  state_typing C φ m'.
Proof.
  introv R. gen φ T Γ. induction R; introv HT HM HS.
  { (* var *)
    inverts HT. simpls. split*. }
  { (* val *)  
    inverts HT. split*. }
  { (* if *) 
    inverts HT. forwards* (HT1&HM1): IHR1. forwards* (HT2&HM2): IHR2. 
    case_if*. }
  { (* let *) 
    inverts HT. forwards* (HT1&HM1): IHR1. forwards* (HT2&HM2): IHR2.
    applys* stack_typing_ctx_add. }
  { (* binop *) 
    rename H into R. inverts HT; inverts* R. }
  { (* get *) 
    splits*. 
    { subst. inverts HT as HT. inverts HT as HT; simpls.  
      inverts HT. applys* typing_val_get. } }
  { (* set *) 
    subst. inverts HT as HT1 HT2. splits*.
    { inverts HT1 as HT1. inverts HT2 as HT2. 
      applys* state_typing_set. } }
  { (* new *) 
    admit. }
  { (* struct_access *) 
    admit. }
  { (* array_access *) 
    admit. }
  { (* app 1 *) 
    admit. }
  { (* app 2 *) 
    admit. }
  { (* errors *) 
    (* In general, it will work more or less like this:
    false H. inverts HT as HT1 HT2;
    (inverts HT1 as HT; inverts HT);
    (inverts HT2 as HT; inverts HT); eauto.*)  admit. }
Admitted.

Definition extends (φ:phi) (φ':phi) :=
      dom φ \c dom φ'
  /\  forall l, l \indom φ -> φ' l = φ l.

Theorem type_soundess : forall C φ m t v T Γ S v m',
  typing (make_env C φ Γ) t T ->
  state_typing C φ m ->
  stack_typing C φ Γ S ->
  red S m t m' v -> 
  exists φ',
        extends φ φ'
    /\  typing_val C φ' v T
    /\  state_typing C φ' m'.
Proof.
Admitted.

(* ********************************************************************** *)
(* * Notation for terms *)

(* ---------------------------------------------------------------------- *)
(** Notation for concrete programs *)

Module NotationForTerms.

(** Note: below, many occurences of [x] have type [bind], and not [var] *)

Notation "'()" := val_unit : trm_scope.

Notation "'If_' t0 'Then' t1 'Else' t2" :=
  (trm_if t0 t1 t2)
  (at level 69, t0 at level 0) : trm_scope.

Notation "'If_' t0 'Then' t1 'End'" :=
  (trm_if t0 t1 val_unit)
  (at level 69, t0 at level 0) : trm_scope.

Notation "'Let' x ':=' t1 'in' t2" :=
  (trm_let x t1 t2)
  (at level 69, x at level 0, right associativity,
  format "'[v' '[' 'Let'  x  ':='  t1  'in' ']'  '/'  '[' t2 ']' ']'") : trm_scope.

Notation "t1 ;;; t2" :=
  (trm_seq t1 t2)
  (at level 68, right associativity, only parsing,
   format "'[v' '[' t1 ']'  ;;;  '/'  '[' t2 ']' ']'") : trm_scope.

End NotationForTerms.
