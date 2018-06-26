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
  LibMonoid LibSet LibContainer LibList LibMap.

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

Check map.

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
  | access_array : nat -> access
  | access_field : field -> access.

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
  | prim_struct_access : typ -> field -> prim
  | prim_array_access : typ -> prim.

(** TODO: Change this! Probably use Flocq? *)
Definition double := int.

Check update.

Inductive val : Type :=
  | val_unit : val
  | val_bool : bool -> val
  | val_int : int -> val
  | val_double : double -> val
  | val_abstract_ptr : loc -> accesses -> val
  | val_concrete_ptr : loc -> offset -> val
  | val_prim : prim -> val
  | val_array : list val -> val
  | val_struct : map field val -> val.

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

Definition is_not_val (t:trm) :=
  match t with
  | trm_val v => False
  | _ => True
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
  | read_accesses_array : forall v1 a i π v2,
      Nth i a v1 ->
      read_accesses v1 π v2 ->
      read_accesses (val_array a) ((access_array i)::π) v2
  | read_accesses_struct : forall v1 s f π v2,
      binds s f v1 ->
      read_accesses v1 π v2 ->
      read_accesses (val_struct s) ((access_field f)::π) v2.

(** m(l)[π] = v *)
Inductive read_state (m:state) (l:loc) (π:accesses) (v:val) : Prop :=
  | read_state_intro : forall v1, 
      binds m l v1 ->
      read_accesses v1 π v ->
      read_state m l π v.

Open Scope liblist_scope.

(** update. I guess this shouldn't be here... *)
Fixpoint update A (n:nat) (v:A) (l:list A) { struct l } : list A :=
  match l with
  | nil => nil
  | x::l' =>
     match n with
     | 0 => v::l'
     | S n' => x::update n' v l'
     end
  end.

(** v[π := w] = v' *)
Inductive write_accesses : val -> accesses -> val -> val -> Prop :=
  | write_accesses_nil : forall v w,
      write_accesses v nil w w
  | write_accesses_array : forall v1 v2 a1 i π w a2,
      Nth i a1 v1 ->
      write_accesses v1 π w v2 ->
      a2 = update i v2 a1 ->
      write_accesses (val_array a1) ((access_array i)::π) w (val_array a2)
  | write_accesses_struct : forall v1 s1 s2 f π w v2,
      binds s1 f v1 ->
      write_accesses v1 π w v2 ->
      s2 = s1[f := v2] ->
      write_accesses (val_struct s1) ((access_field f)::π) w (val_struct s2).

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
  introv H. induction H; constructors*.
  (** Write a lemma Nth_update in LibList? *)
  { applys* Nth_of_nth; apply Nth_inbound in H; subst a2. 
    { applys* nth_update_same. } 
    { rewrite* length_update. } } 
  { subst s2. applys* binds_update_same. }
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
      write_state m1 l π v m2 ->
      red S m1 (trm_app (prim_set T) (p::t::nil)) m2 val_unit
  | red_new : forall l (v:val) S m1 T (t:trm) m2 l,
      t = v ->
      l <> null ->
      l \notindom m1 ->
      m2 = m1[l := v] ->
      red S m1 (trm_app (prim_new T) (t::nil)) m2 (val_abstract_ptr l nil)
  | red_struct_access : forall S m t l s f π T v vr,
      t = val_abstract_ptr l π ->
      binds m l (val_struct s) ->
      binds s f v ->
      vr = val_abstract_ptr l (π++((access_field f)::nil)) ->
      red S m (trm_app (prim_struct_access T f) (t::nil)) m vr
  | red_array_access : forall S m t l i π T vr ti (k:nat),
      t = val_abstract_ptr l π ->
      ti = trm_val (val_int i) ->
      i = k ->
      vr = val_abstract_ptr l (π++(access_array k)::nil) ->
      red S m (trm_app (prim_array_access T) (t::ti::nil)) m vr
  (* Arguments *) 
  | red_args_one : forall v1 m2 S m1 op t1 m3 v2,
      is_not_val t1 ->
      red S m1 t1 m2 v1 ->
      red S m2 (trm_app op ((trm_val v1)::nil)) m3 v2 ->
      red S m1 (trm_app op (t1::nil)) m3 v2
  | red_args_two_fst : forall v1 m2 S m1 op t1 t2 m3 v3,
      is_not_val t1 ->
      red S m1 t1 m2 v1 ->
      red S m2 (trm_app op ((trm_val v1)::t2::nil)) m3 v3 ->
      red S m1 (trm_app op (t1::t2::nil)) m3 v3
  | red_args_two_snd : forall m2 v2 S m1 op v1 t2 m3 v3,
      is_not_val t2 ->
      red S m1 t2 m2 v2 ->
      red S m2 (trm_app op ((trm_val v1)::(trm_val v2)::nil)) m3 v3 ->
      red S m1 (trm_app op ((trm_val v1)::t2::nil)) m3 v3.
  (*| red_args_1 : forall v1 m2 S m1 op t1 ts m3 v2,
      is_not_val t1 ->
      red S m1 t1 m2 v1 ->
      red S m2 (trm_app op ((trm_val v1)::ts)) m3 v2 ->
      red S m1 (trm_app op (t1::ts)) m3 v2
  | red_args_2 : forall m2 v2 S m1 op v1 t2 ts m3 v3,
      is_not_val t2 ->
      red S m1 t2 m2 v2 ->
      red S m2 (trm_app op ((trm_val v1)::(trm_val v2)::ts)) m3 v3 ->
      red S m1 (trm_app op ((trm_val v1)::t2::ts)) m3 v3.*)

End Red.

(* Derived *)

Lemma red_seq : forall S m1 m2 m3 t1 t2 r1 r,
  red S m1 t1 m2 r1 ->
  red S m2 t2 m3 r ->
  red S m1 (trm_seq t1 t2) m3 r.
Proof using. intros. applys* red_let. Qed.


(* ---------------------------------------------------------------------- *)
(** Type inference rules *)

Definition gamma := Ctx.ctx typ.
Definition phi := map loc typ.

(** T[π] = T1 *)
Inductive follow_typ (C:typdefctx) : typ -> accesses -> typ -> Prop :=
  | follow_typ_nil : forall T,
      follow_typ C T nil T
  | follow_typ_array : forall T' π' T1 i n,
      follow_typ C T' π' T1 ->
      (0 <= i < n)%nat ->
      follow_typ C (typ_array T' n) ((access_array i)::π') T1
  | follow_typ_struct : forall S m f T' π' T1,
      binds C S m ->
      binds m f T' ->
      follow_typ C T' π' T1 ->
      follow_typ C (typ_struct S) ((access_field f)::π') T1.

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
  | typing_val_struct : forall C φ mt mv s T,
      binds C s mt ->
      dom mt = dom mv ->
      (forall f v, binds mt f T -> 
          binds mv f v ->
          typing_val C φ v T) ->
      typing_val C φ (val_struct mv) (typ_struct s)
  | typing_val_array : forall C φ a T,
      (forall i v, Nth i a v -> 
        typing_val C φ v T) -> 
      typing_val C φ (val_array a) (typ_array T (List.length a))
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
  | typing_struct_access : forall E s Fs f T T1 t,
      binds (env_typdefctx E) s Fs ->
      binds Fs f T1 ->
      T = typ_struct s ->
      typing E t (typ_ptr T) ->
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

Check Some.

(* Lemma for stack_typing_ctx_add *)
Lemma ctx_lookup_add {A:Type} :
  forall C (z1 z2:var) (w1 w2:A),
    Ctx.lookup z1 ((z2, w1) :: C) = Some w2 ->
        (z1 = z2 /\ w1 = w2)
    \/  (z1 <> z2 /\ Ctx.lookup z1 C = Some w2).
Proof.
  introv H. unfold Ctx.lookup in H.
  destruct (var_eq z1 z2) eqn:HE.
  { left. rewrite var_eq_spec in HE.  
    rewrite isTrue_eq_true_eq in HE.
    inverts H. splits*. }
  { right. folds Ctx.lookup.
    rewrite var_eq_spec in HE.  
    rewrite isTrue_eq_false_eq in HE.
    splits*. }
Qed.

(* Lemma for let case *)
Lemma stack_typing_ctx_add :
  forall C φ z T Γ v S,
    stack_typing C φ Γ S ->
    typing_val C φ v T ->
    stack_typing C φ (Ctx.add z T Γ) (Ctx.add z v S).
Proof.
  introv HS HT. unfolds* stack_typing. introv HS1 HT1.
  unfolds* Ctx.add. destruct z.
  { forwards*: HS. }
  { apply ctx_lookup_add in HS1. 
    apply ctx_lookup_add in HT1.
    inverts* HS1; inverts* HT1.
    destruct H. destruct H0. 
    subst v. subst T. auto. }
Qed.

(* Lemma for get case *)
Lemma typing_val_get :
  forall m l π C φ w T,
    state_typing C φ m ->
    read_state m l π w ->
    read_phi C φ l π T ->
    typing_val C φ w T.
Proof.
  (*introv HT HS HP. induction HS. unfolds state_typing. destruct HT as (HT1&HT2).
  inverts HP. apply HT2 in H1. inverts H0.
  {  }
  inverts HP. apply HT2 in H. inverts HS. inverts H2.
  { inverts H0. }*)
Admitted.

(* Lemma for uniqueness of bind *)
(* binds m l w -> forall v, binds m l v -> v = w.*)

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
    inverts HT. forwards* (HT1&HM1): IHR1. forwards* (HT2&HM2): IHR2. case_if*. }
  { (* let *) 
    inverts HT. forwards* (HT1&HM1): IHR1. forwards* (HT2&HM2): IHR2.
    applys* stack_typing_ctx_add. }
  { (* binop *) 
    inverts HT; splits*; inverts H; constructors*. }
  { (* get *) 
    splits*. inverts HT. subst p. inverts H5. simpls. inverts H2.
    applys* typing_val_get.  }
  { (* set *) admit. }
  { (* new *) admit. }
  { (* struct access *) admit. }
  { (* array_access *) admit. }
  { (* app 1 *) admit. }
  { (* app 2 fst *) admit. }
  { (* app 2 snd *) admit. }
Qed.

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
