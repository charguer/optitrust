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
Require Import LibMonoid.
Require Export LibString LibList LibCore.
Require Export Fmap Bind TLCbuffer.
Open Scope string_scope.

Axiom fmap_get : forall A B, fmap A B -> A -> B.

(*Axiom fmap_fold : forall A B C, (monoid_op C) -> (A->B->C) -> fmap A B -> C. *)
Axiom fmap_fold : forall A B C, (monoid_op C) -> (A->B->C) -> fmap A B -> C.

Axiom fold_induction:
  forall A B C (m : monoid_op C) (f : A -> B -> C) (P : C -> Prop),
  Comm_monoid m ->
  P (monoid_neutral m) ->
  (forall x a b, P x -> P (monoid_oper m (f a b) x)) ->
  forall E,
  P (fmap_fold m f E).

Axiom fmap_indom : forall A B, fmap A B -> A -> Prop.

Axiom fmap_binds : forall A B, fmap A B -> A -> B -> Prop.

Axiom fmap_dom : forall A B, fmap A B -> A.

Axiom fmap_of_map : forall A B, Fmap.map A B -> fmap A B.

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

Definition typdef_struct : Type := fmap field typ.

Definition typdefctx := fmap typvar typdef_struct.


(* ---------------------------------------------------------------------- *)
(** Size of types *)

Definition typdefctx_size := fmap typvar size.

Definition typdefctx_offset := fmap typvar (fmap field offset).

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
  | typ_struct X => fmap_get S X
  | typ_fun _ _ => 1%nat
  end.

Definition sizeof_typdef_struct (S:typdefctx_size) (m:typdef_struct) : nat :=
  fmap_fold (monoid_make plus 0%nat) (fun f T => sizeof S T) m.

Definition wf_typctx_size (C:typdefctx) (S:typdefctx_size) : Prop :=
  forall X, fmap_indom C X -> 
     fmap_indom S X 
  /\ fmap_get S X = sizeof_typdef_struct S (fmap_get C X).

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

Inductive val : Type :=
  | val_unit : val
  | val_bool : bool -> val
  | val_int : int -> val
  | val_double : double -> val
  | val_abstract_ptr : loc -> accesses -> val
  | val_concrete_ptr : loc -> offset -> val
  | val_prim : prim -> val
  | val_array : list val -> val
  | val_struct : Fmap.map field val -> val.

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

Definition prog : Type := typdefctx * trm.


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

Coercion trms_vals (vs:vals) : trms :=
  List.map trm_val vs.


(* ---------------------------------------------------------------------- *)
(** Induction principle *)

(** An induction principle for trees *)

Section Trm_induct.

Variables
(P : trm -> Prop)
(Q : trms -> Prop)
(P1: forall v : var, P v)
(P2: forall v : val, P v)
(P3: forall t : trm, P t -> forall t0 : trm, P t0 -> forall t1 : trm, P t1 -> P (trm_if t t0 t1))
(P4: forall (b : bind) (t : trm), P t -> forall t0 : trm, P t0 -> P (trm_let b t t0))
(P5: forall (f : prim) (l : list trm), Q l -> P (trm_app f l))
(Q1: Q nil)
(Q2: forall t l, P t -> Q l -> Q (t::l)).

Fixpoint trm_induct_gen (t : trm) : P t :=
  let F := trm_induct_gen in
  match t as x return P x with
  | trm_var v => P1 v
  | trm_val v => P2 v
  | trm_if t0 t1 t2 => P3 (F t0) (F t1) (F t2)
  | trm_let b t0 t1 => P4 b (F t0) (F t1)
  | trm_app f l => P5 f 
      ((fix trms_induct (l : trms) : Q l :=
      match l as x return Q x with
      | nil   => Q1
      | t::l' => Q2 (F t) (trms_induct l')
      end) l)
  end.

End Trm_induct.

(* To use it:
  eapply tree_induct_gen with (Q := fun l =>
    forall t, Mem t l -> P t);
*)


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

Definition state := fmap loc val.

Definition stack := Ctx.ctx val.

Section Red.

Definition is_val t :=
  match t with
  | trm_val v => True
  | _ => False
  end.

Local Open Scope fmap_scope.

Inductive redbinop : binop -> val -> val -> val -> Prop :=
  | redbinop_add : forall n1 n2,
      redbinop binop_add (val_int n1) (val_int n2) (val_int (n1 + n2))
  | redbinop_sub : forall n1 n2,
      redbinop binop_sub (val_int n1) (val_int n2) (val_int (n1 - n2))
  | redbinop_eq : forall v1 v2,
      redbinop binop_eq v1 v2 (val_bool (isTrue (v1 = v2))).

Open Scope list_scope.

(** v'..π = v *)
Inductive follow : val -> accesses -> val -> Prop :=
  | follow_nil : forall v,
    follow v nil v
  | follow_array : forall v1 a i π v2,
    Nth i a v1 ->
    follow v1 π v2 ->
    follow (val_array a) ((access_array i)::π) v2
  | follow_struct : forall v1 s f π v2,
    fmap_binds s f v1 ->
    follow v1 π v2 ->
    follow (val_struct s) ((access_field f)::π) v2.

(** m(l)..π = v *)
Inductive read_mem (m:state) (l:loc) (π:accesses) (v:val) : Prop :=
  read_mem_intro : forall v1, 
  fmap_binds m l v1 ->
  follow v1 π v ->
  read_mem m l π v.

(** m' ~(l,π) m && m'(l).π = v *)
Definition updated_state (l:loc) (π:accesses) (v:val) (m:state) (m':state) : Prop :=
  forall l',
      (not (l = l') -> fmap_data m l' = fmap_data m' l') 
  /\  (l = l' ->  read_mem m' l π v).

(** <E, m, t> // <m', v> *)
Inductive red : stack -> state -> trm -> state -> val -> Prop :=
  | red_var : forall E m v x,
      Ctx.lookup x E = Some v ->
      red E m (trm_var x) m v
  | red_val : forall E m v,
      red E m v m v
  | red_if : forall E m1 m2 m3 b r t0 t1 t2,
      red E m1 t0 m2 (val_bool b) ->
      red E m2 (if b then t1 else t2) m3 r ->
      red E m1 (trm_if t0 t1 t2) m3 r
  | red_let : forall E m1 m2 m3 z t1 t2 v1 r,
      red E m1 t1 m2 v1 ->
      red (Ctx.add z v1 E) m2 t2 m3 r ->
      red E m1 (trm_let z t1 t2) m3 r
  (* Binary operations *)
  | red_binop : forall E (op:binop) m v1 v2 v,
      redbinop op v1 v2 v ->
      red E m (trm_app op ((trm_val v1)::(trm_val v2)::nil)) m v
  (* Operations on the abstract heap *) 
  | red_get : forall l π E T (p:trm) m w,
      p = val_abstract_ptr l π ->
      read_mem m l π w ->
      red E m (trm_app (prim_get T) (p::nil)) m w
  | red_set : forall (v:val) l π  E m1 T (p:trm) (t:trm) m2,
      p = val_abstract_ptr l π ->
      t = trm_val v ->
      updated_state l π v m1 m2 ->
      red E m1 (trm_app (prim_set T) (p::t::nil)) m2 val_unit
  | red_new: forall l (v:val) E m1 T (t:trm) m2 l,
      t = v ->
      l <> null ->
      \# m1 (fmap_single l v) -> (* l \notin fmap_dom m2 *)
      m2 = fmap_update m1 l v ->
      red E m1 (trm_app (prim_new T) (t::nil)) m2 (val_abstract_ptr l nil)
  | red_struct_access : forall E m t l s f π T v vr,
      t = val_abstract_ptr l π ->
      fmap_binds m l (val_struct s) ->
      fmap_binds (fmap_of_map s) f v ->
      vr = val_abstract_ptr l (π++((access_field f)::nil)) ->
      red E m (trm_app (prim_struct_access T f) (t::nil)) m vr
  | red_array_access : forall E m t l i π T vr ti (k:nat),
      t = val_abstract_ptr l π ->
      ti = trm_val (val_int i) ->
      i = k ->
      vr = val_abstract_ptr l (π++(access_array k)::nil) ->
      red E m (trm_app (prim_array_access T) (t::ti::nil)) m vr.

End Red.

(* Derived *)

Lemma red_seq : forall E m1 m2 m3 t1 t2 r1 r,
  red E m1 t1 m2 r1 ->
  red E m2 t2 m3 r ->
  red E m1 (trm_seq t1 t2) m3 r.
Proof using. intros. applys* red_let. Qed.


(* ---------------------------------------------------------------------- *)
(** Type inference rules *)

Definition gamma := Ctx.ctx typ.
Definition phi := fmap loc typ.

(** T'..π = T *)
Inductive follow_typ (C:typdefctx) : typ -> accesses -> typ -> Prop :=
  | follow_typ_nil : forall T,
    follow_typ C T nil T
  | follow_typ_array : forall T' π' T1 i n,
    follow_typ C T' π' T1 ->
    (0 <= i < n)%nat ->
    follow_typ C (typ_array T' n) ((access_array i)::π') T1
  | follow_typ_struct : forall S m f T' π' T1,
    fmap_binds C S m ->
    fmap_binds m f T' ->
    follow_typ C T' π' T1 ->
    follow_typ C (typ_struct S) ((access_field f)::π') T1.

(** φ(l)..π = T *)
Inductive read_phi (C:typdefctx) (φ:phi) (l:loc) (π:accesses) (T:typ) : Prop :=
  | read_phi_intro : forall T1, 
    fmap_binds φ l T1 -> 
    follow_typ C T1 π T -> 
    read_phi C φ l π T.

Record env := make_env { 
  env_typdefctx : typdefctx;
  env_gamma : gamma;
  env_phi : phi
}.

Definition env_add_binding E z X :=
  match E with
  | make_env C Γ φ => make_env C (Ctx.add z X Γ) φ
  end. 

Inductive typing : env -> trm -> typ -> Prop :=
  (* Values *)
  | typing_val_unit : forall E, 
      typing E val_unit typ_unit
  | typing_val_bool : forall E b,
      typing E (val_bool b) typ_bool
  | typing_val_int : forall E i,
      typing E (val_int i) typ_int
  | typing_val_double : forall E d,
      typing E (val_double d) typ_double
  | typing_val_struct : forall E mt mv s T,
      fmap_binds (env_typdefctx E) s mt ->
      fmap_dom mt = fmap_dom mv ->
      (forall f v, fmap_binds mt f T -> 
          fmap_binds (fmap_of_map mv) f v ->
          typing E v T) ->
      typing E (val_struct mv) (typ_struct s)
  | typing_val_array : forall E a T,
      (forall i v, Nth i a v -> 
        typing E v T) -> 
      typing E (val_array a) (typ_array T (List.length a))
  | typing_val_abstract_ptr : forall E l π T,
      read_phi (env_typdefctx E) (env_phi E) l π T ->
      typing E (val_abstract_ptr l π) (typ_ptr T)
  (* Binary operations *)
  | typing_binop : forall E v1 v2 (op:binop),
      typing E v1 typ_int ->
      typing E v2 typ_int ->
      typing E (trm_app op ((trm_val v1)::(trm_val v2)::nil)) typ_int
  (* Abstract heap operations *)
  | typing_get : forall E T p,
      typing E p (typ_ptr T) ->
      typing E (trm_app (prim_get T) (p::nil)) T
  | typing_set : forall E p t T,
      typing E p (typ_ptr T) ->
      typing E t T ->
      typing E (trm_app (prim_set T) (p::t::nil)) typ_unit
  | typing_alloc : forall E t T l, 
      typing E t T ->
      typing E (trm_app (prim_alloc T) (t::nil)) (typ_ptr T)
  | typing_struct_access : forall E s m f T T1 t,
      fmap_binds (env_typdefctx E) s m ->
      fmap_binds m f T1 ->
      T = typ_struct s ->
      typing E t (typ_ptr T) ->
      typing E (trm_app (prim_struct_access T f) (t::nil)) (typ_ptr T1)
  | typing_array_access : forall E t A i n,
      typing E t (typ_ptr (typ_array A n)) ->
      typing E i typ_int ->
      typing E (trm_app (prim_array_access A) (t::i::nil)) (typ_ptr A)
  (* Variables *)
  | typing_var : forall E x T,
      Ctx.lookup x (env_gamma E) = Some T ->
      typing E x T
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

(* If m(l)..π = w and |- w:T then φ(l)(π) = T. *)
(*Definition wf_state (φ:phi) (m:state) : Prop := forall l π v w T f,
  fmap_data m l = Some v ->
  follow v π = Some w ->
  typing fmap_empty Ctx.empty fmap_empty w T ->
      fmap_data φ l = Some f
  /\  fmap_data f π = Some T.

(** C,Γ |- t:T => (E)v:T. t \\ v *)
Theorem type_soundness : forall t T v C Γ φ,
  typing C Γ φ t T -> exists v m,
      red Ctx.empty fmap_empty t m v
  /\  typing C Γ φ v T.
Proof.
  intros. induction H.
  { exists val_unit.  exists (@fmap_empty loc val). 
    split; constructors*. }
  { exists (val_bool b). exists (@fmap_empty loc val). 
    split; constructors*. }
  { exists i. exists (@fmap_empty loc val). 
    split; constructors*. }
  { exists (val_double d). exists (@fmap_empty loc val). 
    split; constructors*. }
  { exists (val_struct mv). exists (@fmap_empty loc val). 
    split; constructors*. }
  { exists (val_array a). exists (@fmap_empty loc val). 
    split; constructors*. }
  { exists (val_abstract_ptr l π). exists (@fmap_empty loc val).
    split; constructors*. }
  {  }
Admitted.*)

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
