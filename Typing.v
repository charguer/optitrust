(**

This file describes transformations of the layout of records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export TLCbuffer Semantics LibSet LibMap.



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
(** Typing of access paths *)

(** T[π] = T1 *)

Inductive follow_typ (C:typdefctx) : typ -> accesses -> typ -> Prop :=
  | follow_typ_nil : forall T,
      follow_typ C T nil T
  | follow_typ_array : forall T π Tr i n,
      follow_typ C T π Tr ->
      follow_typ C (typ_array T n) ((access_array i)::π) Tr
  | follow_typ_struct : forall T f π Tr,
      T \indom C ->
      f \indom C[T] ->
      follow_typ C C[T][f] π Tr ->
      follow_typ C (typ_struct T) ((access_field T f)::π) Tr.

(** φ(l)..π = T *)

Inductive read_phi (C:typdefctx) (φ:phi) (l:loc) (π:accesses) (T:typ) : Prop :=
  | read_phi_intro : 
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
  | typing_val_struct : forall Tfs vfs T,
      Tfs = C[T] ->
      dom Tfs = dom vfs ->
      (forall f, 
          f \indom Tfs -> 
          f \indom vfs ->
          typing_val C φ vfs[f] Tfs[f]) ->
      typing_val C φ (val_struct T vfs) (typ_struct T)
  | typing_val_array : forall a T (n:nat),
      length a = n ->
      (forall i, 
        index a i -> 
        typing_val C φ a[i] T) -> 
      typing_val C φ (val_array a) (typ_array T (Some n))
  | typing_val_abstract_ptr : forall l π T,
      read_phi C φ l π T ->
      typing_val C φ (val_abstract_ptr l π) (typ_ptr T).


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
  | typing_get : forall E T p,
      typing E p (typ_ptr T) ->
      typing E (trm_app (prim_get T) (p::nil)) T
  | typing_set : forall E p t T,
      typing E p (typ_ptr T) ->
      typing E t T ->
      typing E (trm_app (prim_set T) (p::t::nil)) typ_unit
  | typing_new : forall E T, 
      typing E (trm_app (prim_new T) nil) (typ_ptr T)
  | typing_new_array : forall E T t, 
      typing E t typ_int ->
      typing E (trm_app (prim_new_array T) (t::nil)) (typ_ptr (typ_array T None))
  | typing_struct_access : forall E C Tfs f T t,
      C = env_typdefctx E ->
      T \indom C ->
      Tfs = C[T] ->
      f \indom Tfs ->
      typing E t (typ_ptr (typ_struct T)) ->
      typing E (trm_app (prim_struct_access T f) (t::nil)) (typ_ptr Tfs[f])
  | typing_array_access : forall E t T ti n,
      typing E t (typ_ptr (typ_array T n)) ->
      typing E ti typ_int ->
      typing E (trm_app (prim_array_access T) (t::ti::nil)) (typ_ptr T)
  (* Operations on structs and arrays as values *)
  | typing_struct_get : forall E C Tfs f T t,
      C = env_typdefctx E ->
      T \indom C ->
      Tfs = C[T] ->
      f \indom Tfs ->
      typing E t (typ_struct T) ->
      typing E (trm_app (prim_struct_get T f) (t::nil)) Tfs[f]
  | typing_array_get : forall E T t ti n,
      typing E t (typ_array T n) ->
      typing E ti typ_int ->
      typing E (trm_app (prim_array_get T) (t::ti::nil)) T
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
      dom φ \c dom m
  /\  (forall l, typing_val C φ m[l] φ[l]).

Definition stack_typing (C:typdefctx) (φ:phi) (Γ:gamma) (S:stack) : Prop := 
  forall x v T,
    Ctx.lookup x S = Some v ->
    Ctx.lookup x Γ = Some T ->
    typing_val C φ v T.


(* ********************************************************************** *)
(* * Type soundness *)

Section TypeSoundness.

Hint Constructors typing_val redbinop. 


(* ---------------------------------------------------------------------- *)
(** Functional predicates *)

(* Types are well-formed *)
Lemma functional_follow_typ : forall C T π T1 T2,
  follow_typ C T π T1 ->
  follow_typ C T π T2 ->
  T1 = T2.
Proof.
  introv HF1 HF2. induction HF1; inverts* HF2.
Qed.

(* φ is well-formed *)
Lemma read_phi_inj : forall C φ l π T1 T2,
  read_phi C φ l π T1 ->
  read_phi C φ l π T2 ->
  T1 = T2.
Proof.
  introv H1 H2. inverts H1. inverts H2.
  applys* functional_follow_typ.
Qed.


(* ---------------------------------------------------------------------- *)
(** Preservation of typing over operations *)

(** Lemma for typing preservation of [let] *)

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

(** Auxiliary lemma for typing preservation of [get] *)

Lemma typing_val_follow : forall T1 w1 π C φ w2 T2,
  typing_val C φ w1 T1 ->
  follow_typ C T1 π T2 ->
  read_accesses w1 π w2 ->
  typing_val C φ w2 T2.
Proof.
  introv HT HF HR. gen π. induction HT; intros;
   try solve [ intros ; inverts HR; inverts HF; constructors* ].
  { inverts HF as; inverts HR as; subst*; try constructors*. }
  { inverts HF as; inverts HR as; try constructors*.
    introv HN1 HR HT. eauto. }
Qed.

(** Lemma for typing preservation of [get] *)

Lemma typing_val_get : forall m l π C φ w T,
  state_typing C φ m ->
  read_state m l π w ->
  read_phi C φ l π T ->
  typing_val C φ w T.
Proof.
  introv (HD&HT) HS HP. inverts HS as Hi HR.
  inverts HP as HF. forwards HTl: HT l.
  applys* typing_val_follow HTl HF HR.
Qed.

(** Auxiliary lemma for typing preservation of [set] *)

Lemma typing_val_after_write : forall v1 w π T2 C φ v2 T1,
  write_accesses v1 π w v2 ->
  typing_val C φ v1 T1 ->
  follow_typ C T1 π T2 ->
  typing_val C φ w T2 ->
  typing_val C φ v2 T1.
Proof.
  introv HW HT1 HF HT2. gen T1. induction HW; intros.
  { subst. inverts* HF. }
  { inverts HF. inverts HT1. subst. constructors. 
    { rewrite* length_update. }
    { intros. rewrite index_update_eq in *. 
      rewrite* read_update_case. case_if*. } }
  { inverts HF as _ _ HF.
    inverts HT1 as HD HCT. subst. constructors*.
    { unfold state. rewrite* dom_update_at_index. }
    { intros f' Hi1 Hi2. rewrite read_update.
      case_if*. 
      { subst. applys* IHHW. } } }
Qed.

(** Lemma for typing preservation of [set] *)

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
  { unfold state. rewrite* dom_update_at_index. }
  { intros l'. forwards HT': HT l'. rewrite read_update. case_if*.
    subst. inverts HP as HF. applys* typing_val_after_write. }
Qed.

(** Lemma for typing preservation of [new] *)

Lemma uninitialized_val_typ : forall C T v φ,
  uninitialized_val C T v ->
  typing_val C φ v T.
Proof.
  introv Hu. induction Hu; subst; constructors~.
Qed.

(** Lemma for typing preservation of [struct_access] *)

Lemma follow_typ_access_field : forall C π T T' f,
  T \indom C ->
  f \indom C[T] ->
  follow_typ C T' π (typ_struct T) ->
  follow_typ C T' (π & (access_field T f)) C[T][f].
Proof.
  introv HTin Hfin HF. gen C T T' f. induction π; 
  intros; inverts HF; rew_list; repeat constructors*. 
Qed.

(** Lemma for typing preservation of [array_access] *)

Lemma follow_typ_access_array : forall C T' T i n π,
  follow_typ C T' π (typ_array T n) ->
  follow_typ C T' (π & access_array i) T.
Proof.
  introv HF. gen C T T' i n. induction π;
  intros; inverts HF; rew_list; repeat constructors*.
Qed.


(* TODO: These are shared with TrGroup. 
   Find a nice way to factor this out. *)
Axiom not_is_error_args_1 : forall C S m op ts m' v w,
  red C S m (trm_app op (trm_val w :: ts)) m' v ->
  ~ is_error v ->
  ~ is_error w.

Axiom not_is_error_args_2 : forall C S m op t ts m' v w,
  red C S m (trm_app op (t :: trm_val w :: ts)) m' v ->
  ~ is_error v ->
  ~ is_error w.


(* ---------------------------------------------------------------------- *)
(** Typing state extension *)

Definition extends (φ:phi) (φ':phi) :=
      dom φ \c dom φ'
  /\  forall l, l \indom φ -> φ' l = φ l.

(* TODO: I added this for automation *)

Axiom trans_extends : trans extends.

Hint Extern 1 (extends ?φ1 ?φ3) => 
  match goal with
  | H: extends ?φ1 ?φ2 |- _ => applys trans_extends H
  | H: extends ?φ2 ?φ3 |- _ => applys trans_extends H
  end.

Lemma extends_transitivity_demo : forall φ1 φ2 φ3,
  extends φ1 φ2 ->
  extends φ2 φ3 ->
  extends φ1 φ3.
Proof using. intros. auto. Qed.


(* ---------------------------------------------------------------------- *)
(** Type preservation proof *)

Theorem type_soundess_warmup : forall C φ m t v T Γ S m',
  red C S m t m' v -> 
  ~ is_error v ->
  typing (make_env C φ Γ) t T ->
  state_typing C φ m ->
  stack_typing C φ Γ S ->
        typing_val C φ v T
    /\  state_typing C φ m'.
Proof.
  introv R He. gen φ T Γ. induction R; introv HT HM HS;
  try solve [ forwards*: He ; unfolds~ ]. 
  { (* var *)
    inverts HT. simpls. split*. }
  { (* val *)  
    inverts HT. split*. }
  { (* if *) 
    inverts HT. 
    forwards* (HT1&HM1): IHR1. introv HN. inverts HN.
    forwards* (HT2&HM2): IHR2. 
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
  { (* new_array *)
    admit. }
  { (* struct_access *) 
    subst. inverts HT as HTin Hfin HT. splits~.
    inverts HT as HT. simpls.
    inverts HT as Hφ.
    inverts Hφ as HF.
    repeat constructors.
    applys~ follow_typ_access_field. }
  { (* array_access *) 
    inverts HT as HT HTi. subst.
    inverts HT as HT. simpls.
    inverts HT as Hφ.
    inverts Hφ as HF.
    inverts HM as HD HM.
    repeat constructors~.
    applys* follow_typ_access_array. }
  { (* struct_get *) 
    subst. inverts HT as HTin Hfin HT. 
    splits~.
    inverts HT as HT. simpls.
    inverts HT as HD HT.
    applys~ HT. }
  { (* array_get *) 
    subst. inverts HT as HTa HTi.
    splits~.
    inverts HTa as HTa. simpls.
    inverts HTa as Hl HTa.
    applys~ HTa. }
  { (* app 1 *) 
    forwards*: IHR2; inverts HT; forwards* (HTv1&Hm2): IHR1;
    try applys* not_is_error_args_1 ; repeat constructors*. }
  { (* app 2 *) 
    forwards*: IHR2; inverts HT; forwards* (HTv1&Hm2): IHR1;
    try applys* not_is_error_args_2 ; repeat constructors*. }
Qed.


Theorem type_soundess : forall C φ m t v T Γ S m',
  typing (make_env C φ Γ) t T ->
  state_typing C φ m ->
  stack_typing C φ Γ S ->
  red C S m t m' v -> 
  exists φ',
        extends φ φ'
    /\  typing_val C φ' v T
    /\  state_typing C φ' m'.
Proof.
Admitted.

End TypeSoundness.
