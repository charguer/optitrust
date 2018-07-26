(**

This file describes the syntax and semantics of an imperative lambda 
calculus with records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export Typing Validity.

Open Scope set_scope.
Open Scope container_scope.


(* ********************************************************************** *)
(* * Semantics *)

(* ---------------------------------------------------------------------- *)
(** Semantics of binary operations *)

Inductive redbinop : binop -> val -> val -> val -> Prop :=
  | redbinop_add : forall n1 n2,
      redbinop binop_add (val_int n1) (val_int n2) (val_int (n1 + n2))
  | redbinop_sub : forall n1 n2,
      redbinop binop_sub (val_int n1) (val_int n2) (val_int (n1 - n2))
  | redbinop_mul : forall n1 n2,
      redbinop binop_mul (val_int n1) (val_int n2) (val_int (n1 * n2))
  | redbinop_div : forall n1 n2,
      redbinop binop_div (val_int n1) (val_int n2) (val_int (n1 / n2))
  | redbinop_mod : forall n1 n2,
      redbinop binop_mod (val_int n1) (val_int n2) (val_int (n1 mod n2))
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
  | uninitialized_array : forall os T Ta a,
      typing_array C Ta T os ->
      (forall n,
        os = Some n ->
        length a = n) ->
      (forall i,
        index a i ->
        uninitialized C T a[i]) ->
      uninitialized C Ta (val_array Ta a)
  | uninitialized_struct : forall Ts Tfs vfs,
      typing_struct C Ts Tfs ->
      dom Tfs = dom vfs ->
      (forall f,
        f \indom Tfs ->
        uninitialized C Tfs[f] vfs[f]) ->
      uninitialized C Ts (val_struct Ts vfs).


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
  | write_accesses_array : forall v a1 i T π w a2,
      index a1 i -> 
      write_accesses a1[i] π w v ->
      a2 = a1[i:=v] ->
      write_accesses (val_array T a1) ((access_array T i)::π) w (val_array T a2)
  | write_accesses_struct : forall T s1 s2 f π w v,
      f \indom s1 ->
      write_accesses s1[f] π w v ->
      s2 = s1[f := v] ->
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
      red C S m (trm_val v) m v
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
      valid_typ C T ->
      uninitialized C T v -> 
      m2 = m1[l := v] ->
      red C S m1 (trm_app (prim_new T) nil) m2 vr
  | red_new_array : forall l (n:int) (k:nat) a S m1 T v1 m2 vr,
      v1 = val_int n ->
      vr = val_abstract_ptr l nil ->
      l <> null ->
      l \notindom m1 ->
      n = k ->
      valid_typ C T ->
      uninitialized C (typ_array T (Some k)) (val_array (typ_array T (Some k)) a) -> 
      m2 = m1[l := (val_array  (typ_array T None) a)] ->
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
      ~ (exists a, uninitialized C (typ_array T (Some k)) (val_array (typ_array T (Some k)) a)) -> 
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
(** Lemmas about the error cases *)

Lemma not_is_error_args_1 : forall C S m op ts m' v w,
  red C S m (trm_app op (trm_val w :: ts)) m' v ->
  ~ is_error v ->
  ~ is_error w.
Proof.
  introv HR He HN. destruct w; inverts HN;
  inverts HR; tryfalse*. inverts_head red; tryfalse*.
Qed.

Lemma not_is_error_args_2 : forall C S m op t ts m' v w,
  red C S m (trm_app op (t :: trm_val w :: ts)) m' v ->
  ~ is_error v ->
  ~ is_error w.
Proof.
  introv HR He HN. destruct w; inverts HN; 
  inverts HR; tryfalse*. inverts_head red; tryfalse*.
Qed.


(* ---------------------------------------------------------------------- *)
(** Lemmas about the completeness of the reduction rules *)

Lemma red_complete : forall C S m1 t, 
  exists v m2, red C S m1 t m2 v.
Proof.
Admitted.


(* ********************************************************************** *)
(* * Validity results *)

Lemma stack_valid_add : forall C x v S,
  valid_stack C S ->
  valid_val C v ->
  valid_stack C (Ctx.add x v S).
Proof.
  introv HS Hv. unfolds Ctx.add. destruct~ x.
  unfolds. introv HCl. unfold Ctx.lookup in HCl.
  case_if in HCl.
  { inverts~ HCl. }
  { applys~ HS x. }
Qed.

Lemma redbinop_valid : forall op v1 v2 C vr,
  redbinop op v1 v2 vr ->
  valid_val C v1 ->
  valid_val C v2 ->
  valid_val C vr.
Proof.
  introv HR Hv1 Hv2. induction HR; constructors*.
Qed.

Lemma read_accesses_valid : forall v1 π C v2,
  read_accesses v1 π v2 ->
  valid_val C v1 ->
  valid_accesses C π ->
  valid_val C v2.
Proof.
  introv HR Hv1 Hπ. induction HR.
  { auto. }
  { inverts Hπ. inverts Hv1 as Hv1 Hai. 
    applys~ IHHR. }
  { inverts Hπ. inverts Hv1 as Hv1 Hsf.
    applys~ IHHR. }
Qed.

Lemma read_state_valid : forall m l π C v,
  read_state m l π v ->
  valid_state C m ->
  valid_accesses C π ->
  valid_val C v.
Proof.
  introv HR Hm Hπ. unfolds valid_state. inverts HR.
  forwards*: Hm. applys* read_accesses_valid.
Qed.

Lemma write_accesses_valid : forall v1 w π C v2,
  write_accesses v1 π w v2 ->
  valid_val C v1 ->
  valid_val C w ->
  valid_accesses C π ->
  valid_val C v2.
Proof.
  introv HW Hv1 Hw Hπ. induction HW.
  { auto. }
  { subst. inverts Hπ. inverts Hv1 as Hv1 Ha0i.
    constructors*. introv Hi. rew_index~ in Hi. rew_reads*. }
  { subst. inverts Hπ. inverts Hv1 as Hv1 Hs1f.
    constructors*. introv Hf. rew_reads*. }
Qed.

Lemma write_state_valid : forall m1 l π v C m2,
  write_state m1 l π v m2 ->
  valid_state C m1 ->
  valid_accesses C π ->
  valid_val C v ->
  valid_state C m2.
Proof. 
  introv HW Hm1 Hπ Hv. inverts HW. unfolds valid_state.
  forwards*: Hm1. introv Hl0. rew_reads*; intros.
  { applys* write_accesses_valid. }
  { applys~ Hm1. applys~ indom_update_inv_neq l l0 v2. }
Qed.

Lemma typing_array_valid : forall Ta os C T,
  typing_array C Ta T os ->
  valid_typ C Ta ->
  valid_typ C T.
Proof.
  introv HTa HVTa. induction HTa; inverts~ HVTa.
Qed.

Lemma typing_struct_valid : forall Ts C Tfs,
  typing_struct C Ts Tfs ->
  valid_typ C Ts ->
  (forall f,
    f \indom Tfs ->
    valid_typ C Tfs[f]).
Proof.
  introv HTs HVTs. induction HTs; inverts~ HVTs.
Qed.

Lemma uninitialized_valid : forall v C T,
  valid_typ C T ->
  uninitialized C T v ->
  valid_val C v.
Proof.
  introv HT Hu. induction Hu; try solve [ constructors* ].
  { constructors~. introv Hi. applys~ H2.
    applys* typing_array_valid. }
  { constructors~. introv Hf. rewrite <- H0 in Hf. applys~ H2.
    applys* typing_struct_valid. }
Qed.

(* Path surgery of valid accesses *)

Lemma valid_accesses_app : forall C π1 π2,
  valid_accesses C π1 ->
  valid_accesses C π2 ->
  valid_accesses C (π1 ++ π2).
Proof.
  introv Ha1 Ha2. gen π2. induction Ha1; intros;
  rew_list in *; eauto; constructors~.
Qed.

Lemma red_valid : forall S m1 t C m2 v,
  red C S m1 t m2 v ->
  valid_stack C S ->
  valid_state C m1 ->
  valid_trm C t ->
  ~ is_error v ->
      valid_state C m2
  /\  valid_val C v.
Proof.
  introv HR HS Hm1 Ht He. induction HR; intros;
  try solve [ inverts* Ht ; forwards*: He ].
  { inverts Ht. forwards* (Hm2&HVb): IHHR1.
    applys~ IHHR2. case_if*. }
  { inverts Ht. forwards* (Hm2&HVb): IHHR1.
    applys~ IHHR2. applys~ stack_valid_add. }
  { inverts Ht as Hop Hv1 Hv2.
    inverts Hv1 as Hv1. inverts Hv2 as Hv2.
    splits~. applys* redbinop_valid. }
  { subst. inverts Ht as Hop Hp. inverts Hp as Hp. 
    inverts Hp as Hπ. splits~. 
    applys* read_state_valid. }
  { subst. inverts Ht as Hop Hp Hv2. 
    inverts Hv2 as Hv2. inverts Hp as Hp.
    inverts Hp as Hπ. splits; try constructors*.
    applys* write_state_valid. }
  { subst. splits.
    { unfolds valid_state. introv Hl0. rew_reads; intros; subst.
      { applys* uninitialized_valid. }
      { applys~ Hm1. applys~ indom_update_inv_neq l l0 v. } }
    { repeat constructors*. } }
  { subst. splits.
    { unfolds valid_state. introv Hl0. rew_reads; intros; subst.
      { forwards HV: uninitialized_valid H5. { constructors~. }
        inverts HV as HV HVai. repeat constructors~. }
      { applys~ Hm1. applys* indom_update_inv_neq l l0. } }
    { repeat constructors*. } }
  { subst. inverts Ht as Hop Hp. inverts Hop. inverts Hp as Hπ.
    splits~. constructors~. inverts Hπ. applys~ valid_accesses_app.
    repeat constructors~. }
  { subst. inverts Ht as Hop Hp Hi. inverts Hop. inverts Hp as Hπ.
    splits~. constructors~. inverts Hπ. applys~ valid_accesses_app.
    repeat constructors~. }
  { subst. inverts Ht as Hop Hs. inverts Hop. inverts Hs as Hs.
    inverts Hs as Hs Hsf0. splits~. }
  { subst. inverts Ht as Hop Ha Hi. inverts Ha as Ha.
    inverts Ha as Ha Hai. splits~. }
  { forwards* (Hm2&Hv1): IHHR1.
    { inverts~ Ht. }
    { applys* not_is_error_args_1. }
    applys~ IHHR2.
    { inverts~ Ht; repeat constructors~. } }
  { forwards* (Hm2&Hv1): IHHR1.
    { inverts~ Ht. }
    { applys* not_is_error_args_2. }
    applys~ IHHR2.
    { inverts~ Ht; repeat constructors~. } }
Qed.

(* TODO:
   - rew_reads improve.
   - dom_prove new tactic. 
   - map on maps. 
   - automation for red_complete. *)
