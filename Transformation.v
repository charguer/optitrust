(**

Basic transformations.

*)


Set Implicit Arguments.

Require Export Semantics.


Global Instance Inhab_trm : Inhab trm.
Proof using. apply (Inhab_of_val (trm_val val_unit)). Qed.

(* Initial typdefctx *)
Definition pos : typdef_struct := (\{})["x" := typ_int]["y" := typ_int]["z" := typ_int].
Definition C : typdefctx := (\{})["pos" := pos].

(* Final typdefctx *)
Definition struct_x : typdef_struct := (\{})["x" := typ_int].
Definition pos' : typdef_struct := (\{})["s" := (typ_struct "struct_x")]["y" := typ_int]["z" := typ_int].
Definition C' : typdefctx := (\{})["pos" := pos'].

(* π ~ |π| *)
Inductive tr_accesses : accesses -> accesses -> Prop :=
  | tr_accesses_nil :
      tr_accesses nil nil
  | tr_accesses_access_x : forall π π',
      tr_accesses π π' ->
      tr_accesses ((access_field "x")::π) ((access_field "s")::(access_field "x")::π')
  | tr_accesses_other : forall π π' x,
      x <> access_field "x" ->
      tr_accesses π π' ->
      tr_accesses (x::π) (x::π').

(* v ~ |v| *)
Inductive tr_val : val -> val -> Prop :=
  | tr_val_unit :
      tr_val val_unit val_unit
  | tr_val_bool : forall b,
      tr_val (val_bool b) (val_bool b)
  | tr_val_int : forall i,
      tr_val (val_int i) (val_int i)
  | tr_val_double : forall d,
      tr_val (val_double d) (val_double d)
  | tr_val_abstract_ptr : forall l π π',
      tr_accesses π π' ->
      tr_val (val_abstract_ptr l π) (val_abstract_ptr l π')
  | tr_val_array : forall a a',
      length a = length a' ->
      (forall i, 
        index a i -> 
        index a' i /\ tr_val a[i] a'[i]) ->
      tr_val (val_array a) (val_array a')
  | tr_val_struct : forall s s',
      dom s = dom s' ->
      (forall f,
        index s f ->
        index s' f /\ tr_val s[f] s'[f]) ->
      tr_val (val_struct s) (val_struct s').

Search Ctx.ctx.

(* S ~ |S| *)
Inductive tr_stack : stack -> stack -> Prop :=
  | tr_stack_intro : forall S S',
      List.map fst S = List.map fst S' ->
      (forall x v v',
        Ctx.lookup x S = Some v ->
        Ctx.lookup x S' = Some v' /\ tr_val v v') ->
      tr_stack S S'.

(* m ~ |m| *)
Inductive tr_state : state -> state -> Prop :=
  | tr_state_intro : forall m m',
      dom m = dom m' ->
      (forall l,
        index m l ->
        index m' l /\ tr_val m[l] m'[l]) ->
      tr_state m m'.

(* t ~ |t| *)
Inductive tr_trm : trm -> trm -> Prop :=
  | tr_trm_val : forall v v',
      tr_val v v' ->
      tr_trm (trm_val v) (trm_val v')
  | tr_trm_var : forall x,
      tr_trm (trm_var x) (trm_var x)
  | tr_trm_if : forall t1 t2 t3 t1' t2' t3',
      tr_trm t1 t1' ->
      tr_trm t2 t2' ->
      tr_trm t3 t3' ->
      tr_trm (trm_if t1 t2 t3) (trm_if t1' t2' t3')
  | tr_trm_let : forall x t1 t2 t1' t2',
      tr_trm t1 t1' ->
      tr_trm t2 t2' ->
      tr_trm (trm_let x t1 t2) (trm_let x t1' t2')
  | tr_trm_binop : forall t1 t2 t1' t2',
      tr_trm t1 t1' ->
      tr_trm t2 t2' ->
      tr_trm (trm_app binop_add (t1::t2::nil)) (trm_app binop_add (t1'::t2'::nil))
  (* Abstract heap operations *)
  | tr_trm_get : forall T p p',
      tr_trm p p' ->
      tr_trm (trm_app (prim_get T) (p::nil)) (trm_app (prim_get T) (p'::nil))
  | tr_trm_set : forall T p p' t t',
      tr_trm p p' ->
      tr_trm t t' ->
      tr_trm (trm_app (prim_set T) (p::t::nil)) (trm_app (prim_set T) (p'::t'::nil))
  | tr_trm_new : forall T t t',
      tr_trm t t' ->
      tr_trm (trm_app (prim_new T) (t::nil)) (trm_app (prim_new T) (t'::nil))
  | tr_trm_array_access : forall A t i t' i',
      tr_trm (trm_app (prim_array_access A) (t::i::nil)) (trm_app (prim_array_access A) (t'::i'::nil))
  (* Special case: struct access *)
  | tr_trm_struct_access_x : forall p p' a1 a2 r,
      tr_trm p p' ->
      a1 = prim_struct_access (typ_struct "struct_x") "x" ->
      a2 = prim_struct_access (typ_struct "pos") "s" ->
      r = trm_app a1 ((trm_app a2 (p'::nil))::nil) ->
      tr_trm (trm_app (prim_struct_access (typ_struct "pos") "x") (p::nil)) r
  | tr_trm_struct_access_other : forall p p' T f r,
      (T <> "pos" \/ f <> "x") -> 
      tr_trm p p' -> 
      r = (trm_app (prim_struct_access (typ_struct T) f) (p'::nil)) ->
      tr_trm (trm_app (prim_struct_access (typ_struct T) f) (p::nil)) r.

(* Semantics preserved by tr. *)
Theorem red_tr: forall t t' v S m1 m2,
  red S m1 t m2 v -> 
  tr_trm t t' ->
  (exists v' S' m1' m2',
        tr_val v v'
    /\  tr_stack S S'
    /\  tr_state m1 m1'
    /\  tr_state m2 m2'
    /\  red S' m1' t' m2' v').
Proof.
  introv H TRt. induction H. 
Admitted.
