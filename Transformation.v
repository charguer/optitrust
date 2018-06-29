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

(*
Axiom map_tr_vals_on_map_vals : forall A B, map A B -> map A B.

(*
Definition map_union := monoid_make (fun m1 m2 : map field val => m1 \u m2) \{}.

Fixpoint map_map_vals (f:val -> val) (m:map field val) : map field val :=
  fold map_union (fun (k:field) (v:val) => \{}[k:=(f v)]) m.
*)

Fixpoint tr_accesses (π:accesses) : accesses :=
  match π with
  | nil => nil
  | (access_field "x")::π' => (access_field "s")::(access_field "x")::(tr_accesses π')
  | p::π' => p::(tr_accesses π')
  end.

Fixpoint tr_val (v:val) : val :=
  match v with
  | val_abstract_ptr l π => val_abstract_ptr l (tr_accesses π)
  | val_array a => val_array (List.map tr_val a)
  | val_struct s => val_struct (map_tr_vals_on_map_vals s)
  | _ => v
  end.

(* Assuming we're going from C to C' *)
Fixpoint tr (t:trm) : trm :=
  match t with
  | trm_val v => trm_val (tr_val v)
  | trm_var x => trm_var x
  | trm_if t1 t2 t3 =>
      trm_if (tr t1) (tr t2) (tr t3)
  | trm_let b t1 t2 =>
      trm_let b (tr t1) (tr t2)
  | trm_app f ts => 
      let ts' := List.map tr ts in
      match f, ts with
      | (prim_struct_access pos "x"), p::nil => 
          let outer_access := prim_struct_access (typ_struct "struct_x") "x" in
          let inner_access := prim_struct_access (typ_struct "pos") "s" in
          trm_app outer_access ((trm_app inner_access (p::nil))::nil)
      | _, _ => trm_app f ts'
      end
  end.*)

Inductive tr_accesses : accesses -> accesses -> Prop :=
  | tr_accesses_nil :
      tr_accesses nil nil
  | tr_accesses_access_field_x : forall π π',
      tr_accesses π π' ->
      tr_accesses ((access_field "x")::π) ((access_field "s")::(access_field "x")::π')
  | tr_accesses_other : forall π π' x,
      tr_accesses π π' ->
      tr_accesses (x::π) (x::π').

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

Inductive tr_state : state -> state -> Prop :=
  | tr_state_intro : forall m m',
      dom m = dom m' ->
      (forall l,
        index m l ->
        index m' l /\ tr_val m[l] m'[l]) ->
      tr_state m m'.

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
      tr_trm (trm_app (prim_array_access A) (t::i::nil)) (trm_app (prim_array_access A) (t'::i'::nil)).
  (* Special case: struct access *)
  (*| tr_trm_struct_access :
      (trm_app (prim_struct_access T f) (t::nil))*)

(* Semantics preserved by tr. *)
Theorem red_tr: forall E t s1 s2 v,
  red E s1 t s2 v -> 
  red E s1 (tr t) s2 v.
Proof.
Admitted.
