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
  end.

(* Thoughts:
The critical terms in which we need to change the pointer are those applied on
a get or a set. At first I thought that the whole program should be changed 
using what now is called tr_critical. However, then we would not be able to 
prove that the semantics are preserved as a program could evaluate to a 
val_abstract_ptr that we change. So we could invoke tr_critical in terms 
that follow a get or a set, but that would not work either because of variables.
If we apply tr_critical on let bindings as well though, we have the same problem
with the semantics not being preserved. I assume that that is not so bad and we
can reformulate the statement of semantics preservation to take into account this
case.
*)

(* Semantics preserved by tr. *)
Theorem red_tr: forall E t s1 s2 v,
  red E s1 t s2 v -> 
  red E s1 (tr t) s2 v.
Proof.
Admitted.
