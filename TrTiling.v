(**

This file describes transformations of the layout of records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export Semantics LibSet LibMap TLCbuffer.

(** In order to make this transformation work, we will need to first:
    - Annotate val_array with the array type.
    - Annotate array_access with the array type.
*)


(* ********************************************************************** *)
(* * Definition of the transformation *)

(** Grouping transformation. Specified by:
    - The name of the struct to be modified.
    - The set of fields to be grouped.
    - The name of the new struct that will hold the fields to be grouped.
    - The name of the field in the new struct that will have as type
      the new struct. *)

Record group_tr := make_group_tr {
  group_tr_struct_name : typvar;
  group_tr_fields : set field;
  group_tr_new_struct_name : typvar;
  group_tr_new_struct_field : field
}.

Notation make_group_tr' := make_group_tr.

(** Transformation of paths: π ~ |π| *)

Inductive tr_accesses (gt:group_tr) : accesses -> accesses -> Prop :=
  | tr_accesses_nil :
      tr_accesses gt nil nil
  | tr_accesses_array : forall π π' i,
      tr_accesses gt π π' ->
      tr_accesses gt ((access_array i)::π) ((access_array i)::π')
  | tr_accesses_field_group : forall π π' f fg Ts Tsg,
      tr_accesses gt π π' ->
      Ts = group_tr_struct_name gt ->
      Tsg = group_tr_new_struct_name gt ->
      f \in (group_tr_fields gt) ->
      fg = group_tr_new_struct_field gt ->
      tr_accesses gt ((access_field Ts f)::π) ((access_field Ts fg)::(access_field Tsg f)::π')
  | tr_accesses_field_other : forall T Ts π π' f,
      tr_accesses gt π π' ->
      Ts = group_tr_struct_name gt ->
      (T <> Ts \/ f \notin (group_tr_fields gt)) ->
      tr_accesses gt ((access_field T f)::π) ((access_field T f)::π').

(** Transformation of values: v ~ |v| *)

Inductive tr_val (gt:group_tr) : val -> val -> Prop :=
  | tr_val_error :
      tr_val gt val_error val_error
  | tr_val_uninitialized :
      tr_val gt val_uninitialized val_uninitialized
  | tr_val_unit :
      tr_val gt val_unit val_unit
  | tr_val_bool : forall b,
      tr_val gt (val_bool b) (val_bool b)
  | tr_val_int : forall i,
      tr_val gt (val_int i) (val_int i)
  | tr_val_double : forall d,
      tr_val gt (val_double d) (val_double d)
  | tr_val_abstract_ptr : forall l π π',
      tr_accesses gt π π' ->
      tr_val gt (val_abstract_ptr l π) (val_abstract_ptr l π')
  | tr_val_array : forall a a',
      length a = length a' ->
      (forall i,
        index a i ->
        tr_val gt a[i] a'[i]) ->
      tr_val gt (val_array a) (val_array a')
  | tr_val_struct_group : forall Ts Tsg s s' fg fs sg,
      gt = make_group_tr Ts fs Tsg fg ->
      fs \c dom s ->
      fg \notindom s ->
      dom s' = (dom s \- fs) \u \{fg} ->
      dom sg = fs ->
      (forall f,
        f \indom sg ->
        tr_val gt s[f] sg[f]) ->
      (forall f,
        f \notin fs ->
        f \indom s ->
        tr_val gt s[f] s'[f]) ->
      s'[fg] = val_struct Tsg sg ->
      tr_val gt (val_struct Ts s) (val_struct Ts s')
  | tr_val_struct_other : forall T s s',
      T <> group_tr_struct_name gt ->
      dom s = dom s' ->
      (forall f,
        f \indom s ->
        tr_val gt s[f] s'[f]) ->
      tr_val gt (val_struct T s) (val_struct T s').

Definition is_prim_struct_access (op:prim) :=
  match op with
  | prim_struct_access _ _ => True
  | _ => False
  end.


(** Transformation of terms: t ~ |t| *)

Inductive tr_trm (gt:group_tr) : trm -> trm -> Prop :=
  | tr_trm_val : forall v v',
      tr_val gt v v' ->
      tr_trm gt (trm_val v) (trm_val v')
  | tr_trm_var : forall x,
      tr_trm gt (trm_var x) (trm_var x)
  | tr_trm_if : forall t1 t2 t3 t1' t2' t3',
      tr_trm gt t1 t1' ->
      tr_trm gt t2 t2' ->
      tr_trm gt t3 t3' ->
      tr_trm gt (trm_if t1 t2 t3) (trm_if t1' t2' t3')
  | tr_trm_let : forall x t1 t2 t1' t2',
      tr_trm gt t1 t1' ->
      tr_trm gt t2 t2' ->
      tr_trm gt (trm_let x t1 t2) (trm_let x t1' t2')
  (* new *)  
  | tr_trm_new : forall T,
      tr_trm gt (trm_app (prim_new T) nil) (trm_app (prim_new T) nil)
  (* Special case: struct access *)
  | tr_trm_struct_access_x : forall p p' s s_g f f_g a1 a2 r,
      tr_trm gt p p' ->
      s = group_tr_struct_name gt ->
      s_g = group_tr_new_struct_name gt ->
      f \in (group_tr_fields gt) ->
      f_g = group_tr_new_struct_field gt ->
      a1 = prim_struct_access s_g f ->
      a2 = prim_struct_access s f_g ->
      r = trm_app a1 ((trm_app a2 (p'::nil))::nil) ->
      tr_trm gt (trm_app (prim_struct_access s f) (p::nil)) r
  | tr_trm_struct_access_other : forall s p p' T f r,
      tr_trm gt p p' ->
      s = group_tr_struct_name gt ->
      (T <> s \/ f \notin (group_tr_fields gt)) ->
      r = (trm_app (prim_struct_access T f) (p'::nil)) ->
      tr_trm gt (trm_app (prim_struct_access T f) (p::nil)) r
  (* Args *)
  | tr_trm_args1 : forall op t1 t1',
      ~ is_prim_struct_access op ->
      tr_trm gt t1 t1' ->
      tr_trm gt (trm_app op (t1::nil)) (trm_app op (t1'::nil))
  | tr_trm_args2 : forall op t1 t1' t2 t2',
      tr_trm gt t1 t1' ->
      tr_trm gt t2 t2' ->
      tr_trm gt (trm_app op (t1::t2::nil)) (trm_app op (t1'::t2'::nil)).


(** Transformation of stacks: S ~ |S| *)

Inductive tr_stack_item (gt:group_tr) : (var * val) -> (var * val) -> Prop :=
  | tr_stack_item_intro : forall x v v',
      tr_val gt v v' -> 
      tr_stack_item gt (x, v) (x, v').

Inductive tr_stack (gt:group_tr) : stack -> stack -> Prop :=
  | tr_stack_intro : forall S S',
      LibList.Forall2 (tr_stack_item gt) S S' ->
      tr_stack gt S S'.

Lemma stack_lookup_tr : forall gt S S' x v,
  tr_stack gt S S' ->
  Ctx.lookup x S = Some v -> 
    exists v', 
       Ctx.lookup x S' = Some v' 
    /\ tr_val gt v v'.
Proof.
  introv HS Hx. inverts HS as HS. induction HS.
  { inverts Hx. }
  { inverts H as Hv. inverts Hx as Hx. case_if in Hx.
    { inverts Hx. exists v'. splits*. unfolds. case_if*. }
    { forwards (v''&Hx'&Hv''): IHHS Hx. exists v''.
      splits*. unfolds. case_if. fold Ctx.lookup. auto. } }
Qed.


(** Transformation of states: m ~ |m| *)

Inductive tr_state (gt:group_tr) : state -> state -> Prop :=
  | tr_state_intro : forall m m',
      dom m = dom m' ->
      (forall l,
        l \indom m ->
        tr_val gt m[l] m'[l]) ->
      tr_state gt m m'.


(** Transformation of typdefctxs: C ~ |C| *)

Inductive tr_typdefctx (gt:group_tr) : typdefctx -> typdefctx -> Prop :=
  | tr_typdefctx_intro : forall Tt fs Tg fg C C',
      gt = make_group_tr Tt fs Tg fg ->
      Tt \indom C ->
      Tg \notindom C ->
      dom C' = dom C \u \{Tg} ->
      (forall T,
        T \indom C ->
        T <> Tt ->
        C'[T] = C[T]) ->
      fs \c dom (C[Tt]) ->
      fg \notindom (C[Tt]) ->
      dom (C'[Tt]) = (dom (C[Tt]) \- fs) \u \{fg} ->
      (forall f,
        f \indom (C[Tt]) ->
        f \notin fs ->
        C'[Tt][f] = C[Tt][f]) ->
      C'[Tt][fg] = typ_struct Tg ->
      dom (C'[Tg]) = fs ->
      (forall f,
        f \indom (C'[Tg]) ->
        C'[Tg][f] = C[Tt][f]) ->
      tr_typdefctx gt C C'.
