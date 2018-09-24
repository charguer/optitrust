(**

This file describes the grouping transformation relations as a function.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)


Set Implicit Arguments.
Require Export Semantics LibSet LibMap TLCbuffer TrGroup.


(* ---------------------------------------------------------------------- *)
(** Explicit definitions *)

Fixpoint fun_tr_accesses (gt:group_tr) (π:accesses) : accesses :=
  let aux := fun_tr_accesses gt in
  match π with
  | nil => nil
  | ((access_array T i)::π') => ((access_array T i)::(aux π'))
  | ((access_field T f)::π') => 
      if isTrue(T = typ_var (group_tr_struct_name gt)) then
        if isTrue(f \in group_tr_fields gt) then
          let Tg := typ_var (group_tr_new_struct_name gt) in
          let fg := group_tr_new_struct_field gt in
          ((access_field T fg)::(access_field Tg f)::(aux π'))
        else
          ((access_field T f)::(aux π'))
      else
        ((access_field T f)::(aux π'))
  end.

Fixpoint fun_tr_val_depth (depth:nat) (gt:group_tr) (v:val) : val :=
  match depth with
    | O => v
    | S n =>
      let aux := fun_tr_val_depth n gt in
      match v with
      | val_error => val_error
      | val_uninitialized => val_uninitialized
      | val_unit => val_unit
      | val_bool b => val_bool b
      | val_int i => val_int i
      | val_double d => val_double d
      | val_abstract_ptr l π => val_abstract_ptr l (fun_tr_accesses gt π)
      | val_concrete_ptr l o => val_concrete_ptr l o (* TODO: This is not correct. *)
      | val_array T nil => val_array T nil
      | val_array T vs => (val_array T (LibList.map aux vs)) 
      | val_struct T s =>
          let m : monoid_op (map field val) := monoid_make (fun a b => a \u b) \{} in
          let g : field -> val -> map field val := fun f v => (\{})[f:=(aux v)] in
          if isTrue(T=typ_var (group_tr_struct_name gt)) then
            let Tg := typ_var (group_tr_new_struct_name gt) in
            let fg := group_tr_new_struct_field gt in
            let fs := group_tr_fields gt in
            let s' := fold m g s in
            let g1' : field -> val -> map field val := 
              fun f v => if (isTrue(f \in fs)) then (\{}) else (\{})[f:=v] in
            let g2' : field -> val -> map field val := 
              fun f v => if (isTrue(f \in fs)) then (\{})[f:=v] else (\{}) in
            let s1' := fold m g1' s' in
            let s2' := fold m g2' s' in
            val_struct T (s1'[fg:=(val_struct Tg s2')])
          else
            val_struct T (fold m g s)
        | val_words lw => val_words lw (* TODO: Not correct either. *)
      end
   end.


(* ---------------------------------------------------------------------- *)
(** Lemmas outline. Some ideas. *)

Lemma tr_val_fun_tr_val : forall gt n v,
  fun_tr_val_depth n gt v = fun_tr_val_depth (S n) gt v ->
  tr_val gt v (fun_tr_val_depth n gt v).
Proof using.
Admitted.

Lemma fixpoint_fun_tr_val : forall gt v,
  exists n, fun_tr_val_depth n gt v = fun_tr_val_depth (S n) gt v.
Proof using.
Admitted.

Lemma depth_fun_tr_val : forall gt v,
  exists n, tr_val gt v (fun_tr_val_depth n gt v).
Proof using.
  intros. forwards (n&H): fixpoint_fun_tr_val gt v.
  exists n. applys tr_val_fun_tr_val. 
  unfolds fun_tr_val_depth. auto.
Qed.

Lemma total_tr_accesses : forall gt π,
  exists π', tr_accesses gt π π'.
Proof using.
  induction π; eauto. destruct a.  
  { inverts IHπ; exists __. constructors*.  }
  { tests: (t = typ_var (group_tr_struct_name gt)).
    { tests: (f \in group_tr_fields gt).
      { inverts IHπ. destruct gt. exists __. 
        applys* tr_accesses_field_group. }
      { inverts IHπ. destruct gt. exists __. 
        applys* tr_accesses_field_other. } }
    { inverts IHπ; exists __. applys* tr_accesses_field_other. } }
Qed.

Lemma total_tr_val : forall gt v,
  exists v', tr_val gt v v'.
Proof using.
  intros. forwards* (n&H): depth_fun_tr_val gt v.
Qed.
