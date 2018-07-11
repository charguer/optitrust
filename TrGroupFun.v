(**

This file describes grouping transformation relations as functions.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export Semantics LibSet LibMap TLCbuffer TrGroup.


(* ---------------------------------------------------------------------- *)
(** Hints *)

(* TODO: Factor this out. *)
Lemma index_of_index_length : forall A (l' l : list A) i,
  index l' i ->
  length l' = length l ->
  index l i.
Proof.
  intros. rewrite index_eq_index_length in *.
  applys* index_of_index_length'.
Qed.

Hint Resolve index_of_index_length.

Hint Constructors red.
Hint Constructors tr_trm tr_val tr_accesses tr_state tr_stack.
Hint Constructors read_accesses write_accesses.


(* ---------------------------------------------------------------------- *)
(** Functionality of the relations *)

Theorem functional_tr_accesses : forall gt π π1 π2,
  tr_accesses gt π π1 ->
  tr_accesses gt π π2 ->
    π1 = π2.
Proof.
  introv H1 H2. gen π2. induction H1; intros;
  inverts_head tr_accesses; repeat fequals*;
  inverts_head Logic.or; repeat fequals*.
Qed. 

Theorem functional_tr_val : forall gt v v1 v2,
  tr_val gt v v1 ->
  tr_val gt v v2 ->
  v1 = v2.
Proof using.
  introv H1 H2. gen v2. induction H1; intros;
  inverts_head tr_val; fequals*.
  { applys* functional_tr_accesses. }
  { applys* eq_of_extens. math. }
  { applys read_extens. 
    { inverts_head make_group_tr'. congruence. }
    { introv Hin. tests C: (i = fg).
      { inverts_head make_group_tr'.
        asserts_rewrite~ (s'0[fg0] = val_struct Tsg0 sg0).
        asserts_rewrite~ (s'[fg0] = val_struct Tsg0 sg).
        fequals. applys~ read_extens. introv Hk. 
        asserts_rewrite* (dom sg = dom sg0) in *. }
      { inverts_head make_group_tr'.
        asserts_rewrite~ (dom s' = dom s \- dom sg \u '{fg0}) in Hin.
        inverts Hin as Hin; tryfalse. inverts Hin as Hin Hnotin.
        asserts_rewrite* (dom sg = dom sg0) in *. } } }
  { subst. simpls. contradiction. }
  { applys read_extens.
    { congruence. }
    { introv Hin. 
      asserts_rewrite* (dom s' = dom s) in *. } }
Qed.

Theorem functional_tr_trm : forall gt t t1 t2,
  tr_trm gt t t1 ->
  tr_trm gt t t2 ->
  t1 = t2.
Proof.
  introv H1 H2. gen t2. induction H1; intros;
  try solve [ inverts H2 ; try subst ; repeat fequals* ].
  { inverts H2. fequals. applys* functional_tr_val. }
  { inverts_head tr_trm; subst. 
    { repeat fequals~. }
    { inverts_head Logic.or; tryfalse. } 
    { unfolds is_prim_struct_access. contradiction. } }
  { inverts_head tr_trm; subst.
    { inverts_head Logic.or; tryfalse. }
    { repeat fequals~. } 
    { unfolds is_prim_struct_access. contradiction. } }
  { inverts_head tr_trm; 
    try solve [ unfolds is_prim_struct_access ; contradiction ].
    repeat fequals~. }
Qed.

Theorem functional_tr_stack_item : forall gt i i1 i2,
  tr_stack_item gt i i1 ->
  tr_stack_item gt i i2 ->
  i1 = i2.
Proof.
  introv Hi1 Hi2. 
  inverts Hi1 as H. inverts Hi2 as H'. 
  forwards*: functional_tr_val H H'.
Qed.

Theorem functional_tr_stack : forall gt S S1 S2,
  tr_stack gt S S1 ->
  tr_stack gt S S2 ->
  S1 = S2.
Proof.
  introv HS1 HS2. inverts HS1 as HS1. inverts HS2 as HS2. 
  gen S2. induction HS1; intros.
  { inverts~ HS2. }
  { inverts HS2 as HSy0 HS1'. fequals.
    { forwards*: functional_tr_stack_item H HSy0. }
    { applys~ IHHS1. } }
Qed.

Theorem functional_tr_state : forall gt m m1 m2,
  tr_state gt m m1 ->
  tr_state gt m m2 ->
  m1 = m2.
Proof.
  introv Hm1 Hm2. 
  inverts Hm1 as HD1 Htr1. inverts Hm2 as HD2 Htr2.
  applys read_extens.
  { unfolds state. congruence. }
  { introv Hi. rewrite HD1 in *. 
    forwards Hm1i: Htr1 Hi. forwards Hm2i: Htr2 Hi.
    forwards~: functional_tr_val Hm1i Hm2i. }
Qed.


(* ---------------------------------------------------------------------- *)
(** Explicit definitions *)

Fixpoint fun_tr_accesses (gt:group_tr) (π:accesses) : accesses :=
  let aux := fun_tr_accesses gt in
  match π with
  | nil => nil
  | ((access_array i)::π') => ((access_array i)::(aux π'))
  | ((access_field T f)::π') => 
      if isTrue(T = group_tr_struct_name gt) then
        if isTrue(f \in group_tr_fields gt) then
          let Tg := group_tr_new_struct_name gt in
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
      | val_array nil => val_array nil
      | val_array vs => (val_array (LibList.map aux vs)) 
      | val_struct T s =>
          let m : monoid_op (map field val) := monoid_make (fun a b => a \u b) \{} in
          let g : field -> val -> map field val := fun f v => (\{})[f:=(aux v)] in
          if isTrue(T=group_tr_struct_name gt) then
            let Tg := group_tr_new_struct_name gt in
            let fg := group_tr_new_struct_field gt in
            let fs := group_tr_fields gt in
            let s' := fold m g s in
            let g1' : field -> val -> map field val := 
              fun f v => if (isTrue(f \in fs)) then (\{}) else (\{})[f:=v] in
            let g2' : field -> val -> map field val := 
              fun f v => if (isTrue(f \in fs)) then (\{})[f:=v] else (\{}) in
            let s1' := fold m g1' s in
            let s2' := fold m g2' s in
            val_struct T (s1'[fg:=(val_struct Tg s2')])
          else
            val_struct T (fold m g s)
      end
   end.


(* ---------------------------------------------------------------------- *)
(** Lemmas outline *)

Lemma fixpoint_fun_tr_val : forall gt n v,
  fun_tr_val_depth n gt v = fun_tr_val_depth (S n) gt v ->
  tr_val gt v (fun_tr_val_depth n gt v).
Proof.
Admitted.

Lemma depth_fun_tr_val : forall gt v,
  exists n, tr_val gt v (fun_tr_val_depth n gt v).
Proof.
Admitted.

Lemma total_tr_accesses : forall gt π,
  exists π', tr_accesses gt π π'.
Proof.
  induction π; eauto. destruct a.  
  { inverts IHπ; exists __. constructors*.  }
  { tests: (t = group_tr_struct_name gt).
    { tests: (f \in group_tr_fields gt).
      { inverts IHπ; exists __. applys* tr_accesses_field_group. }
      { inverts IHπ; exists __. applys* tr_accesses_field_other. } }
    { inverts IHπ; exists __. applys* tr_accesses_field_other. } }
Qed.

Lemma total_tr_val : forall gt v,
  exists v', tr_val gt v v'.
Proof.
  intros. forwards* (n&H): depth_fun_tr_val gt v.
Qed.

(*Lemma tr_array : forall gt (a:list val),
  exists a', 
        length a' = length a
    /\  (forall i, 
          index a' i -> 
          tr_val gt a[i] a'[i]).
Proof.
  induction a.
  { exists __. rewrite length_nil in *. splits~. introv Hi.
    inverts Hi. rewrite LibList.length_nil in *. math. }
  { inverts IHa as (IHl&IHx). forwards (v'&Hv'): total_tr_val gt a.
    exists (v'::x). splits.
    { repeat rewrite length_cons. fequals. }
    { introv Hi. tests: (i = 0).
      { repeat rewrite~ read_zero. }
      { repeat rewrite read_cons_case. case_if*. 
        inverts Hi. forwards~: IHx (i - 1).
        applys int_index_prove. math.  
        rewrite LibList.length_cons in *. math. } } }
Qed.*)
