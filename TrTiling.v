(**

This file describes transformations of the layout of records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export Semantics LibSet LibMap TLCbuffer.

(* ********************************************************************** *)
(* * Definition of the transformation *)

(** Tiling transformation. Specified by:
    - The typvar of the array to be tiled.
    - The new typvar of the tiles.
    - The size of the tiles. *)

Record tiling_tr := make_tiling_tr {
  tiling_tr_array_name : typvar;
  tiling_tr_tile_name : typvar;
  tiling_tr_tile_size : size
}.

Notation make_tiling_tr' := make_tiling_tr.

(** Checking if the transformation is acceptable *)

Inductive tiling_tr_ok : tiling_tr -> typdefctx -> Prop :=
  | tiling_tr_ok_intros : forall Ta Tt n T os tt C,
      tt = make_tiling_tr Ta Tt n ->
      Ta \indom C ->
      C[Ta] = typ_array T os ->
      Tt \notindom C ->
      (forall Tv,
        Tv \indom C ->
        Tv <> Ta ->
        ~ free_typvar C Tt C[Tv]) ->
      tiling_tr_ok tt C.


(* ********************************************************************** *)
(* * The transformation applied to the different constructs. *)

(** Transformation of typdefctxs: C ~ |C| *)

Inductive tr_typdefctx (tt:tiling_tr) : typdefctx -> typdefctx -> Prop :=
  | tr_typdefctx_intro : forall T Tt Ta k os os' C C',
      tt = make_tiling_tr Ta Tt k ->
      dom C' = dom C \u \{Tt} ->
      C[Ta] = typ_array T os ->
      C'[Ta] = typ_array (typ_var Tt) os' ->
      C'[Tt] = typ_array T (Some k) ->
      (forall Tv,
        Tv \indom C ->
        Tv <> Ta ->
        C'[Tv] = C[Tv]) ->
      (forall n,
        os = Some n ->
        os' = Some (n / k)) ->
      (os = None ->
        os' = None) ->
      tr_typdefctx tt C C'.

(** Transformation of paths: π ~ |π| *)

Inductive tr_accesses (tt:tiling_tr) : accesses -> accesses -> Prop :=
  | tr_accesses_nil :
      tr_accesses tt nil nil
  | tr_accesses_array_tiling : forall π π' Ta Tt i n a0 a1 a2,
      tr_accesses tt π π' ->
      tt = make_tiling_tr Ta Tt n ->
      a0 = access_array (typ_var Ta) i ->
      a1 = access_array (typ_var Ta) (i/n) ->
      a2 = access_array (typ_var Tt) (i mod n) ->
      tr_accesses tt (a0::π) (a1::a2::π')
  | tr_accesses_array_other : forall π π' T i,
      T <> typ_var (tiling_tr_array_name tt) ->
      tr_accesses tt π π' ->
      tr_accesses tt ((access_array T i)::π) ((access_array T i)::π')
  | tr_accesses_field : forall T π π' f,
      tr_accesses tt π π' ->
      tr_accesses tt ((access_field T f)::π) ((access_field T f)::π').

(** Transformation of values: v ~ |v| *)

Inductive tr_val (tt:tiling_tr) : val -> val -> Prop :=
  | tr_val_error :
      tr_val tt val_error val_error
  | tr_val_uninitialized :
      tr_val tt val_uninitialized val_uninitialized
  | tr_val_unit :
      tr_val tt val_unit val_unit
  | tr_val_bool : forall b,
      tr_val tt (val_bool b) (val_bool b)
  | tr_val_int : forall i,
      tr_val tt (val_int i) (val_int i)
  | tr_val_double : forall d,
      tr_val tt (val_double d) (val_double d)
  | tr_val_abstract_ptr : forall l π π',
      tr_accesses tt π π' ->
      tr_val tt (val_abstract_ptr l π) (val_abstract_ptr l π')
  | tr_val_array_tiling : forall (l:nat) k Tt Ta a a',
      tt = make_tiling_tr Ta Tt k ->
      length a = l ->
      length a' = l / k ->
      (forall i a'',
        index a' i ->
        a'[i] = val_array (typ_var Tt) a'' ->
        length a'' = k ->
        (forall j,
          index a'' j ->
          tr_val tt a[i*(length a')+j] a''[j])) ->
      tr_val tt (val_array (typ_var Ta) a) (val_array (typ_var Ta) a')
  | tr_val_array_other : forall T a a',
      T <> typ_var (tiling_tr_array_name tt) ->
      length a = length a' ->
      (forall i,
        index a i ->
        tr_val tt a[i] a'[i]) ->
      tr_val tt (val_array T a) (val_array T a')
  | tr_val_struct_other : forall T s s',
      dom s = dom s' ->
      (forall f,
        f \indom s ->
        tr_val tt s[f] s'[f]) ->
      tr_val tt (val_struct T s) (val_struct T s').

Definition is_array_access (op:prim) :=
  match op with
  | prim_array_access _ => True
  | prim_array_get _ => True
  | _ => False
  end.


(* Transformation used in the struct cases to avoid repetition. *)

Inductive tr_array_op (tt:tiling_tr) : trm -> trm -> Prop :=
  | tr_array_op_tiling : forall Tt k op1 op2 ta1 ta2 tlk tlj tli pr Ta t1 t2 tlt,
      pr = prim_array_access \/ pr = prim_array_get ->
      tt = make_tiling_tr Ta Tt k ->
      (* let t = t1 in
         let i = t2 in
         let j = i / k in
         let k = i % k in
           t[j][k] *)
      op1 = pr (typ_var Ta) ->
      op2 = pr (typ_var Tt) ->
      ta1 = trm_app op1 ((trm_var "t")::(trm_var "j")::nil) ->
      ta2 = trm_app op2 (ta1::(trm_var "k")::nil) ->
      tlk = trm_let "k" (trm_app binop_mod ((trm_var "i")::(trm_val (val_int k))::nil)) ta2 ->
      tlj = trm_let "j" (trm_app binop_div ((trm_var "i")::(trm_val (val_int k))::nil)) tlk ->
      tli = trm_let "i" t2 tlj ->
      tlt = trm_let "t" t1 tli ->
      tr_array_op tt (trm_app (pr (typ_var Ta)) (t1::t2::nil)) tlt
  | tr_array_op_other : forall Ta pr T ts,
      pr = prim_array_access \/ pr = prim_array_get ->
      Ta = tiling_tr_array_name tt ->
      T <> (typ_var Ta) ->
      tr_array_op tt (trm_app (pr T) ts) (trm_app (pr T) ts).

(** Transformation of terms: t ~ |t| *)

Inductive tr_trm (tt:tiling_tr) : trm -> trm -> Prop :=
  | tr_trm_val : forall v v',
      tr_val tt v v' ->
      tr_trm tt (trm_val v) (trm_val v')
  | tr_trm_var : forall x,
      tr_trm tt (trm_var x) (trm_var x)
  | tr_trm_if : forall t1 t2 t3 t1' t2' t3',
      tr_trm tt t1 t1' ->
      tr_trm tt t2 t2' ->
      tr_trm tt t3 t3' ->
      tr_trm tt (trm_if t1 t2 t3) (trm_if t1' t2' t3')
  | tr_trm_let : forall x t1 t2 t1' t2',
      tr_trm tt t1 t1' ->
      tr_trm tt t2 t2' ->
      tr_trm tt (trm_let x t1 t2) (trm_let x t1' t2')
  (* new *)  
  | tr_trm_new : forall T,
      tr_trm tt (trm_app (prim_new T) nil) (trm_app (prim_new T) nil)
  (* Special case: array access *)
  | tr_trm_array : forall t1' t2' op t1 t2 tr,
      is_array_access op ->
      tr_trm tt t1 t1' ->
      tr_trm tt t2 t2' ->
      tr_array_op tt (trm_app op (t1'::t2'::nil)) tr ->
      tr_trm tt (trm_app op (t1::t2::nil)) tr
  (* Args *)
  | tr_trm_args1 : forall op t1 t1',
      tr_trm tt t1 t1' ->
      tr_trm tt (trm_app op (t1::nil)) (trm_app op (t1'::nil))
  | tr_trm_args2 : forall op t1 t1' t2 t2',
      ~ is_array_access op ->
      tr_trm tt t1 t1' ->
      tr_trm tt t2 t2' ->
      tr_trm tt (trm_app op (t1::t2::nil)) (trm_app op (t1'::t2'::nil)).


(** Transformation of stacks: S ~ |S| *)

Inductive tr_stack_item (tt:tiling_tr) : (var * val) -> (var * val) -> Prop :=
  | tr_stack_item_intro : forall x v v',
      tr_val tt v v' -> 
      tr_stack_item tt (x, v) (x, v').

Inductive tr_stack (tt:tiling_tr) : stack -> stack -> Prop :=
  | tr_stack_intro : forall S S',
      LibList.Forall2 (tr_stack_item tt) S S' ->
      tr_stack tt S S'.

Lemma stack_lookup_tr : forall tt S S' x v,
  tr_stack tt S S' ->
  Ctx.lookup x S = Some v -> 
    exists v', 
       Ctx.lookup x S' = Some v' 
    /\ tr_val tt v v'.
Proof.
  introv HS Hx. inverts HS as HS. induction HS.
  { inverts Hx. }
  { inverts H as Hv. inverts Hx as Hx. case_if in Hx.
    { inverts Hx. exists v'. splits*. unfolds. case_if*. }
    { forwards (v''&Hx'&Hv''): IHHS Hx. exists v''.
      splits*. unfolds. case_if. fold Ctx.lookup. auto. } }
Qed.


(** Transformation of states: m ~ |m| *)

Inductive tr_state (tt:tiling_tr) : state -> state -> Prop :=
  | tr_state_intro : forall m m',
      dom m = dom m' ->
      (forall l,
        l \indom m ->
        tr_val tt m[l] m'[l]) ->
      tr_state tt m m'.
