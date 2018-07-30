(**

This file describes transformations of the layout of records and arrays.

Author: Ramon Fernandez I Mir and Arthur Charguéraud.

License: MIT.

*)

Set Implicit Arguments.
Require Export Semantics LibSet LibMap LibList TLCbuffer Typing.

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
      (forall i,
        index a' i ->
        exists a'',
            a'[i] = val_array (typ_var Tt) a''
        /\  length a'' = k) ->
      (forall i a'',
        index a' i ->
        a'[i] = val_array (typ_var Tt) a'' ->
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

(** Transformation of states: m ~ |m| *)

Inductive tr_state (tt:tiling_tr) : state -> state -> Prop :=
  | tr_state_intro : forall m m',
      dom m = dom m' ->
      (forall l,
        l \indom m ->
        tr_val tt m[l] m'[l]) ->
      tr_state tt m m'.


(* ********************************************************************** *)
(* * Preservation of semantics proof. *)

(* These lemmas are common with all transformations. *)
Section CommonResults.

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

Lemma tr_stack_add : forall tt z v S v' S',
  tr_stack tt S S' ->
  tr_val tt v v' ->
  tr_stack tt (Ctx.add z v S) (Ctx.add z v' S').
Proof.
  introv HS Hv. constructors~. inverts HS.
  unfolds Ctx.add. destruct* z.
  applys~ Forall2_cons. constructors~.
Qed.

Lemma not_tr_val_error : forall tt v1 v2,
  tr_val tt v1 v2 ->
  ~ is_error v2.
Proof.
  introv Hv He. unfolds is_error. destruct* v2. inverts Hv.
Qed.

Lemma not_is_val_tr : forall tt t1 t2,
  tr_trm tt t1 t2 ->
  ~ is_val t1 ->
  ~ is_val t2.
Proof.
  introv Htr Hv. induction Htr; introv HN;
  try solve [ subst ; inverts HN ]. 
  forwards*: Hv.
  inverts H0; inverts HN. (* Changes here. *)
Qed.

Lemma not_is_error_tr : forall tt v1 v2,
  tr_val tt v1 v2 ->
  ~ is_error v1 ->
  ~ is_error v2.
Proof.
  introv Htr He. induction Htr; introv HN;
  try solve [ subst ; inverts HN ; forwards*: He ]. (* Changes here. *) 
Qed.

Lemma not_is_uninitialized_tr : forall tt v v',
  tr_val tt v v' -> 
  ~ is_uninitialized v ->
  ~ is_uninitialized v'.
Proof.
  introv Htr Hu. induction Htr; introv HN;
  subst; inverts HN. forwards~: Hu.
Qed.

End CommonResults.


Theorem functional_tr_accesses : forall tt π π1 π2,
  tr_accesses tt π π1 ->
  tr_accesses tt π π2 ->
    π1 = π2.
Proof.
  introv H1 H2. gen π2. induction H1; intros;
  inverts_head tr_accesses; repeat fequals*;
  inverts_head access_array; subst; simpls; tryfalse.
Qed.

Hint Resolve TLCbuffer.index_of_index_length.

Theorem functional_tr_val : forall tt v v1 v2,
  tr_val tt v v1 ->
  tr_val tt v v2 ->
  v1 = v2.
Proof using.
  introv H1 H2. gen v2. induction H1; intros;
  inverts_head tr_val; fequals*; subst; simpls; tryfalse.
  { applys* functional_tr_accesses. }
  { asserts Hl: (length a' = length a'0).
    { inverts_head make_tiling_tr'.
      rewrite H1. rewrite H10. applys eq_int_of_eq_nat.
      rewrite length_eq in *.
      forwards Heq1: eq_nat_of_eq_int H0.
      forwards Heq2: eq_nat_of_eq_int H9.
      rewrite <- Heq1. rewrite~ <- Heq2. }
    applys* eq_of_extens. inverts_head make_tiling_tr'.
    introv Hi.
    asserts Hi': (index a'0 i).
    { rewrite index_eq_index_length in *. rewrite~ <- Hl. }
    forwards* (a1''&Ha1''i&Hla1''): H11 i.
    forwards* (a2''&Ha2''i&Hla2''): H2 i.
    rewrite Ha1''i. rewrite Ha2''i. fequals.
    asserts Hl': (length a1'' = length a2'').
    { congruence. }
    applys~ eq_of_extens. introv Hi0. applys~ H4 i.
    rewrite Hl. applys~ H13.
    { rewrite index_eq_index_length in *. rewrite~ Hl'. } }
  { applys eq_of_extens. 
    { congruence. }
    { introv Hi. asserts: (index a i).
      { rewrite index_eq_index_length in *. rewrite~ H0. }
      applys* H2. } }
  { applys read_extens.
    { congruence. }
    { introv Hin. 
      asserts_rewrite* (dom s' = dom s) in *. } }
Qed.

Axiom div_mod_eq : forall i j k:Z, 
  (i / k)%Z = (j / k)%Z ->
  (i mod k)%Z = (j mod k)%Z ->
  i = j.

Lemma tr_accesses_inj : forall C tt π π1 π2,
  tiling_tr_ok tt C ->
  valid_accesses C π1 ->
  valid_accesses C π2 ->
  tr_accesses tt π1 π ->
  tr_accesses tt π2 π ->
    π1 = π2.
Proof.
  introv Hok Hva1 Hva2 Hπ1 Hπ2. gen C π2. induction Hπ1; intros.
  { inverts Hπ2. auto. }
  { subst. inverts Hπ2; inverts Hva1; inverts Hva2.
    { inverts_head make_tiling_tr'. repeat inverts_head access_array.
      repeat fequals*. applys* div_mod_eq. }
    { simpls. false. } }
  { inverts Hπ2; inverts Hva1; inverts Hva2.
    { inverts_head access_array. fequals*. }
    { fequals*. } }
  { inverts Hπ2; inverts Hva1; inverts Hva2.
    { inverts_head access_array. }
    { fequals*. } }
Qed.

Lemma tr_val_inj : forall C tt v v1 v2,
  tiling_tr_ok tt C ->
  valid_val C v1 ->
  valid_val C v2 ->
  tr_val tt v1 v ->
  tr_val tt v2 v ->
  v1 = v2.
Proof.
  introv Hok HV1 HV2 Hv1 Hv2. gen C v2. induction Hv1; intros;
  try solve [ inverts Hv2; repeat fequals*; subst; simpls; tryfalse* ].
  { inverts Hv2 as Hπ. fequals*.
    inverts HV1 as HRφ1. inverts HV2 as HRφ2.
    applys* tr_accesses_inj. }
Admitted.

Lemma tr_val_inj_cp : forall C tt v1 v2 v1' v2',
  tiling_tr_ok tt C ->
  valid_val C v1 ->
  valid_val C v2 ->
  tr_val tt v1 v1' ->
  tr_val tt v2 v2' ->
  v1 <> v2 ->
  v1' <> v2'.
Proof.
  introv Hok HTv1 HTv2 Hv1 Hv2 Hneq HN. subst. 
  forwards*: tr_val_inj Hok HTv1 HTv2 Hv1.
Qed.

Axiom index_div : forall (l:nat) (k:size) (i:int),
  index (nat_to_Z l) i ->
  index (nat_to_Z (l/k)) ((i/k)%Z).


Section TransformationsProofs.

Hint Constructors red redbinop.
Hint Constructors read_accesses write_accesses.
Hint Constructors tr_trm tr_val tr_accesses tr_state tr_stack.
Hint Constructors valid_trm valid_prim valid_val.

Hint Resolve red_valid.

Lemma tr_read_accesses : forall gt v π v' π' w,
  tr_val gt v v' ->
  tr_accesses gt π π' ->
  read_accesses v π w ->
  (exists w',
      tr_val gt w w'
  /\  read_accesses v' π' w').
Proof.
  introv Hv Ha HR. gen gt v' π'. induction HR; intros.
  { (* nil *)
    inverts Ha. exists~ v'. }
  { (* array_access *)
    inverts Ha as.
    { (* tiling array *) 
      introv Hπ Heq. inverts Heq.
      inverts Hv as.
      2:{ introv HN. simpls. false. }
      introv Heq Hla Hla' Ha'' Htrv.
      inverts Heq.
      asserts Hia': (index a' (i0 / k)%Z).
      { rewrite index_eq_index_length in *.
        rewrite Hla'. rewrite Hla in H.
        applys~ index_div. }
      forwards~ (a''&Ha'i&Hla''): Ha'' ((i0/k)%Z).
      asserts Hia'': (index a'' (i0 mod k)%Z).
      { admit. }
      forwards*: Htrv ((i0/k)%Z) a'' ((i0 mod k)%Z).
 } }
    { (* other array *) } }
  { (* struct_access *)
     }
Qed.




Theorem red_tr: forall tt C C' t t' v S S' m1 m1' m2,
  wf_typdefctx C ->
  tiling_tr_ok tt C ->
  tr_typdefctx tt C C' ->
  tr_trm tt t t' ->
  tr_stack tt S S' ->
  tr_state tt m1 m1' ->
  red C S m1 t m2 v ->
  valid_stack C S ->
  valid_state C m1 ->
  valid_trm C t ->
  ~ is_error v ->
  exists v' m2',
      tr_val tt v v'
  /\  tr_state tt m2 m2'
  /\  red C' S' m1' t' m2' v'.
Proof.
  introv Hwf Hok HC Ht HS Hm1 HR HVS HVm1 HV. introv He. gen tt C' t' S' m1'.
  induction HR; intros; try solve [ forwards*: He; unfolds* ].
  { (* val *) 
    inverts Ht as Hv. exists* v' m1'. }
  { (* var *) 
    inverts Ht. forwards* (v'&H'&Hv'): stack_lookup_tr HS H. exists* v' m1'. }
  { (* if *)
    inverts Ht as Hb HTrue HFalse. inverts HV as HV0 HV1 HV2.
    forwards* (v'&m2'&Hv'&Hm2'&HR3): IHHR1 Hb HS Hm1.
    inverts* Hv'. destruct b;
    forwards* (vr'&m3'&Hvr'&Hm3'&HR4): IHHR2 HS Hm2';
    forwards*: red_valid HR1; exists* vr' m3'. }
  { (* let *)
    inverts Ht as Ht1 Ht2. inverts HV as HV0 HV1.
    forwards* (v'&m2'&Hv'&Hm2'&HR3): IHHR1 Ht1 HS Hm1.
    forwards HS': tr_stack_add z HS Hv'.
    forwards: not_tr_val_error Hv'.
    forwards* (vr'&m3'&Hvr'&Hm3'&HR4): IHHR2 Ht2 HS' Hm2'.
    { applys~ stack_valid_add. applys* red_valid HR1. }
    { applys* red_valid HR1. }
    exists* vr' m3'. }
  { (* binop *)
    inverts Ht as.
    { introv Hop. inverts Hop. }
    { introv Hop Ht1 Ht2. inverts Ht1 as Ht1. inverts Ht2 as Ht2.
      inverts H1;
      try solve [ exists __ m1' ; splits~ ; inverts Ht1 ;
      inverts Ht2 ; repeat constructors~ ].
      { exists __ m1'. splits~. constructors.
        forwards: functional_tr_val Ht1 Ht2. subst.
        applys* not_is_error_tr. applys* not_is_error_tr.
        constructors~. applys* functional_tr_val. }
      { exists __ m1'. splits~. constructors.
        applys* not_is_error_tr. applys* not_is_error_tr.
        inverts HV as HVprim HV1 HV2.
        inverts HV1 as HV1. inverts HV2 as HV2.
        forwards*: tr_val_inj_cp H2. } } }
  { (* get *)
    inverts Ht as Ht1'. subst.
    inverts Ht1' as Ht1'. inverts Ht1' as Hπ.
    inverts Hm1 as HD Htrm.
    inverts H0 as Hi Ha.
    forwards Htrml: Htrm Hi.
    forwards (w'&Hw'&Ha'): tr_read_accesses Htrml Hπ Ha.
    exists w' m1'. splits*.
    repeat constructors~. rewrite~ <- HD.
    applys* not_is_uninitialized_tr. }
Admitted.

End TransformationProofs.






























