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
        ~ free_typvar C Ta C[Tv]) ->
      tiling_tr_ok tt C.


(* ********************************************************************** *)
(* * The transformation applied to the different constructs. *)

(** Transformation of typdefctxs: C ~ |C| *)

Definition nbtiles (n:size) (k:size) (m:size) : Prop :=
  n = (m*k)%nat.

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
      (match os, os' with
      | Some n, Some m => nbtiles n k m
      | None, None => True
      | _,_ => False
      end) ->
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

(* TODO: tr_index *)

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
            a'[i] = (val_array (typ_var Tt) a'')
        /\  length a'' = k) ->
      (forall i a'',
        index a' i ->
        a'[i] = (val_array (typ_var Tt) a'') ->
        (forall j,
          index a'' j ->
              tr_val tt a[i*k+j] a''[j])) ->
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


(* Transformation used in the struct cases to avoid repetition. *)

(* let t = t1 in
   let i = t2 in
   let j = i / k in
   let k = i % k in
     t[j][k] *)
Inductive tr_access (tt:tiling_tr) (pr:typ->prim) (t1:trm) (t2:trm) (tlt:trm) : Prop :=
  | tr_access_intro : forall op1 Ta op2 Tt ta1 ta2 k tlk tlj tli,
      tt = make_tiling_tr Ta Tt k ->
      op1 = pr (typ_var Ta) ->
      op2 = pr (typ_var Tt) ->
      ta1 = trm_app op1 ((trm_var "t")::(trm_var "j")::nil) ->
      ta2 = trm_app op2 (ta1::(trm_var "k")::nil) ->
      tlk = trm_let "k" (trm_app binop_mod ((trm_var "i")::(trm_val (val_int k))::nil)) ta2 ->
      tlj = trm_let "j" (trm_app binop_div ((trm_var "i")::(trm_val (val_int k))::nil)) tlk ->
      tli = trm_let "i" t2 tlj ->
      tlt = trm_let "t" t1 tli ->
      tr_access tt pr t1 t2 tlt.

Inductive tr_array_op (tt:tiling_tr) : trm -> trm -> Prop :=
  | tr_array_op_tiling : forall pr t1 t2 tlt,
      pr = prim_array_access \/ pr = prim_array_get ->
      tr_access tt pr t1 t2 tlt ->
      tr_array_op tt (trm_app (pr (typ_var (tiling_tr_array_name tt))) (t1::t2::nil)) tlt
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
      is_array_op op ->
      tr_trm tt t1 t1' ->
      tr_trm tt t2 t2' ->
      tr_array_op tt (trm_app op (t1'::t2'::nil)) tr ->
      tr_trm tt (trm_app op (t1::t2::nil)) tr
  (* Args *)
  | tr_trm_args1 : forall op t1 t1',
      tr_trm tt t1 t1' ->
      tr_trm tt (trm_app op (t1::nil)) (trm_app op (t1'::nil))
  | tr_trm_args2 : forall op t1 t1' t2 t2',
      ~ is_array_op op ->
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

Lemma is_basic_tr : forall tt v1 v2,
  tr_val tt v1 v2 ->
  is_basic v1 ->
  is_basic v2.
Proof.
  introv Htr Hv1. induction Htr;
  try solve [ inverts Hv1 ];
  constructors~.
Qed.

Lemma not_tr_val_error : forall tt v1 v2,
  tr_val tt v1 v2 ->
  ~ is_error v2.
Proof.
  introv Hv He. unfolds is_error.
  destruct* v2. destruct* b.
  inverts Hv.
Qed.


(* Definition preserves_is_val R := forall t1 t2
  R t1 t2 ->
  ~ is_val t1 ->
  ~ is_val t2.

Lemma tr_preserves_is_val : forall R,  
  R = tr_trm t \/ R = tr_tiling gt ->
  preserves_is_val R.

 *)

Lemma not_is_val_tr_access : forall tt pr t1 t2 tlt,
  tr_access tt pr t1 t2 tlt ->
  ~ is_val tlt.
Proof.
  introv Htra HN. inverts Htra. inverts HN.
Qed.

Lemma not_is_val_tr : forall tt t1 t2,
  tr_trm tt t1 t2 ->
  ~ is_val t1 ->
  ~ is_val t2.
Proof.
  introv Htr Hv. induction Htr; introv HN;
  try solve [ subst ; inverts HN ].
  forwards*: Hv.
  inverts H0 as.
  { introv Hor Htra. applys* not_is_val_tr_access. }
  { introv Hor Hneq. inverts HN. }
Qed.

Lemma not_is_uninitialized_tr : forall tt v v',
  tr_val tt v v' -> 
  ~ is_uninitialized v ->
  ~ is_uninitialized v'.
Proof.
  introv Htr Hu. induction Htr; introv HN;
  subst; inverts HN. forwards~: Hu.
Qed.

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
  { fequals. applys* functional_tr_accesses. }
  { asserts Hl: (length a' = length a'0).
    { inverts_head make_tiling_tr'.
      rewrite H1. rewrite H10. apply eq_int_of_eq_nat.
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
    applys~ H13.
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

(** Results about division and modulo operation. *)
Section DivModResults.

Axiom div_mod_eq : forall i j k:Z, 
  (i / k)%Z = (j / k)%Z ->
  (i mod k)%Z = (j mod k)%Z ->
  i = j.

Axiom index_div : forall (l:nat) (k:size) (i:int),
  index (nat_to_Z l) i ->
  index (nat_to_Z (l/k)) ((i/k)%Z).

Axiom div_plus_mod_eq : forall (i:int) (k:size),
  i = (i/k)*k + (i mod k).

Axiom index_mod : forall (k:nat) (i:int),
  index (nat_to_Z k) ((i mod k)%Z).

Axiom index_mul_plus : forall (l k:size) (i j:int),
  index (nat_to_Z (l / k)) i ->
  index (nat_to_Z k) j ->
  index (nat_to_Z l) (i * k + j)%Z.

End DivModResults.


Lemma tr_accesses_inj : forall C tt π π1 π2,
  tiling_tr_ok tt C ->
  wf_accesses C π1 ->
  wf_accesses C π2 ->
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
  is_basic v1 ->
  is_basic v2 ->
  wf_val C v1 ->
  wf_val C v2 ->
  tr_val tt v1 v ->
  tr_val tt v2 v ->
  v1 = v2.
Proof.
  introv Hok HBv1 HBv2 Hwfv1 Hwfv2 Hv1 Hv2. gen C v2. induction Hv1; intros;
  try solve [ inverts Hv2; repeat fequals*; subst; simpls; tryfalse* ].
  { inverts Hv2 as Hπ. repeat fequals*.
    inverts Hwfv1 as HRφ1. inverts Hwfv2 as HRφ2.
    applys* tr_accesses_inj. }
Qed.

Lemma tr_val_inj_cp : forall C tt v1 v2 v1' v2',
  tiling_tr_ok tt C ->
  is_basic v1 ->
  is_basic v2 ->
  wf_val C v1 ->
  wf_val C v2 ->
  tr_val tt v1 v1' ->
  tr_val tt v2 v2' ->
  v1 <> v2 ->
  v1' <> v2'.
Proof.
  introv Hok HBv1 HBv2 HTv1 HTv2 Hv1 Hv2 Hneq HN. subst.
  forwards*: tr_val_inj Hok HTv1 HTv2 Hv1.
Qed.

Section TransformationsProofs.

Hint Constructors red redbinop.
Hint Constructors read_accesses write_accesses.
Hint Constructors tr_trm tr_val tr_accesses tr_state tr_stack.
Hint Constructors wf_trm wf_prim wf_val.

Hint Resolve wf_red.

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
      { rewrite index_eq_index_length. rewrite Hla''.
        applys~ index_mod. }
      forwards* Hai0: Htrv ((i0/k)%Z) a'' ((i0 mod k)%Z).
      asserts Heq: (i0 = (i0 / k * k + i0 mod k)%Z).
      { applys~ div_plus_mod_eq. }
      rewrite <- Heq in Hai0.
      forwards* (w'&Hvw'&HR'): IHHR.
      exists w'. splits~.
      constructors~. rewrite Ha'i.
      constructors~. }
    { (* absurd case *)
      introv Hneq Hπ. inverts Hv as.
      { intros. simpls. false. }
      { introv _ Hla Htrv.
        forwards Htrv': Htrv H.
        forwards* (w'&Hvw'&HR'): IHHR.
        exists w'. splits~.
        constructors~.
        { rewrite index_eq_index_length in *.
          rewrite~ <- Hla. } } } }
  { (* struct_access *)
    inverts Ha as.
    { introv _ HN. inverts HN. }
    introv Hπ. inverts Hv as HD Hsf.
    forwards~ Htr: Hsf f.
    forwards* (w'&Htrv2w'&HR'): IHHR.
    exists w'. splits~.
    constructors~. rewrite~ <- HD. }
Qed.


Lemma tr_write_accesses : forall v1 w gt π v1' π' w' v2,
  tr_val gt v1 v1' ->
  tr_val gt w w' ->
  tr_accesses gt π π' ->
  write_accesses v1 π w v2 ->
  (exists v2',
        tr_val gt v2 v2'
    /\  write_accesses v1' π' w' v2').
Proof.
  introv Hv1 Hw Hπ HW. gen gt v1' w' π'. induction HW; intros.
  { (* nil *)
    inverts Hπ. exists~ w'. }
  { (* array_access *)
    inverts Hπ as; inverts Hv1 as.
    { (* tiling *)
      introv Htt Hla1 Hla' Ha'i1 Htra1 Hπ Heq.
      inverts Htt. inverts Heq. subst.
      forwards (a''&Ha'i&Hla''): Ha'i1 ((i0/k)%Z).
      { rewrite index_eq_index_length in *. 
        rewrite Hla'. rewrite Hla1 in H.
        applys* index_div. }
      forwards* Htra'': Htra1 ((i0/k)%Z) a'' ((i0 mod k)%Z).
      { rewrite index_eq_index_length in *.
        rewrite Hla'. rewrite Hla1 in H.
        applys* index_div. }
      { rewrite index_eq_index_length in *.
        rewrite Hla''. applys* index_mod. }
      rewrite <- div_plus_mod_eq in Htra''.
      forwards* (v2'&Hv2'&HW'): IHHW.
      remember (val_array (typ_var Tt0) a''[((i0 mod k)%Z):=v2']) as a'''.
      exists (val_array (typ_var Ta0) a'[((i0/k)%Z):=a''']). subst.
      splits.
      { asserts Hex:
          (forall i : int,
            index a'[(i0 / k)%Z:=val_array (typ_var Tt0) a''[(i0 mod k)%Z:=v2']] i ->
            exists a''0,
               a'[(i0 / k)%Z:=val_array (typ_var Tt0) a''[(i0 mod k)%Z:=v2']][i] 
               = val_array (typ_var Tt0) a''0 
            /\ length a''0 = k).
        { introv Hi. 
          asserts Hi': (index a' i).
          { rewrite index_eq_index_length in *.
            rewrite~ length_update in Hi. }
          rew_reads*. intros. subst.
          exists (a''[(i0 mod k)%Z:=v2']). splits*.
          rewrite~ length_update. }
        applys~ tr_val_array_tiling l0.
        { rewrite~ length_update. }
        { rewrite~ length_update. }
        { introv Hi' Hup Hi''.
          forwards* (a''1&Heq&Hla''1): Hex. rewrite Hup in Heq.
          inverts Heq.
          asserts Hi''': (index a1 (i * k + j)%Z).
          { rewrite index_eq_index_length in *.
            rewrite length_update in *. 
            rewrite Hla''1 in Hi''.
            rewrite Hla1. rewrite Hla' in Hi'.
            applys~ index_mul_plus. }
          asserts Hai': (index a' i).
          { admit. }
          asserts Ha''i: (index a'' j).
          { admit. }
          rew_reads~ in Hup.
          { introv Heq. subst. inverts Hup. rew_reads*.
            { admit. (*contradiction*) }
            { admit. (*contradiction*) } }
          { introv Hneq.
            forwards* Htra1': Htra1.
            asserts Hneq': (i*k+j <> i0).
            { admit. }
            rew_reads~. } } }
        { constructors~. admit.
          rewrite Ha'i. constructors~. admit. auto. } }
      { (* absurd case *)
        introv Hneq Hla1 Htra1i1 Hπ Heq. inverts Heq. simpls. false. }
      { (* absurd case *) 
        intros. simpls. false. }
      { (* other array *) 
        introv _ Hla1 Htra1i0 Hneq Hπ. subst.
        forwards* (v2'&Hv2'&HW'): IHHW.
        exists (val_array T a'[i:=v2']). splits.
        { constructors~.
          { repeat rewrite~ length_update. }
          { introv Hi0. rewrite index_update_eq in Hi0. rew_reads*.
            admit. (*index*) } }
        { constructors~. admit. auto. } } }
  { (* struct *) 
    inverts Hπ as; inverts Hv1 as. 
    { (* absurd case *)
      introv _ _ _ HN. inverts HN. }
    { (* any struct *) 
      introv HDs1 Hs1f0 Hπ. subst.
      forwards* (v2'&Hv2'&HW'): IHHW.
      exists (val_struct T s'[f:=v2']). splits.
      { constructors~.
        { repeat rewrite dom_update. congruence. }
        { introv Hf0. Search dom. rewrite* dom_update_at_indom in Hf0.
          rew_reads*. } }
      { constructors~. rewrite~ <- HDs1. auto. } } }
Qed.

(* ---------------------------------------------------------------------- *)
(** The transformation preserves well-founded types. *)

Lemma tr_typdefctx_wf_typ : forall tt C C' T,
  tr_typdefctx tt C C' ->
  wf_typ C T ->
  wf_typ C' T.
Proof.
  introv HC HT. induction HT; try solve [ constructors* ].
  inverts HC as HDC' HCTa HC'Ta HC'Tt HC'Tv Hos.
  constructors.
  { rewrite HDC'. rew_set~. }
  { tests: (Tv=Ta).
    { rewrite HC'Ta. repeat constructors~.
      { rewrite HDC'. rew_set~. }
      { rewrite HC'Tt. constructors~. 
        rewrite HCTa in IHHT.
        inverts~ IHHT. } }
    { rewrite~ HC'Tv. } }
Qed.


(* ---------------------------------------------------------------------- *)
(** uninitialized is coherent with the transformation *)

Lemma tr_typing_struct : forall tt C C' Ts Tfs,
  tr_typdefctx tt C C' ->
  typing_struct C Ts Tfs ->
  typing_struct C' Ts Tfs.
Proof.
  introv HC HTs. induction HTs; intros.
  { constructors~. }
  { inverts HC as HD HCTa HC'Ta HC'Tt HC'Tv _.
    constructors~.
    { rewrite HD. rew_set~. }
    { tests: (Tv=Ta).
      { rewrite HCTa in HTs. inverts HTs. }
      { rewrite~ HC'Tv. } } }
Qed.

Lemma tr_typing_array : forall Tat Tt k C C' Ta T os,
  tr_typdefctx (make_tiling_tr Tat Tt k) C C' ->
  wf_typdefctx C ->
  ~ free_typvar C Tat Ta ->
  typing_array C Ta T os ->
  typing_array C' Ta T os.
Proof.
  introv HC Hwf Hfv HTa. gen Tt Tat k C'. induction HTa; intros.
  { constructors~. applys* tr_typdefctx_wf_typ. }
  { inverts HC as Htt HD HCTa HC'Ta HC'Tt HC'Tv Hos.
    inverts Htt. constructors.
    { rewrite HD. rew_set~. }
    { tests: (Tv=Ta).
      { false. applys~ Hfv. constructors~. }
      { rewrite~ HC'Tv. applys* IHHTa Tt0 Ta k0.
        { introv HN. applys~ Hfv. constructors~. }
        { constructors*. } } } }
Qed.


Lemma tr_uninitialized_val_aux : forall tt v v' T C C',
  tr_typdefctx tt C C' ->
  tiling_tr_ok tt C ->
  wf_typdefctx C ->
  tr_val tt v v' ->
  uninitialized C T v ->
  uninitialized C' T v'.
Proof using.
  introv HC Hok Hwf Hv Hu. gen tt C' v'. induction Hu; intros;
  try solve [ inverts Hv ; constructors~ ].
  { (* array *)
    inverts HC as HD HCTa HC'Ta HC'Tt HC'Tv Hos.
    inverts Hv as.
    { (* tiling array *)
      introv Htt Hla Hla' Ha'i Htra. inverts Htt.
      inverts Hok as Htt HTain HCTa' HTt0nin Hfv.
      inverts Htt. unfolds wf_typdefctx.
      rewrite HCTa in HCTa'. inverts HCTa'.
      inverts H as _ HTCTa.
      rewrite HCTa in HTCTa. inverts HTCTa.
      destruct* os; destruct* os'.
      { applys uninitialized_array (Some (l0/n)).
        3:{ introv Hi. forwards* (a''&Ha'i'&Hla''): Ha'i.
            rewrite Ha'i'. applys uninitialized_array (Some n).
            { constructors.
              { rewrite HD. rew_set~. }
              { rewrite HC'Tt. constructors~.
                applys* tr_typdefctx_wf_typ. constructors*. } }
            { introv Heq. inverts~ Heq. }
            { introv Hi0. forwards* Htra'': Htra i a'' i0.
              applys* H2.
              { admit. (*index. NOTE: not true without assuming perfect tiling. *) }
              { constructors*. }
              { constructors*. } } }
        { constructors.
          { rewrite HD. rew_set~. }
          { rewrite HC'Ta. 
            { asserts_rewrite (s0=l0/n). { admit. (*comes from nbtiles*) }
              constructors. constructors.
              { rewrite HD. rew_set~. }
              { rewrite HC'Tt. constructors. 
                applys* tr_typdefctx_wf_typ. constructors*. } } } }
        { introv Hn0. inverts~ Hn0. } }
      { (* variable length array..... *)
        applys uninitialized_array.
        { constructors.
          { rewrite HD. rew_set~. }
          { rewrite HC'Ta. repeat constructors~.
            { rewrite HD. rew_set~. }
            { rewrite HC'Tt. constructors~. 
              applys* tr_typdefctx_wf_typ. constructors*. } } }
        { introv HN. inverts HN. }
        { introv Hi. forwards* (a''&Ha'i'&Hla''): Ha'i.
          rewrite Ha'i'. constructors.
          { constructors.
            { rewrite HD. rew_set~. }
            { rewrite HC'Tt. constructors~.
              applys* tr_typdefctx_wf_typ. constructors*. } }
          { introv Hn. inverts~ Hn. }
          { introv Hi0. forwards* Htra': Htra i a'' i0.
            applys* H2. 
            { admit. (*index. NOTE: not true without assuming perfect tiling. *) }
            { constructors*. }
            { constructors*. } } } } }
    { (* other array *)
      introv Hneq Hla Htra. simpls. constructors.
      2:{ rewrite <- Hla. eapply H0. }
      { inverts H as.
        { introv HwfT. constructors*. 
          applys* tr_typdefctx_wf_typ. constructors*. }
        { introv HTvin HTCTv.
          inverts Hok as Htt HTain HCTa' HTt0nin Hfv.
          inverts Htt. unfolds wf_typdefctx. constructors*.
          { rewrite HD. rew_set~. }
          { rewrite~ HC'Tv. applys~ tr_typing_array Ta Tt0 n C.
            { rewrite HCTa in HCTa'. inverts HCTa'. constructors*. }
            { applys~ Hfv. introv HN. subst. applys~ Hneq. }
            { introv HN. subst. applys~ Hneq. } } } }
       { introv Hi. forwards* Htra': Htra i. admit. (*index*)
        applys* H2. admit. (*index*) constructors*. } } }
  { (* struct *)
    inverts Hv as HD Hvfsf. constructors.
    2:{ rewrite~ H0. }
    { applys* tr_typing_struct. }
    { introv Hfin. applys* H2. applys Hvfsf.
      rewrite~ <- H0. } }
Qed.

(* This will be proved when the relation is translated to a function. 
   See TrTilingFun.v. *)
Lemma total_tr_val_aux : forall gt v,
  exists v', tr_val gt v v'.
Proof.
Admitted.

(* Lemma for the new case. *)
Lemma tr_uninitialized_val : forall tt v T C C',
  tr_typdefctx tt C C' ->
  tiling_tr_ok tt C ->
  wf_typdefctx C ->
  uninitialized C T v ->
  exists v',
        tr_val tt v v'
    /\  uninitialized C' T v'.
Proof.
  introv HC Hok Hwf Hu. forwards* (v'&Hv'): total_tr_val_aux tt v.
  exists v'. splits~. applys* tr_uninitialized_val_aux.
Qed.

(* ---------------------------------------------------------------------- *)
(** Path surgery *)

Lemma tr_accesses_app : forall tt π1 π2 π1' π2',
  tr_accesses tt π1 π1' ->
  tr_accesses tt π2 π2' ->
  tr_accesses tt (π1 ++ π2) (π1' ++ π2').
Proof.
  introv Ha1 Ha2. gen π2 π2'. induction Ha1; intros;
  rew_list in *; eauto. 
Qed.


(* Main lemma *)

Theorem red_tr: forall tt C C' t t' v S S' m1 m1' m2,
  red C S m1 t m2 v ->
  tiling_tr_ok tt C ->
  tr_typdefctx tt C C' ->
  tr_trm tt t t' ->
  tr_stack tt S S' ->
  tr_state tt m1 m1' ->
  wf_typdefctx C ->
  wf_trm C t ->
  wf_stack C S ->
  wf_state C m1 ->
  ~ is_error v ->
  exists v' m2',
      tr_val tt v v'
  /\  tr_state tt m2 m2'
  /\  red C' S' m1' t' m2' v'.
Proof.
  introv HR Hok HC Ht HS Hm1 HwfC Hwft HwfS Hwfm1.
  introv He. gen tt C' t' S' m1'.
  induction HR; intros; try solve [ forwards*: He; unfolds* ].
  { (* val *) 
    inverts Ht as Hv. exists* v' m1'. }
  { (* var *) 
    inverts Ht. forwards* (v'&H'&Hv'): stack_lookup_tr HS H. exists* v' m1'. }
  { (* if *)
    inverts Ht as Hb HTrue HFalse. 
    inverts Hwft as Hwft0 Hwft1 Hwft2.
    forwards* (v'&m2'&Hv'&Hm2'&HR3): IHHR1 Hb HS Hm1.
    inverts* Hv'. destruct b;
    forwards* (vr'&m3'&Hvr'&Hm3'&HR4): IHHR2 HS Hm2';
    forwards*: wf_red HR1; exists* vr' m3'. }
  { (* let *)
    inverts Ht as Ht1 Ht2.
    inverts Hwft as Hwft0 Hwft1.
    forwards* (v'&m2'&Hv'&Hm2'&HR3): IHHR1 Ht1 HS Hm1.
    forwards HS': tr_stack_add z HS Hv'.
    forwards: not_tr_val_error Hv'.
    forwards* (vr'&m3'&Hvr'&Hm3'&HR4): IHHR2 Ht2 HS' Hm2'.
    { applys~ wf_stack_add. applys* wf_red HR1. }
    { applys* wf_red HR1. }
    exists* vr' m3'. }
  { (* binop *)
    inverts Ht as.
    { introv Hop. inverts Hop. }
    { introv Hop Ht1 Ht2. inverts Ht1 as Ht1. inverts Ht2 as Ht2.
      inverts H3;
      try solve [ exists __ m1' ; splits~ ; inverts Ht1 ;
      inverts Ht2 ; repeat constructors~ ].
      { exists __ m1'. splits~.
        forwards: functional_tr_val Ht1 Ht2. subst.
        constructors;
        repeat applys* is_basic_tr;
        repeat applys* not_tr_val_error.
        constructors~. }
      { exists __ m1'. splits~. constructors;
        repeat applys* is_basic_tr;
        repeat applys* not_tr_val_error.
        inverts Hwft as Hwfp Hwft1 Hwft2.
        inverts Hwft1 as Hwft1. inverts Hwft2 as Hwft2.
        forwards*: tr_val_inj_cp H H0. } } }
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
  { (* set *)
    inverts Ht as.
    { introv HN. inverts HN. }
    introv Hneq Htrt1' Htrt2'. subst.
    inverts Hm1 as HD Htrm.
    inverts H2 as Hin HW.
    forwards Htrml: Htrm Hin.
    inverts Htrt1' as Hp.
    inverts Hp as Hπ.
    inverts Htrt2' as Hv.
    forwards (w'&Hw'&HW'): tr_write_accesses Htrml Hv Hπ HW.
    exists val_unit m1'[l:=w']. splits~.
    { constructors.
      { unfold state. repeat rewrite~ dom_update.
        fold state. rewrite~ HD. }
      { introv Hi'. rew_reads~. intros. applys Htrm.
        applys~ indom_update_inv_neq Hi'. } }
    { constructors~. applys* not_tr_val_error.
      constructors*. rewrite~ <- HD. } }
  { (* new *)
    inverts Ht. subst.
    inverts Hm1 as HD Htrm.
    forwards* (v'&Hv'&Hu): tr_uninitialized_val.
    exists (val_abstract_ptr l nil) m1'[l:=v']. splits~.
    { constructors.
      { unfold state. repeat rewrite~ dom_update.
        fold state. rewrite~ HD. }
      { introv Hin. unfolds state. rew_reads; intros; eauto. } }
    { constructors*. rewrite~ <- HD. applys* tr_typdefctx_wf_typ. } }
  { (* new_array *)
    inverts Ht as.
    introv Ht.
    inverts Ht as Hv.
    inverts Hm1 as HD Htrm. subst.
    forwards* (v''&Hv''&Hu): tr_uninitialized_val.
    inverts Hv''.
    exists (val_abstract_ptr l nil) m1'[l:=(val_array (typ_array T None) a')]. splits~.
    { constructors.
      { unfold state. repeat rewrite~ dom_update.
        fold state. rewrite~ HD. }
      { introv Hin. unfolds state. rew_reads; intros; eauto. 
        constructors*. introv HN. inverts HN. } }
    { inverts Hv. applys~ red_new_array. rewrite~ <- HD. 
      applys* tr_typdefctx_wf_typ. auto. } }
  { (* struct access *)
    inverts Ht as Ht. subst.
    inverts Ht as Ht.
    inverts Ht as Hπ.
    exists (val_abstract_ptr l (π' & access_field T f)) m1'. 
    splits~.
    { constructors. applys~ tr_accesses_app. }
    { constructors~. } }
  { (* array access *) 
    subst. inverts Ht as.
    { introv Hop Ht1' Ht2' Haop.
      inverts Haop as.
      { (* tiling array *)
        introv Hor Htra Hpr. inverts Hor; inverts Hpr.
        inverts Ht1' as Htp.
        inverts Ht2' as Htv.
        inverts Htp as Hπ.
        inverts Htv.
        inverts Htra. simpls.
        remember (access_array (typ_var Ta) ((i/k)%Z)) as a1.
        remember (access_array (typ_var Tt) ((i mod k)%Z)) as a2.
        exists (val_abstract_ptr l (π'++(a1::a2::nil))) m1'. 
        subst. splits~.
        { constructors~. applys~ tr_accesses_app. constructors~. }
        { do 2 constructors~. unfolds Ctx.add. simpls.
          applys red_let m1' (val_int ((i/k)%Z)).
          { repeat constructors~. admit. (*TODO: Assume k <> 0*) }
          { introv HN. unfolds~ is_error. }
          { unfolds Ctx.add. simpls.
            applys red_let m1' (val_int ((i mod k)%Z)).
            { repeat constructors~. admit. (*TODO: Assume k <> 0*) }
            { introv HN. unfolds~ is_error. }
            { unfolds Ctx.add. simpls. applys~ red_args_1.
              { applys red_args_1. auto. constructors. simpls. eauto.
                applys red_args_2. auto. constructors. simpls. eauto.
                applys~ red_array_access. }
              { applys~ red_args_2.
                { constructors. simpls. eauto. }
                { constructors*. rew_list~. } } } } } }
      { (* other array *)
        introv Hor Hneq Hpr. inverts Hor; inverts Hpr.
        inverts Ht1' as Htp.
        inverts Ht2' as Htv.
        inverts Htp as Hπ.
        inverts Htv.
        exists (val_abstract_ptr l (π'++(access_array T i::nil))) m1'.
        splits~.
        { constructors~. applys~ tr_accesses_app. }
        { constructors~. } } }
    { (* absurd case *)
      introv HN. false. applys HN. unfolds~. } }
  { (* struct get *)
    inverts Ht as Ht. subst.
    inverts Ht as Hv.
    inverts Hv as HD Hsf.
    exists s'[f] m1'. splits~.
    constructors~. rewrite~ <- HD. }
  { (* array get *)
    subst. inverts Ht as.
    { (* array get *)
      introv Hop Ht1' Ht2' Haop.
      inverts Haop as.
      { (* tiling array *)
        introv Hor Htracc Hpr. inverts Hor; inverts Hpr.
        inverts Ht1' as Hva.
        inverts Ht2' as Hvi.
        inverts Hva as; try solve [ intros ; false ].
        introv Htt Hla Hla' Ha'' Ha'.
        inverts Hvi.
        forwards* (a''&Ha'i&Hla''): Ha'' ((i/k)%Z). admit. (*index*)
        forwards* Htra: Ha' ((i/k)%Z) a'' ((i mod k)%Z). admit. admit. (*index*)
        rewrite <- div_plus_mod_eq in Htra.
        exists a''[(i mod k)%Z] m1'. splits~.
        inverts Htracc. inverts Htt.
        do 2 constructors~. unfolds Ctx.add. simpls.
          applys red_let m1' (val_int ((i/k)%Z)).
          { repeat constructors~. admit. (*TODO: Assume k <> 0*) }
          { introv HN. unfolds~ is_error. }
          { unfolds Ctx.add. simpls.
            applys red_let m1' (val_int ((i mod k)%Z)).
            { repeat constructors~. admit. (*TODO: Assume k <> 0*) }
            { introv HN. unfolds~ is_error. }
            { unfolds Ctx.add. simpls. applys~ red_args_1.
              { applys red_args_1. auto. constructors. simpls. eauto.
                applys red_args_2. auto. constructors. simpls. eauto.
                applys~ red_array_get. admit. (*index*) }
              { applys~ red_args_2.
                { constructors. simpls. eauto. }
                { constructors*. admit. (*index*) } } } } }
      { (* another array *) 
        introv Hor Hneq Hpr. inverts Hor; inverts Hpr.
        inverts Ht1' as Hva.
        inverts Ht2' as Hvi.
        inverts Hva as _ Hla Htrai; simpls; tryfalse.
        inverts Hvi.
        exists a'[i] m1'. splits~.
        constructors~. admit. (*index*) } }
    { (* absurd case *)
      introv HN. false. applys HN. unfolds~. } }
  { (* args 1 *)
    inverts Ht as.
    { introv Hop Ht1' Ht2' Htaop. inverts Hwft.
      forwards* (v'&m2'&Hv'&Hm2'&HR'): IHHR1.
      forwards*: not_is_error_args_1 HR2 He.
      (* inverts Htaop; inverts_head Logic.or.
      forwards* (v''&m3'&Hv''&Hm3'&HR''): IHHR2.
      repeat constructors*.
      applys* wf_red HR1.
      applys* wf_red HR1.
      applys* tr_trm_array.
      {  
        { constructors*. inverts_head tr_access. }
        { admit. }
        { constructors*. }
        { constructors*. } }
      exists v'' m3'. splits*. *) admit. }
    { admit. }
    { admit. } }
  { (* args 2 *)
    admit. }
Admitted.

End TransformationProofs.






























