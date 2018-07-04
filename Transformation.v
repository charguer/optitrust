(**

Basic transformations.

*)


Set Implicit Arguments.

Require Export Semantics.


Hint Constructors red.

Open Scope list_scope.


Global Instance Inhab_trm : Inhab trm.
Proof using. apply (Inhab_of_val (trm_val val_unit)). Qed.

(* Initial typdefctx *)
Definition pos : typdef_struct := (\{})["x" := typ_int]["y" := typ_int]["z" := typ_int].
Definition C : typdefctx := (\{})["pos" := pos].

(* Final typdefctx *)
Definition struct_x : typdef_struct := (\{})["x" := typ_int].
Definition pos' : typdef_struct := (\{})["s" := (typ_struct "struct_x")]["y" := typ_int]["z" := typ_int].
Definition C' : typdefctx := (\{})["pos" := pos'].

(* Grouping transformation *)
Record group_tr := make_group_tr {
  group_tr_struct_name : typvar; (* Ts *)
  group_tr_fields : set field; (* {..f..} *)
  group_tr_new_struct_name : typvar; (* Tsg *)
  group_tr_new_struct_field : field (* fg *)
}.

Notation make_group_tr' := make_group_tr.

(* π ~ |π| *)
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

(* v ~ |v| *)
Inductive tr_val (gt:group_tr) : val -> val -> Prop :=
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
        f \in fs ->
        tr_val gt s[f] sg[f]) ->
      (forall f,       
        f \notin fs ->
        f \indom s ->
        tr_val gt s[f] s'[f]) ->
      binds s' fg (val_struct Tsg sg) ->
      tr_val gt (val_struct Ts s) (val_struct Ts s')
  | tr_val_struct_other : forall T s s',
      T <> group_tr_struct_name gt ->
      dom s = dom s' ->
      (forall f,
        index s f ->
        tr_val gt s[f] s'[f]) ->
      tr_val gt (val_struct T s) (val_struct T s').

(* S ~ |S| *)
Inductive tr_stack (gt:group_tr) : stack -> stack -> Prop :=
  | tr_stack_intro : forall S S',
      length S = length S' ->
      (forall x v,
        Ctx.lookup x S = Some v ->
        (exists v', Ctx.lookup x S' = Some v' /\ tr_val gt v v')) ->
      tr_stack gt S S'.

(* m ~ |m| *)
Inductive tr_state (gt:group_tr) : state -> state -> Prop :=
  | tr_state_intro : forall m m',
      dom m = dom m' ->
      (forall l,
        index m l ->
        tr_val gt m[l] m'[l]) ->
      tr_state gt m m'.

(* t ~ |t| *)
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
  | tr_trm_binop : forall t1 t2 t1' t2',
      tr_trm gt t1 t1' ->
      tr_trm gt t2 t2' ->
      tr_trm gt (trm_app binop_add (t1::t2::nil)) (trm_app binop_add (t1'::t2'::nil))
  (* Abstract heap operations *)
  | tr_trm_get : forall T p p',
      tr_trm gt p p' ->
      tr_trm gt (trm_app (prim_get T) (p::nil)) (trm_app (prim_get T) (p'::nil))
  | tr_trm_set : forall T p p' t t',
      tr_trm gt p p' ->
      tr_trm gt t t' ->
      tr_trm gt (trm_app (prim_set T) (p::t::nil)) (trm_app (prim_set T) (p'::t'::nil))
  | tr_trm_new : forall T t t',
      tr_trm gt t t' ->
      tr_trm gt (trm_app (prim_new T) (t::nil)) (trm_app (prim_new T) (t'::nil))
  | tr_trm_array_access : forall A t i t' i' r,
      tr_trm gt t t' ->
      tr_trm gt i i' ->
      r = trm_app (prim_array_access A) (t'::i'::nil) ->
      tr_trm gt (trm_app (prim_array_access A) (t::i::nil)) r
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
      s = group_tr_struct_name gt ->
      (T <> s \/ f \notin (group_tr_fields gt)) -> 
      tr_trm gt p p' -> 
      r = (trm_app (prim_struct_access T f) (p'::nil)) ->
      tr_trm gt (trm_app (prim_struct_access T f) (p::nil)) r.

Lemma index_of_index_length' : forall A (l' l : list A) i,
  index l' i ->
  length l' = length l ->
  index l i.
Proof.
  intros. rewrite index_eq_index_length in *.
  applys* index_of_index_length'.
Qed.

Hint Resolve index_of_index_length'.

(* Transformations are functions. *)
Theorem functional_tr_accesses : forall gt π π1 π2,
  tr_accesses gt π π1 ->
  tr_accesses gt π π2 ->
    π1 = π2.
Proof.
  introv H1 H2. gen π2. induction H1; intros;
  try solve [ inverts* H2 ; repeat fequals* ].
  { inverts H4; repeat fequals*;
    inverts H11; tryfalse. }
  { inverts H2; repeat fequals*;
    inverts H0; tryfalse. }
Qed.

Lemma tr_accesses_app : forall gt π1 π2 π1' π2',
  tr_accesses gt π1 π1' ->
  tr_accesses gt π2 π2' ->
  tr_accesses gt (π1 ++ π2) (π1' ++ π2').
Proof.
  introv Ha1 Ha2. gen π2 π2'. induction Ha1; intros;
  try solve [ subst ; forwards Ha': IHHa1 Ha2 ;
  repeat rewrite* <- List.app_comm_cons ; constructors* ].
  1: repeat rewrite* List.app_nil_l. 
Qed.

Theorem functional_tr_val : forall gt v v1 v2,
  tr_val gt v v1 ->
  tr_val gt v v2 ->
  v1 = v2.
Proof.
  introv H1 H2. gen v2. induction H1; intros; 
  try solve [ inverts H2 ; fequals* ].
  { inverts H2. fequals. applys* functional_tr_accesses. }
  { inverts H2. fequals. applys* eq_of_extens. math. }
  { admit. } (* extens lemma for maps *)
Admitted.

Theorem functional_tr_trm : forall gt t t1 t2,
  tr_trm gt t t1 ->
  tr_trm gt t t2 ->
  t1 = t2.
Proof.
  introv H1 H2. gen t2. induction H1; intros;
  try solve [ inverts H2 ; try subst ; repeat fequals* ].
  { inverts H2. fequals. applys* functional_tr_val. }
  { inverts* H7.
    { forwards*: IHtr_trm H11. subst*. } 
    { inverts H12; tryfalse. } }
  { inverts* H3. 
    { inverts H0; tryfalse. }
    { forwards*: IHtr_trm H10. subst*. } }
Qed.

Theorem functional_tr_stack : forall gt S S1 S2,
  tr_stack gt S S1 ->
  tr_stack gt S S2 ->
  S1 = S2.
Proof.
  admit. (* extens lemma for ctxts. *)
Admitted.

Theorem functional_tr_state : forall gt m m1 m2,
  tr_state gt m m1 ->
  tr_state gt m m2 ->
  m1 = m2.
Proof.
  admit. (* extens lemma for maps. *)
Admitted.

Lemma tr_stack_add : forall gt z v S v' S',
  tr_stack gt S S' ->
  tr_val gt v v' ->
  tr_stack gt (Ctx.add z v S) (Ctx.add z v' S').
Proof.
Admitted.

Hint Constructors tr_val tr_accesses read_accesses write_accesses.

Axiom in_union : forall A (x:A) (S1 S2:set A), 
        (x \in S1) \/ (x \in S2) -> 
        x \in (S1 \u S2).

Axiom in_setminus : forall A (x:A) (S1 S2: set A),
        x \in S1 -> 
        x \notin S2 -> 
        x \in (S1 \- S2).

Axiom not_tr_val_error : forall gt v1 v2, 
  tr_val gt v1 v2 -> 
  ~ is_error v2.

Axiom index_of_update_neq : forall A B (v:B) (l l':A) (m:map A B),
  index m[l:=v] l' ->
  l <> l' ->
  index m l'.

Axiom index_of_update_neq' : forall A (v:A) i i' (l:list A),
  index l[i:=v] i' ->
  i <> i' ->
  index l i'.

Axiom in_notin_neq : forall A (x y:A) (S:set A),
  x \in S ->
  y \notin S ->
  y <> x.

Axiom notin_notin_subset : forall A (x:A) (S1 S2:set A),
  S1 \c S2 ->
  x \notin S2 ->
  x \notin S1.

Axiom in_subset : forall A (x:A) (S1 S2:set A),
  S2 \c S1 ->
  x \in S2 ->
  x \in S1.

Axiom in_single : forall A (x:A) (S:set A),
  S = '{x} ->
  x \in S.

Axiom union_eq : forall A (S1 S2 S3 S4:set A),
  S1 = S3 ->
  S2 = S4 ->
  S1 \u S2 = S3 \u S4.

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
    inverts Ha. exists* v'. } 
  { (* array_access *)
    inverts Ha as Ha. inverts Hv as Hl Htr.
    forwards Htra: Htr H. 
    forwards (w'&Hw'&Hπ'): IHHR Htra Ha.
    exists* w'. }
  { (* struct_access *) 
    inverts Ha as; inverts Hv as; 
    try solve [ intros ; false ].
    { (* one of the fields to group *) 
      introv HD1 Hgt HD2 HD3 Hsg Hfs HB Ha Hin.
      rewrite* Hgt in Hin. simpls.
      forwards Hsf: Hsg Hin.
      forwards Heq: read_of_binds H. 
      subst_hyp Heq.
      forwards (w'&Hw'&HR'): IHHR Hsf Ha.
      exists* w'. splits*.
      constructors*; rewrite Hgt; simpls*. 
      constructors*. applys* binds_of_indom_read. }
    { (* struct transformed but another field *) 
      introv HD1 HD2 HD3 Hsg Hfs HB Ha Hor.
      inverts Hor as Hf; simpl in Hf; tryfalse.
      forwards Hf': indom_of_binds H. typeclass.
      forwards Hsf: Hfs Hf Hf'.
      forwards Hv1: read_of_binds H. 
      subst_hyp Hv1.
      forwards (w'&Hw'&HR'): IHHR Hsf Ha.
      exists w'. splits*. constructors*. 
      applys* binds_of_indom_read.
      rewrite HD3. applys* in_union. 
      left. applys* in_setminus. }
    { (* another struct *)
      intros Hn HD Hfs Ha Hor. 
      forwards Hidx: index_of_binds H. typeclass.
      forwards Hsf: Hfs Hidx. 
      forwards Heq: read_of_binds H.
      subst_hyp Heq. 
      forwards (w'&Hw'&HR'): IHHR Hsf Ha.
      exists w'. splits*. constructors*. 
      applys* binds_of_indom_read.
      rewrite <- HD. 
      rewrite* <- index_eq_indom. } }
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
  introv Hv1 Hw Ha HW. gen gt v1' w' π'. induction HW; intros.
  { (* nil *)
    inverts Ha. subst_hyp H. exists* w'. }
  { (* array_access *)
    inverts Ha as Ha. inverts Hv1 as Hl Htr.
    forwards Htra: Htr H.
    forwards (v2'&Hv2'&HW'): IHHW Htra Hw Ha.
    exists (val_array a'[i:=v2']).
    splits; constructors*.
    { (* val_array under tr *)
      rewrite H0. repeat rewrite* length_update. }
    { (* write_accesses of transformed array *) 
      introv Hi0. rewrite read_update_case. 
      { case_if*; subst_hyp H0.
        { subst_hyp C0. rewrites* LibListZ.read_update_same. }
        { forwards: index_of_update_neq' Hi0 C0. 
          rewrites* LibListZ.read_update_neq. } }
      { rewrite index_eq_index_length in *.
        rewrite H0 in Hi0. rewrite length_update in Hi0.
        rewrite* <- Hl. } } }
  { (* struct_access *)
    inverts Ha as; inverts Hv1 as;
    try solve [ intros ; false ].
    { (* one of the fields to group *) 
      introv HD1 Hgt HD2 HD3 Hsg Hfs HB Ha Hin.
      rewrite* Hgt in Hin. simpls.
      forwards Hsf: Hsg Hin.
      forwards Heq: read_of_binds H. 
      subst_hyp Heq.
      forwards (v2'&Hv2'&HW'): IHHW Hsf Hw Ha.
      remember (group_tr_struct_name gt) as T.
      exists (val_struct T s'[fg:=(val_struct Tsg sg[f:=v2'])]).
      splits.

      { admit. (* <--- *) }

      { constructors*; subst_hyp Hgt; simpls*.
        constructors*. applys* binds_of_indom_read. } }
    { (* struct transformed but another field *) 
      introv HD1 HD2 HD3 Hsg Hfs HB Ha Hor. 
      inverts Hor as Hf; simpl in Hf; tryfalse.
      forwards Hf': indom_of_binds H. typeclass.
      forwards Hsf: Hfs Hf Hf'.
      forwards Hv1: read_of_binds H. 
      subst_hyp Hv1.
      forwards (v2'&Hv2'&HW'): IHHW Hsf Hw Ha.
      exists (val_struct T s'[f:=v2']). splits.
      { applys* tr_val_struct_group; subst_hyp H0;
        try solve [ rewrite* dom_update_at_indom ].
        { repeat rewrite* dom_update_at_indom.
          rewrite HD3. applys* in_union. left.
          applys* in_setminus. }        
        { introv Hf0. rewrite read_update. case_if*.
          forwards: in_notin_neq Hf0 Hf. false. }
        { introv HD4 HD5. repeat rewrite read_update.
          case_if*. rewrite* dom_update_at_indom in HD5. }
        { applys* binds_update_neq.
          apply in_notin_neq with (S:=dom s1); auto. } }
      { constructors*.  applys* binds_of_indom_read.
        rewrite HD3. applys in_union. left.
        applys* in_setminus. } }
    { (* another struct *)
      intros Hn HD Hfs Ha Hor.
      forwards Hidx: index_of_binds H. typeclass.
      forwards Hsf: Hfs Hidx. 
      forwards Heq: read_of_binds H.
      subst_hyp Heq. 
      forwards (v2'&Hv2'&HW'): IHHW Hsf Hw Ha.
      exists (val_struct T s'[f:=v2']). splits.
      { constructors*; subst_hyp H0.
        { rewrite index_eq_indom in Hidx.
          rewrites* dom_update_at_indom.
          rewrite HD in Hidx.
          rewrites* dom_update_at_indom. }
        { introv Hif0. rewrite read_update. case_if*.
          { subst_hyp C0. rewrite* read_update_same. }
          { rewrite* read_update_neq. 
            forwards Hif0': index_of_update_neq Hif0 C0.
            forwards*: Hfs Hif0'. } } }
      { constructors*. applys* binds_of_indom_read.
        rewrite index_eq_indom in Hidx.
        rewrite* <- HD. } } }
Qed.

(* Semantics preserved by tr. *)
Theorem red_tr: forall gt t t' v S S' m1 m1' m2,
  tr_trm gt t t' ->
  tr_stack gt S S' ->
  tr_state gt m1 m1' ->
  red S m1 t m2 v -> exists v' m2',
      tr_val gt v v'
  /\  tr_state gt m2 m2'
  /\  red S' m1' t' m2' v'.
Proof.
  introv Ht HS Hm1 HR. gen t' S' m1'. induction HR; intros.
  { (* var *)
    inverts Ht. inverts HS. 
    forwards: H1 H. inverts H2. exists* x0 m1'. }
  { (* val *)
    inverts Ht. exists* v' m1'. }
  { (* if *)
    inverts Ht as Hb HTrue HFalse. 
    forwards (v'&m2'&Hv'&Hm2'&HR3): IHHR1 Hb HS Hm1. inverts* Hv'.
    forwards (vr'&m3'&Hvr'&Hm3'&HR4): IHHR2 HS Hm2'. 2: exists* vr' m3'. case_if*. } 
  { (* let *)
    inverts Ht as Ht1 Ht2.
    forwards (v'&m2'&Hv'&Hm2'&HR3): IHHR1 Ht1 HS Hm1.
    forwards HS': tr_stack_add z HS Hv'.
    forwards (vr'&m3'&Hvr'&Hm3'&HR4): IHHR2 Ht2 HS' Hm2'.
    forwards: not_tr_val_error Hv'.
    exists* vr' m3'. }
  { (* binop *)
    inverts Ht as Ht1 Ht2.
    inverts H. exists (n1 + n2)%Z m1'. 
    inverts Ht1 as Ht1. inverts Ht2 as Ht2.
    splits*. constructors*. 
    inverts Ht1. inverts Ht2. constructors*. }
  { (* get *)
    inverts Ht as Hp. inverts Hm1 as HD Htrm.
    inverts H0 as Hb Ha. forwards Hi: index_of_binds Hb.
    typeclass. forwards Htrml: Htrm Hi.
    subst_hyp H. inverts Hp as Hp. inverts Hp.
    forwards: read_of_binds Hb. subst_hyp H.
    forwards (w'&Hw'&Ha'): tr_read_accesses Htrml H2 Ha.
    exists w' m1'. splits*. 
    constructors*. constructors*.
    applys* read_state_intro. 
    applys* binds_of_indom_read. 
    rewrite <- HD at 1.
    forwards*: indom_of_binds Hb. }
  { (* set *)
    inverts Ht as Hp Ht. inverts Hm1 as HD Htrm.
    inverts H2 as Hb HW. forwards Hi: index_of_binds Hb.
    typeclass. forwards Htrml: Htrm Hi. subst_hyp H.
    inverts Hp as Hp. inverts Hp as Ha.
    subst_hyp H0. inverts Ht as Hv. 
    forwards Heq: read_of_binds Hb. subst_hyp Heq.
    forwards (w'&Hw'&HW'): tr_write_accesses Htrml Hv Ha HW.
    exists val_unit m1'[l:=w']. splits*.
    { constructors. 
      { rewrite index_eq_indom in Hi.
        forwards HDm1: dom_update_at_indom Hi. typeclass.
        rewrite HD in Hi at 1.
        forwards HDm1': dom_update_at_indom Hi. typeclass.
        rewrite* HDm1. rewrite* HDm1'. }
      { introv Hi'. do 2 rewrites read_update.
        case_if*. 
        forwards Hi'': index_of_update_neq Hi' C0. 
        forwards*: Htrm Hi''. } }
    { constructors*. applys* not_tr_val_error.
      constructors*. applys* binds_of_indom_read.
      rewrite <- HD at 1. forwards*: indom_of_binds Hb. } }
  { (* new *) 
    inverts Ht as Ht. inverts Ht as Hv. 
    inverts Hm1 as HD Htrm.
    subst_hyp H1.
    exists __ m1'[l0:=v']. (*(val_abstract_ptr l0 nil)*)
    splits*.
    { constructors. 
      { unfold state. repeat rewrite dom_update. 
        applys* union_eq.  }
      { introv Hi. rewrite read_update. case_if*.
        { subst_hyp C0. rewrite* read_update_same. }
        { rewrite* read_update_neq. 
          forwards Hi': index_of_update_neq Hi C0.
          forwards*: Htrm Hi'. } } }
    { constructors*. rewrite* <- HD. } }
  { (* struct_access *) 
    inverts Ht as; inverts Hm1 as HD Htrm.
    { (* accessing grouped field *)
      introv Ht Hf. subst. 
      inverts Ht as Hv. inverts Hv as Ha.
      remember (group_tr_struct_name gt) as Ts.
      remember (group_tr_new_struct_name gt) as Tsg.
      remember (group_tr_new_struct_field gt) as fg.
      remember (access_field Ts fg) as a1.
      remember (access_field Tsg f) as a2.
      exists (val_abstract_ptr l (π'++(a1::a2::nil))) m1'.
      splits.
      { constructors. applys* tr_accesses_app. subst.
        constructors*. }
      { constructors*. }
      { subst. applys* red_args_1. applys* red_struct_access.
        fequals*. rewrite* <- List.app_assoc. } }
    { (* accessing another field *) 
      introv Hor Ht. subst. inverts Ht as Hv. inverts Hv as Ha.
      exists (val_abstract_ptr l (π'++(access_field T f :: nil))) m1'.
      splits; constructors*. applys* tr_accesses_app. } }
  { (* array_access *)
     }
Admitted.


(* Semantics preserved by tr. *)
Theorem red_tr': forall gt t t' v v' S S' m1 m1' m2 m2',
  tr_trm gt t t' ->
  tr_val gt v v' ->
  tr_stack gt S S' ->
  tr_state gt m1 m1' ->
  tr_state gt m2 m2' ->
  red S m1 t m2 v -> 
  red S' m1' t' m2' v'.
Proof.
  introv Ht Hv HS Hm1 Hm2 H. gen t' v' S' m1' m2'.
  induction H; intros.  
  { (* var *)
    inverts Ht. inverts HS as HS1 HS2.
    forwards (H'&Hv'): HS2 H.
    forwards Hveq: functional_tr_val Hv Hv'; rewrite <- Hveq in *.
    forwards Hmeq: functional_tr_state Hm1 Hm2. rewrite Hmeq.
    constructors*.  }
  { (* val *)
    inverts Ht as Hv'. 
    forwards Hveq: functional_tr_val Hv Hv'. rewrite Hveq.
    forwards Hmeq: functional_tr_state Hm1 Hm2. rewrite Hmeq.
    constructors*. }
  { (* if *)
    admit. }
  { (* let *)
    admit. }
Admitted.
