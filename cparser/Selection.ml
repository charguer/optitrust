open AST
open BinNums
open Builtins
open Builtins0
open Cminor
open CminorSel
open Cminortyping
open Coqlib
open Datatypes
open Errors
open Integers
open List0
open Machregs
open Maps
open Op
open SelectDiv
open SelectLong
open SelectOp
open SplitLong
open String0
open Switch

(** val condexpr_of_expr : expr -> condexpr **)

let rec condexpr_of_expr e = match e with
| Eop (o, el) ->
  (match o with
   | Ocmp c -> CEcond (c, el)
   | _ -> CEcond ((Ccompuimm (Cne, Int.zero)), (Econs (e, Enil))))
| Econdition (a, b, c) ->
  CEcondition (a, (condexpr_of_expr b), (condexpr_of_expr c))
| Elet (a, b) -> CElet (a, (condexpr_of_expr b))
| _ -> CEcond ((Ccompuimm (Cne, Int.zero)), (Econs (e, Enil)))

(** val condition_of_expr : expr -> condition * exprlist **)

let condition_of_expr e = match e with
| Eop (o, el) ->
  (match o with
   | Ocmp c -> (c, el)
   | _ -> ((Ccompuimm (Cne, Int.zero)), (Econs (e, Enil))))
| _ -> ((Ccompuimm (Cne, Int.zero)), (Econs (e, Enil)))

(** val load : memory_chunk -> expr -> expr **)

let load chunk e1 =
  let (mode, args) = addressing chunk e1 in Eload (chunk, mode, args)

(** val store : memory_chunk -> expr -> expr -> stmt **)

let store chunk e1 e2 =
  let (mode, args) = addressing chunk e1 in Sstore (chunk, mode, args, e2)

type globdef = (Cminor.fundef, unit) AST.globdef

(** val sel_constant : constant -> expr **)

let sel_constant = function
| Cminor.Ointconst n -> Eop ((Ointconst n), Enil)
| Cminor.Ofloatconst f -> Eop ((Ofloatconst f), Enil)
| Cminor.Osingleconst f -> Eop ((Osingleconst f), Enil)
| Cminor.Olongconst n -> SelectLong.longconst n
| Oaddrsymbol (id, ofs) -> addrsymbol id ofs
| Oaddrstack ofs -> addrstack ofs

(** val sel_unop : helper_functions -> unary_operation -> expr -> expr **)

let sel_unop hf op arg =
  match op with
  | Cminor.Ocast8unsigned -> cast8unsigned arg
  | Cminor.Ocast8signed -> cast8signed arg
  | Cminor.Ocast16unsigned -> cast16unsigned arg
  | Cminor.Ocast16signed -> cast16signed arg
  | Onegint -> negint arg
  | Onotint -> notint arg
  | Cminor.Onegf -> negf arg
  | Cminor.Oabsf -> absf arg
  | Cminor.Onegfs -> negfs arg
  | Cminor.Oabsfs -> absfs arg
  | Cminor.Osingleoffloat -> singleoffloat arg
  | Cminor.Ofloatofsingle -> floatofsingle arg
  | Cminor.Ointoffloat -> intoffloat arg
  | Ointuoffloat -> intuoffloat arg
  | Cminor.Ofloatofint -> floatofint arg
  | Ofloatofintu -> floatofintu arg
  | Cminor.Ointofsingle -> intofsingle arg
  | Ointuofsingle -> intuofsingle arg
  | Cminor.Osingleofint -> singleofint arg
  | Osingleofintu -> singleofintu arg
  | Cminor.Onegl -> SelectLong.negl arg
  | Cminor.Onotl -> SelectLong.notl arg
  | Ointoflong -> SelectLong.intoflong arg
  | Olongofint -> SelectLong.longofint arg
  | Olongofintu -> SelectLong.longofintu arg
  | Cminor.Olongoffloat -> SelectLong.longoffloat hf arg
  | Olonguoffloat -> longuoffloat hf arg
  | Cminor.Ofloatoflong -> SelectLong.floatoflong hf arg
  | Ofloatoflongu -> floatoflongu hf arg
  | Cminor.Olongofsingle -> SelectLong.longofsingle hf arg
  | Olonguofsingle -> longuofsingle hf arg
  | Cminor.Osingleoflong -> SelectLong.singleoflong hf arg
  | Osingleoflongu -> singleoflongu hf arg

(** val sel_binop :
    helper_functions -> binary_operation -> expr -> expr -> expr **)

let sel_binop hf op arg1 arg2 =
  match op with
  | Oadd -> add arg1 arg2
  | Cminor.Osub -> sub arg1 arg2
  | Cminor.Omul -> mul arg1 arg2
  | Cminor.Odiv -> divs arg1 arg2
  | Cminor.Odivu -> divu arg1 arg2
  | Cminor.Omod -> mods arg1 arg2
  | Cminor.Omodu -> modu arg1 arg2
  | Cminor.Oand -> coq_and arg1 arg2
  | Cminor.Oor -> coq_or arg1 arg2
  | Cminor.Oxor -> xor arg1 arg2
  | Cminor.Oshl -> shl arg1 arg2
  | Cminor.Oshr -> shr arg1 arg2
  | Cminor.Oshru -> shru arg1 arg2
  | Cminor.Oaddf -> addf arg1 arg2
  | Cminor.Osubf -> subf arg1 arg2
  | Cminor.Omulf -> mulf arg1 arg2
  | Cminor.Odivf -> divf arg1 arg2
  | Cminor.Oaddfs -> addfs arg1 arg2
  | Cminor.Osubfs -> subfs arg1 arg2
  | Cminor.Omulfs -> mulfs arg1 arg2
  | Cminor.Odivfs -> divfs arg1 arg2
  | Oaddl -> SelectLong.addl arg1 arg2
  | Cminor.Osubl -> SelectLong.subl arg1 arg2
  | Cminor.Omull -> SelectLong.mull hf arg1 arg2
  | Cminor.Odivl -> divls hf arg1 arg2
  | Cminor.Odivlu -> divlu hf arg1 arg2
  | Cminor.Omodl -> modls hf arg1 arg2
  | Cminor.Omodlu -> modlu hf arg1 arg2
  | Cminor.Oandl -> SelectLong.andl arg1 arg2
  | Cminor.Oorl -> SelectLong.orl arg1 arg2
  | Cminor.Oxorl -> SelectLong.xorl arg1 arg2
  | Cminor.Oshll -> SelectLong.shll hf arg1 arg2
  | Cminor.Oshrl -> SelectLong.shrl hf arg1 arg2
  | Cminor.Oshrlu -> SelectLong.shrlu hf arg1 arg2
  | Cminor.Ocmp c -> comp c arg1 arg2
  | Ocmpu c -> compu c arg1 arg2
  | Ocmpf c -> compf c arg1 arg2
  | Ocmpfs c -> compfs c arg1 arg2
  | Ocmpl c -> SelectLong.cmpl c arg1 arg2
  | Ocmplu c -> SelectLong.cmplu c arg1 arg2

(** val sel_select : typ -> expr -> expr -> expr -> expr **)

let sel_select ty cnd ifso ifnot =
  let (cond, args) = condition_of_expr cnd in
  (match select ty cond args ifso ifnot with
   | Some a -> a
   | None -> Econdition ((condexpr_of_expr cnd), ifso, ifnot))

(** val sel_expr : helper_functions -> Cminor.expr -> expr **)

let rec sel_expr hf = function
| Cminor.Evar id -> Evar id
| Econst cst -> sel_constant cst
| Eunop (op, arg) -> sel_unop hf op (sel_expr hf arg)
| Ebinop (op, arg1, arg2) ->
  sel_binop hf op (sel_expr hf arg1) (sel_expr hf arg2)
| Cminor.Eload (chunk, addr) -> load chunk (sel_expr hf addr)

(** val sel_exprlist : helper_functions -> Cminor.expr list -> exprlist **)

let rec sel_exprlist hf = function
| [] -> Enil
| a :: bl -> Econs ((sel_expr hf a), (sel_exprlist hf bl))

(** val sel_select_opt :
    helper_functions -> typ -> Cminor.expr -> Cminor.expr -> Cminor.expr ->
    expr option **)

let sel_select_opt hf ty arg1 arg2 arg3 =
  let (cond, args) = condition_of_expr (sel_expr hf arg1) in
  select ty cond args (sel_expr hf arg2) (sel_expr hf arg3)

type call_kind =
| Call_default
| Call_imm of ident
| Call_builtin of external_function

(** val expr_is_addrof_ident : Cminor.expr -> ident option **)

let expr_is_addrof_ident = function
| Econst c ->
  (match c with
   | Oaddrsymbol (id, ofs) ->
     if Ptrofs.eq ofs Ptrofs.zero then Some id else None
   | _ -> None)
| _ -> None

(** val classify_call : globdef PTree.t -> Cminor.expr -> call_kind **)

let classify_call defmap e =
  match expr_is_addrof_ident e with
  | Some id ->
    (match PTree.get id defmap with
     | Some g ->
       (match g with
        | Gfun f ->
          (match f with
           | Internal _ -> Call_imm id
           | External ef ->
             if ef_inline ef then Call_builtin ef else Call_imm id)
        | Gvar _ -> Call_imm id)
     | None -> Call_imm id)
  | None -> Call_default

(** val sel_builtin_arg :
    helper_functions -> Cminor.expr -> builtin_arg_constraint -> expr
    builtin_arg **)

let sel_builtin_arg hf e c =
  let e' = sel_expr hf e in
  let ba = builtin_arg e' in if builtin_arg_ok ba c then ba else BA e'

(** val sel_builtin_args :
    helper_functions -> Cminor.expr list -> builtin_arg_constraint list ->
    expr builtin_arg list **)

let rec sel_builtin_args hf el cl =
  match el with
  | [] -> []
  | e :: el0 ->
    (sel_builtin_arg hf e (hd OK_default cl)) :: (sel_builtin_args hf el0
                                                   (tl cl))

(** val sel_builtin_res : ident option -> ident builtin_res **)

let sel_builtin_res = function
| Some id -> BR id
| None -> BR_none

(** val sel_known_builtin : builtin_function -> exprlist -> expr option **)

let sel_known_builtin bf args =
  match bf with
  | BI_standard b ->
    (match b with
     | BI_select ty ->
       (match args with
        | Enil -> None
        | Econs (a1, e) ->
          (match e with
           | Enil -> None
           | Econs (a2, e0) ->
             (match e0 with
              | Enil -> None
              | Econs (a3, e1) ->
                (match e1 with
                 | Enil -> Some (sel_select ty a1 a2 a3)
                 | Econs (_, _) -> None))))
     | BI_fabs ->
       (match args with
        | Enil -> None
        | Econs (a1, e) ->
          (match e with
           | Enil -> Some (absf a1)
           | Econs (_, _) -> None))
     | BI_fabsf ->
       (match args with
        | Enil -> None
        | Econs (a1, e) ->
          (match e with
           | Enil -> Some (absfs a1)
           | Econs (_, _) -> None))
     | _ -> None)
  | BI_platform b -> platform_builtin b args

(** val coq_Sno_op : stmt **)

let coq_Sno_op =
  Sseq (Sskip, Sskip)

(** val sel_builtin_default :
    helper_functions -> ident option -> external_function -> Cminor.expr list
    -> stmt **)

let sel_builtin_default hf optid ef args =
  Sbuiltin ((sel_builtin_res optid), ef,
    (sel_builtin_args hf args (builtin_constraints ef)))

(** val sel_builtin :
    helper_functions -> ident option -> external_function -> Cminor.expr list
    -> stmt **)

let sel_builtin hf optid ef args =
  match ef with
  | EF_builtin (name, sg) ->
    (match lookup_builtin_function name sg with
     | Some bf ->
       (match optid with
        | Some id ->
          (match sel_known_builtin bf (sel_exprlist hf args) with
           | Some a -> Sassign (id, a)
           | None -> sel_builtin_default hf optid ef args)
        | None -> coq_Sno_op)
     | None -> sel_builtin_default hf optid ef args)
  | _ -> sel_builtin_default hf optid ef args

(** val compile_switch : coq_Z -> nat -> table -> comptree **)

let compile_switch = Switchaux.compile_switch

(** val sel_switch :
    (expr -> coq_Z -> expr) -> (expr -> coq_Z -> expr) -> (expr -> coq_Z ->
    expr) -> (expr -> expr) -> nat -> comptree -> exitexpr **)

let rec sel_switch make_cmp_eq make_cmp_ltu make_sub make_to_int arg = function
| CTaction act -> XEexit act
| CTifeq (key, act, t') ->
  XEcondition ((condexpr_of_expr (make_cmp_eq (Eletvar arg) key)), (XEexit
    act), (sel_switch make_cmp_eq make_cmp_ltu make_sub make_to_int arg t'))
| CTiflt (key, t1, t2) ->
  XEcondition ((condexpr_of_expr (make_cmp_ltu (Eletvar arg) key)),
    (sel_switch make_cmp_eq make_cmp_ltu make_sub make_to_int arg t1),
    (sel_switch make_cmp_eq make_cmp_ltu make_sub make_to_int arg t2))
| CTjumptable (ofs, sz, tbl, t') ->
  XElet ((make_sub (Eletvar arg) ofs), (XEcondition
    ((condexpr_of_expr (make_cmp_ltu (Eletvar O) sz)), (XEjumptable
    ((make_to_int (Eletvar O)), tbl)),
    (sel_switch make_cmp_eq make_cmp_ltu make_sub make_to_int (S arg) t'))))

(** val sel_switch_int : nat -> comptree -> exitexpr **)

let sel_switch_int =
  sel_switch (fun arg n ->
    comp Ceq arg (Eop ((Ointconst (Int.repr n)), Enil))) (fun arg n ->
    compu Clt arg (Eop ((Ointconst (Int.repr n)), Enil))) (fun arg ofs ->
    sub arg (Eop ((Ointconst (Int.repr ofs)), Enil))) (fun arg -> arg)

(** val sel_switch_long : nat -> comptree -> exitexpr **)

let sel_switch_long =
  sel_switch (fun arg n ->
    SelectLong.cmpl Ceq arg (SelectLong.longconst (Int64.repr n)))
    (fun arg n ->
    SelectLong.cmplu Clt arg (SelectLong.longconst (Int64.repr n)))
    (fun arg ofs ->
    SelectLong.subl arg (SelectLong.longconst (Int64.repr ofs))) lowlong

type stmt_class =
| SCskip
| SCassign of ident * Cminor.expr
| SCother

(** val classify_stmt : Cminor.stmt -> stmt_class **)

let rec classify_stmt = function
| Cminor.Sskip -> SCskip
| Cminor.Sassign (id, a) -> SCassign (id, a)
| Cminor.Sseq (s0, s1) ->
  (match s0 with
   | Cminor.Sskip -> classify_stmt s1
   | _ -> (match s1 with
           | Cminor.Sskip -> classify_stmt s0
           | _ -> SCother))
| _ -> SCother

(** val if_conversion_heuristic :
    Cminor.expr -> Cminor.expr -> Cminor.expr -> typ -> bool **)

let if_conversion_heuristic = Selectionaux.if_conversion_heuristic

(** val if_conversion_base :
    helper_functions -> known_idents -> typenv -> Cminor.expr -> ident ->
    Cminor.expr -> Cminor.expr -> stmt option **)

let if_conversion_base hf ki env cond id ifso ifnot =
  let ty = env id in
  if (&&)
       ((&&) ((&&) (is_known ki id) (safe_expr ki ifso)) (safe_expr ki ifnot))
       (if_conversion_heuristic cond ifso ifnot ty)
  then option_map (fun sel -> Sassign (id, sel))
         (sel_select_opt hf ty cond ifso ifnot)
  else None

(** val if_conversion :
    helper_functions -> known_idents -> typenv -> Cminor.expr -> Cminor.stmt
    -> Cminor.stmt -> stmt option **)

let if_conversion hf ki env cond ifso ifnot =
  match classify_stmt ifso with
  | SCskip ->
    (match classify_stmt ifnot with
     | SCassign (id, a) ->
       if_conversion_base hf ki env cond id (Cminor.Evar id) a
     | _ -> None)
  | SCassign (id1, a1) ->
    (match classify_stmt ifnot with
     | SCskip -> if_conversion_base hf ki env cond id1 a1 (Cminor.Evar id1)
     | SCassign (id2, a2) ->
       if ident_eq id1 id2
       then if_conversion_base hf ki env cond id1 a1 a2
       else None
     | SCother -> None)
  | SCother -> None

(** val sel_stmt :
    globdef PTree.t -> helper_functions -> known_idents -> typenv ->
    Cminor.stmt -> stmt res **)

let rec sel_stmt defmap hf ki env = function
| Cminor.Sskip -> OK Sskip
| Cminor.Sassign (id, e) -> OK (Sassign (id, (sel_expr hf e)))
| Cminor.Sstore (chunk, addr, rhs) ->
  OK (store chunk (sel_expr hf addr) (sel_expr hf rhs))
| Cminor.Scall (optid, sg, fn, args) ->
  OK
    (match classify_call defmap fn with
     | Call_default ->
       Scall (optid, sg, (Coq_inl (sel_expr hf fn)), (sel_exprlist hf args))
     | Call_imm id -> Scall (optid, sg, (Coq_inr id), (sel_exprlist hf args))
     | Call_builtin ef -> sel_builtin hf optid ef args)
| Cminor.Stailcall (sg, fn, args) ->
  OK
    (match classify_call defmap fn with
     | Call_imm id -> Stailcall (sg, (Coq_inr id), (sel_exprlist hf args))
     | _ -> Stailcall (sg, (Coq_inl (sel_expr hf fn)), (sel_exprlist hf args)))
| Cminor.Sbuiltin (optid, ef, args) -> OK (sel_builtin hf optid ef args)
| Cminor.Sseq (s1, s2) ->
  (match sel_stmt defmap hf ki env s1 with
   | OK x ->
     (match sel_stmt defmap hf ki env s2 with
      | OK x0 -> OK (Sseq (x, x0))
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Cminor.Sifthenelse (e, ifso, ifnot) ->
  (match if_conversion hf ki env e ifso ifnot with
   | Some s0 -> OK s0
   | None ->
     (match sel_stmt defmap hf ki env ifso with
      | OK x ->
        (match sel_stmt defmap hf ki env ifnot with
         | OK x0 ->
           OK (Sifthenelse ((condexpr_of_expr (sel_expr hf e)), x, x0))
         | Error msg0 -> Error msg0)
      | Error msg0 -> Error msg0))
| Cminor.Sloop body ->
  (match sel_stmt defmap hf ki env body with
   | OK x -> OK (Sloop x)
   | Error msg0 -> Error msg0)
| Cminor.Sblock body ->
  (match sel_stmt defmap hf ki env body with
   | OK x -> OK (Sblock x)
   | Error msg0 -> Error msg0)
| Cminor.Sexit n -> OK (Sexit n)
| Cminor.Sswitch (b, e, cases, dfl) ->
  if b
  then let t0 = compile_switch Int64.modulus dfl cases in
       if validate_switch Int64.modulus dfl cases t0
       then OK (Sswitch (XElet ((sel_expr hf e), (sel_switch_long O t0))))
       else Error
              (msg
                ('S'::('e'::('l'::('e'::('c'::('t'::('i'::('o'::('n'::(':'::(' '::('b'::('a'::('d'::(' '::('s'::('w'::('i'::('t'::('c'::('h'::(' '::('('::('l'::('o'::('n'::('g'::(')'::[])))))))))))))))))))))))))))))
  else let t0 = compile_switch Int.modulus dfl cases in
       if validate_switch Int.modulus dfl cases t0
       then OK (Sswitch (XElet ((sel_expr hf e), (sel_switch_int O t0))))
       else Error
              (msg
                ('S'::('e'::('l'::('e'::('c'::('t'::('i'::('o'::('n'::(':'::(' '::('b'::('a'::('d'::(' '::('s'::('w'::('i'::('t'::('c'::('h'::(' '::('('::('i'::('n'::('t'::(')'::[]))))))))))))))))))))))))))))
| Cminor.Sreturn o ->
  (match o with
   | Some e -> OK (Sreturn (Some (sel_expr hf e)))
   | None -> OK (Sreturn None))
| Cminor.Slabel (lbl, body) ->
  (match sel_stmt defmap hf ki env body with
   | OK x -> OK (Slabel (lbl, x))
   | Error msg0 -> Error msg0)
| Cminor.Sgoto lbl -> OK (Sgoto lbl)

(** val known_id : Cminor.coq_function -> known_idents **)

let known_id f =
  let add0 = fun ki id -> PTree.set id () ki in
  fold_left add0 f.Cminor.fn_vars
    (fold_left add0 f.Cminor.fn_params PTree.empty)

(** val sel_function :
    globdef PTree.t -> helper_functions -> Cminor.coq_function ->
    coq_function res **)

let sel_function dm hf f =
  let ki = known_id f in
  (match type_function f with
   | OK x ->
     (match sel_stmt dm hf ki x f.Cminor.fn_body with
      | OK x0 ->
        OK { fn_sig = f.Cminor.fn_sig; fn_params = f.Cminor.fn_params;
          fn_vars = f.Cminor.fn_vars; fn_stackspace = f.Cminor.fn_stackspace;
          fn_body = x0 }
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)

(** val sel_fundef :
    globdef PTree.t -> helper_functions -> Cminor.fundef -> fundef res **)

let sel_fundef dm hf f =
  transf_partial_fundef (sel_function dm hf) f

(** val globdef_of_interest : globdef -> bool **)

let globdef_of_interest = function
| Gfun f ->
  (match f with
   | Internal _ -> false
   | External e -> (match e with
                    | EF_runtime (_, _) -> true
                    | _ -> false))
| Gvar _ -> false

(** val record_globdefs : globdef PTree.t -> globdef PTree.t **)

let record_globdefs defmap =
  PTree.fold (fun m id gd ->
    if globdef_of_interest gd then PTree.set id gd m else m) defmap
    PTree.empty

(** val lookup_helper_aux :
    char list -> signature -> ident option -> ident -> globdef -> ident option **)

let lookup_helper_aux name sg res0 id = function
| Gfun f ->
  (match f with
   | Internal _ -> res0
   | External e ->
     (match e with
      | EF_runtime (name', sg') ->
        if (&&) ((fun x -> x) (string_dec name name'))
             ((fun x -> x) (signature_eq sg sg'))
        then Some id
        else res0
      | _ -> res0))
| Gvar _ -> res0

(** val lookup_helper :
    globdef PTree.t -> char list -> signature -> ident res **)

let lookup_helper globs name sg =
  match PTree.fold (lookup_helper_aux name sg) globs None with
  | Some id -> OK id
  | None ->
    Error ((MSG name) :: ((MSG
      (':'::(' '::('m'::('i'::('s'::('s'::('i'::('n'::('g'::(' '::('o'::('r'::(' '::('i'::('n'::('c'::('o'::('r'::('r'::('e'::('c'::('t'::(' '::('d'::('e'::('c'::('l'::('a'::('r'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))) :: []))

(** val get_helpers : globdef PTree.t -> helper_functions res **)

let get_helpers defmap =
  let globs = record_globdefs defmap in
  (match lookup_helper globs
           ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('d'::('t'::('o'::('s'::[])))))))))))))))))))
           sig_f_l with
   | OK x ->
     (match lookup_helper globs
              ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('d'::('t'::('o'::('u'::[])))))))))))))))))))
              sig_f_l with
      | OK x0 ->
        (match lookup_helper globs
                 ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('t'::('o'::('d'::[])))))))))))))))))))
                 sig_l_f with
         | OK x1 ->
           (match lookup_helper globs
                    ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('u'::('t'::('o'::('d'::[])))))))))))))))))))
                    sig_l_f with
            | OK x2 ->
              (match lookup_helper globs
                       ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('t'::('o'::('f'::[])))))))))))))))))))
                       sig_l_s with
               | OK x3 ->
                 (match lookup_helper globs
                          ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('u'::('t'::('o'::('f'::[])))))))))))))))))))
                          sig_l_s with
                  | OK x4 ->
                    (match lookup_helper globs
                             ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('d'::('i'::('v'::[])))))))))))))))))))
                             sig_ll_l with
                     | OK x5 ->
                       (match lookup_helper globs
                                ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('u'::('d'::('i'::('v'::[])))))))))))))))))))
                                sig_ll_l with
                        | OK x6 ->
                          (match lookup_helper globs
                                   ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('m'::('o'::('d'::[])))))))))))))))))))
                                   sig_ll_l with
                           | OK x7 ->
                             (match lookup_helper globs
                                      ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('u'::('m'::('o'::('d'::[])))))))))))))))))))
                                      sig_ll_l with
                              | OK x8 ->
                                (match lookup_helper globs
                                         ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('h'::('l'::[]))))))))))))))))))
                                         sig_li_l with
                                 | OK x9 ->
                                   (match lookup_helper globs
                                            ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('h'::('r'::[]))))))))))))))))))
                                            sig_li_l with
                                    | OK x10 ->
                                      (match lookup_helper globs
                                               ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('a'::('r'::[]))))))))))))))))))
                                               sig_li_l with
                                       | OK x11 ->
                                         (match lookup_helper globs
                                                  ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('u'::('m'::('u'::('l'::('h'::[]))))))))))))))))))))
                                                  sig_ll_l with
                                          | OK x12 ->
                                            (match lookup_helper globs
                                                     ('_'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('i'::('6'::('4'::('_'::('s'::('m'::('u'::('l'::('h'::[]))))))))))))))))))))
                                                     sig_ll_l with
                                             | OK x13 ->
                                               OK { i64_dtos = x; i64_dtou =
                                                 x0; i64_stod = x1;
                                                 i64_utod = x2; i64_stof =
                                                 x3; i64_utof = x4;
                                                 i64_sdiv = x5; i64_udiv =
                                                 x6; i64_smod = x7;
                                                 i64_umod = x8; i64_shl = x9;
                                                 i64_shr = x10; i64_sar =
                                                 x11; i64_umulh = x12;
                                                 i64_smulh = x13 }
                                             | Error msg0 -> Error msg0)
                                          | Error msg0 -> Error msg0)
                                       | Error msg0 -> Error msg0)
                                    | Error msg0 -> Error msg0)
                                 | Error msg0 -> Error msg0)
                              | Error msg0 -> Error msg0)
                           | Error msg0 -> Error msg0)
                        | Error msg0 -> Error msg0)
                     | Error msg0 -> Error msg0)
                  | Error msg0 -> Error msg0)
               | Error msg0 -> Error msg0)
            | Error msg0 -> Error msg0)
         | Error msg0 -> Error msg0)
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)

(** val sel_program : Cminor.program -> program res **)

let sel_program p =
  let dm = prog_defmap p in
  (match get_helpers dm with
   | OK x -> transform_partial_program (sel_fundef dm x) p
   | Error msg0 -> Error msg0)
