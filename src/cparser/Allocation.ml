open AST
open Archi
open BinNums
open BinPos
open Conventions
open Conventions1
open Coqlib
open Datatypes
open Errors
open FSetAVLplus
open Floats
open Int0
open Integers
open Kildall
open LTL
open Lattice
open List0
open Locations
open Machregs
open Maps
open Op
open Ordered
open RTL
open RTLtyping
open Registers

type move =
| MV of loc * loc
| MVmakelong of mreg * mreg * mreg
| MVlowlong of mreg * mreg
| MVhighlong of mreg * mreg

type moves = move list

type block_shape =
| BSnop of moves * LTL.node
| BSmove of reg * reg * moves * LTL.node
| BSmakelong of reg * reg * reg * moves * LTL.node
| BSlowlong of reg * reg * moves * LTL.node
| BShighlong of reg * reg * moves * LTL.node
| BSop of operation * reg list * reg * moves * mreg list * mreg * moves
   * LTL.node
| BSopdead of operation * reg list * reg * moves * LTL.node
| BSload of memory_chunk * addressing * reg list * reg * moves * mreg list
   * mreg * moves * LTL.node
| BSloaddead of memory_chunk * addressing * reg list * reg * moves * LTL.node
| BSload2 of addressing * addressing * reg list * reg * moves * mreg list
   * mreg * moves * mreg list * mreg * moves * LTL.node
| BSload2_1 of addressing * reg list * reg * moves * mreg list * mreg * 
   moves * LTL.node
| BSload2_2 of addressing * addressing * reg list * reg * moves * mreg list
   * mreg * moves * LTL.node
| BSstore of memory_chunk * addressing * reg list * reg * moves * mreg list
   * mreg * LTL.node
| BSstore2 of addressing * addressing * reg list * reg * moves * mreg list
   * mreg * moves * mreg list * mreg * LTL.node
| BScall of signature * (reg, ident) sum * reg list * reg * moves
   * (mreg, ident) sum * moves * LTL.node
| BStailcall of signature * (reg, ident) sum * reg list * moves
   * (mreg, ident) sum
| BSbuiltin of external_function * reg builtin_arg list * reg builtin_res
   * moves * loc builtin_arg list * mreg builtin_res * moves * LTL.node
| BScond of condition * reg list * moves * mreg list * LTL.node * LTL.node
| BSjumptable of reg * moves * mreg * LTL.node list
| BSreturn of reg option * moves

type 'a operation_kind =
| Coq_operation_Omove of 'a
| Coq_operation_Omakelong of 'a * 'a
| Coq_operation_Olowlong of 'a
| Coq_operation_Ohighlong of 'a
| Coq_operation_other of operation * 'a list

(** val classify_operation : operation -> 'a1 list -> 'a1 operation_kind **)

let classify_operation op args =
  match op with
  | Omove ->
    let op0 = Omove in
    (match args with
     | [] -> Coq_operation_other (op0, [])
     | arg :: l ->
       (match l with
        | [] -> Coq_operation_Omove arg
        | a :: l0 -> Coq_operation_other (op0, (arg :: (a :: l0)))))
  | Omakelong ->
    let op0 = Omakelong in
    (match args with
     | [] -> Coq_operation_other (op0, [])
     | arg1 :: l ->
       (match l with
        | [] -> Coq_operation_other (op0, (arg1 :: []))
        | arg2 :: l0 ->
          (match l0 with
           | [] -> Coq_operation_Omakelong (arg1, arg2)
           | a :: l1 ->
             Coq_operation_other (op0, (arg1 :: (arg2 :: (a :: l1)))))))
  | Olowlong ->
    let op0 = Olowlong in
    (match args with
     | [] -> Coq_operation_other (op0, [])
     | arg :: l ->
       (match l with
        | [] -> Coq_operation_Olowlong arg
        | a :: l0 -> Coq_operation_other (op0, (arg :: (a :: l0)))))
  | Ohighlong ->
    let op0 = Ohighlong in
    (match args with
     | [] -> Coq_operation_other (op0, [])
     | arg :: l ->
       (match l with
        | [] -> Coq_operation_Ohighlong arg
        | a :: l0 -> Coq_operation_other (op0, (arg :: (a :: l0)))))
  | x -> Coq_operation_other (x, args)

(** val extract_moves : moves -> bblock -> moves * bblock **)

let rec extract_moves accu b = match b with
| [] -> ((rev accu), b)
| i :: b' ->
  (match i with
   | Lop (op, args, res0) ->
     (match is_move_operation op args with
      | Some arg -> extract_moves ((MV ((R arg), (R res0))) :: accu) b'
      | None -> ((rev accu), b))
   | Lgetstack (sl, ofs, ty, dst) ->
     extract_moves ((MV ((S (sl, ofs, ty)), (R dst))) :: accu) b'
   | Lsetstack (src, sl, ofs, ty) ->
     extract_moves ((MV ((R src), (S (sl, ofs, ty)))) :: accu) b'
   | _ -> ((rev accu), b))

(** val extract_moves_ext : moves -> bblock -> moves * bblock **)

let rec extract_moves_ext accu b = match b with
| [] -> ((rev accu), b)
| i :: b' ->
  (match i with
   | Lop (op, args, res0) ->
     (match classify_operation op args with
      | Coq_operation_Omove arg ->
        extract_moves_ext ((MV ((R arg), (R res0))) :: accu) b'
      | Coq_operation_Omakelong (arg1, arg2) ->
        extract_moves_ext ((MVmakelong (arg1, arg2, res0)) :: accu) b'
      | Coq_operation_Olowlong arg ->
        extract_moves_ext ((MVlowlong (arg, res0)) :: accu) b'
      | Coq_operation_Ohighlong arg ->
        extract_moves_ext ((MVhighlong (arg, res0)) :: accu) b'
      | Coq_operation_other (_, _) -> ((rev accu), b))
   | Lgetstack (sl, ofs, ty, dst) ->
     extract_moves_ext ((MV ((S (sl, ofs, ty)), (R dst))) :: accu) b'
   | Lsetstack (src, sl, ofs, ty) ->
     extract_moves_ext ((MV ((R src), (S (sl, ofs, ty)))) :: accu) b'
   | _ -> ((rev accu), b))

(** val check_succ : LTL.node -> bblock -> bool **)

let check_succ s = function
| [] -> false
| i :: _ -> (match i with
             | Lbranch s' -> (fun x -> x) (peq s s')
             | _ -> false)

(** val pair_Iop_block :
    operation -> reg list -> reg -> LTL.node -> bblock -> block_shape option **)

let pair_Iop_block op args res0 s b =
  let (mv1, b1) = extract_moves [] b in
  (match b1 with
   | [] ->
     if check_succ s b1
     then Some (BSopdead (op, args, res0, mv1, s))
     else None
   | i :: b2 ->
     (match i with
      | Lop (op', args', res') ->
        let (mv2, b3) = extract_moves [] b2 in
        if eq_operation op op'
        then if check_succ s b3
             then Some (BSop (op, args, res0, mv1, args', res', mv2, s))
             else None
        else None
      | _ ->
        if check_succ s b1
        then Some (BSopdead (op, args, res0, mv1, s))
        else None))

(** val pair_instr_block : instruction -> bblock -> block_shape option **)

let pair_instr_block i b =
  match i with
  | Inop s ->
    let (mv, b1) = extract_moves [] b in
    if check_succ s b1 then Some (BSnop (mv, s)) else None
  | Iop (op, args, res0, s) ->
    (match classify_operation op args with
     | Coq_operation_Omove arg ->
       let (mv, b1) = extract_moves [] b in
       if check_succ s b1 then Some (BSmove (arg, res0, mv, s)) else None
     | Coq_operation_Omakelong (arg1, arg2) ->
       if splitlong
       then let (mv, b1) = extract_moves [] b in
            if check_succ s b1
            then Some (BSmakelong (arg1, arg2, res0, mv, s))
            else None
       else pair_Iop_block op args res0 s b
     | Coq_operation_Olowlong arg ->
       if splitlong
       then let (mv, b1) = extract_moves [] b in
            if check_succ s b1
            then Some (BSlowlong (arg, res0, mv, s))
            else None
       else pair_Iop_block op args res0 s b
     | Coq_operation_Ohighlong arg ->
       if splitlong
       then let (mv, b1) = extract_moves [] b in
            if check_succ s b1
            then Some (BShighlong (arg, res0, mv, s))
            else None
       else pair_Iop_block op args res0 s b
     | Coq_operation_other (_, _) -> pair_Iop_block op args res0 s b)
  | Iload (chunk, addr, args, dst, s) ->
    let (mv1, b1) = extract_moves [] b in
    (match b1 with
     | [] ->
       if check_succ s b1
       then Some (BSloaddead (chunk, addr, args, dst, mv1, s))
       else None
     | i0 :: b2 ->
       (match i0 with
        | Lload (chunk', addr', args', dst') ->
          if (&&) ((fun x -> x) (chunk_eq chunk Mint64)) splitlong
          then if chunk_eq chunk' Mint32
               then let (mv2, b3) = extract_moves [] b2 in
                    (match b3 with
                     | [] ->
                       if check_succ s b3
                       then if eq_addressing addr addr'
                            then Some (BSload2_1 (addr, args, dst, mv1,
                                   args', dst', mv2, s))
                            else if option_eq eq_addressing
                                      (offset_addressing addr (Zpos (Coq_xO
                                        (Coq_xO Coq_xH)))) (Some addr')
                                 then Some (BSload2_2 (addr, addr', args,
                                        dst, mv1, args', dst', mv2, s))
                                 else None
                       else None
                     | i1 :: b4 ->
                       (match i1 with
                        | Lload (chunk'', addr'', args'', dst'') ->
                          let (mv3, b5) = extract_moves [] b4 in
                          if chunk_eq chunk'' Mint32
                          then if eq_addressing addr addr'
                               then if option_eq eq_addressing
                                         (offset_addressing addr (Zpos
                                           (Coq_xO (Coq_xO Coq_xH)))) (Some
                                         addr'')
                                    then if check_succ s b5
                                         then Some (BSload2 (addr, addr'',
                                                args, dst, mv1, args', dst',
                                                mv2, args'', dst'', mv3, s))
                                         else None
                                    else None
                               else None
                          else None
                        | _ ->
                          if check_succ s b3
                          then if eq_addressing addr addr'
                               then Some (BSload2_1 (addr, args, dst, mv1,
                                      args', dst', mv2, s))
                               else if option_eq eq_addressing
                                         (offset_addressing addr (Zpos
                                           (Coq_xO (Coq_xO Coq_xH)))) (Some
                                         addr')
                                    then Some (BSload2_2 (addr, addr', args,
                                           dst, mv1, args', dst', mv2, s))
                                    else None
                          else None))
               else None
          else let (mv2, b3) = extract_moves [] b2 in
               if chunk_eq chunk chunk'
               then if eq_addressing addr addr'
                    then if check_succ s b3
                         then Some (BSload (chunk, addr, args, dst, mv1,
                                args', dst', mv2, s))
                         else None
                    else None
               else None
        | _ ->
          if check_succ s b1
          then Some (BSloaddead (chunk, addr, args, dst, mv1, s))
          else None))
  | Istore (chunk, addr, args, src, s) ->
    let (mv1, b1) = extract_moves [] b in
    (match b1 with
     | [] -> None
     | i0 :: b2 ->
       (match i0 with
        | Lstore (chunk', addr', args', src') ->
          if (&&) ((fun x -> x) (chunk_eq chunk Mint64)) splitlong
          then let (mv2, b3) = extract_moves [] b2 in
               (match b3 with
                | [] -> None
                | i1 :: b4 ->
                  (match i1 with
                   | Lstore (chunk'', addr'', args'', src'') ->
                     if chunk_eq chunk' Mint32
                     then if chunk_eq chunk'' Mint32
                          then if eq_addressing addr addr'
                               then if option_eq eq_addressing
                                         (offset_addressing addr (Zpos
                                           (Coq_xO (Coq_xO Coq_xH)))) (Some
                                         addr'')
                                    then if check_succ s b4
                                         then Some (BSstore2 (addr, addr'',
                                                args, src, mv1, args', src',
                                                mv2, args'', src'', s))
                                         else None
                                    else None
                               else None
                          else None
                     else None
                   | _ -> None))
          else if chunk_eq chunk chunk'
               then if eq_addressing addr addr'
                    then if check_succ s b2
                         then Some (BSstore (chunk, addr, args, src, mv1,
                                args', src', s))
                         else None
                    else None
               else None
        | _ -> None))
  | Icall (sg, ros, args, res0, s) ->
    let (mv1, b1) = extract_moves_ext [] b in
    (match b1 with
     | [] -> None
     | i0 :: b2 ->
       (match i0 with
        | Lcall (sg', ros') ->
          let (mv2, b3) = extract_moves_ext [] b2 in
          if signature_eq sg sg'
          then if check_succ s b3
               then Some (BScall (sg, ros, args, res0, mv1, ros', mv2, s))
               else None
          else None
        | _ -> None))
  | Itailcall (sg, ros, args) ->
    let (mv1, b1) = extract_moves_ext [] b in
    (match b1 with
     | [] -> None
     | i0 :: _ ->
       (match i0 with
        | Ltailcall (sg', ros') ->
          if signature_eq sg sg'
          then Some (BStailcall (sg, ros, args, mv1, ros'))
          else None
        | _ -> None))
  | Ibuiltin (ef, args, res0, s) ->
    let (mv1, b1) = extract_moves [] b in
    (match b1 with
     | [] -> None
     | i0 :: b2 ->
       (match i0 with
        | Lbuiltin (ef', args', res') ->
          let (mv2, b3) = extract_moves [] b2 in
          if external_function_eq ef ef'
          then if check_succ s b3
               then Some (BSbuiltin (ef, args, res0, mv1, args', res', mv2,
                      s))
               else None
          else None
        | _ -> None))
  | Icond (cond, args, s1, s2) ->
    let (mv1, b1) = extract_moves [] b in
    (match b1 with
     | [] -> None
     | i0 :: _ ->
       (match i0 with
        | Lcond (cond', args', s1', s2') ->
          if eq_condition cond cond'
          then if peq s1 s1'
               then if peq s2 s2'
                    then Some (BScond (cond, args, mv1, args', s1, s2))
                    else None
               else None
          else None
        | _ -> None))
  | Ijumptable (arg, tbl) ->
    let (mv1, b1) = extract_moves [] b in
    (match b1 with
     | [] -> None
     | i0 :: _ ->
       (match i0 with
        | Ljumptable (arg', tbl') ->
          if list_eq_dec peq tbl tbl'
          then Some (BSjumptable (arg, mv1, arg', tbl))
          else None
        | _ -> None))
  | Ireturn arg ->
    let (mv1, b1) = extract_moves_ext [] b in
    (match b1 with
     | [] -> None
     | i0 :: _ ->
       (match i0 with
        | Lreturn -> Some (BSreturn (arg, mv1))
        | _ -> None))

(** val pair_codes :
    coq_function -> LTL.coq_function -> block_shape PTree.t **)

let pair_codes f1 f2 =
  PTree.combine (fun opti optb ->
    match opti with
    | Some i ->
      (match optb with
       | Some b -> pair_instr_block i b
       | None -> None)
    | None -> None) f1.fn_code f2.LTL.fn_code

(** val pair_entrypoints :
    coq_function -> LTL.coq_function -> moves option **)

let pair_entrypoints f1 f2 =
  match PTree.get f2.LTL.fn_entrypoint f2.LTL.fn_code with
  | Some b ->
    let (mv, b1) = extract_moves_ext [] b in
    if check_succ f1.fn_entrypoint b1 then Some mv else None
  | None -> None

type equation_kind =
| Full
| Low
| High

type equation = { ekind : equation_kind; ereg : reg; eloc : loc }

module IndexedEqKind =
 struct
  type t = equation_kind

  (** val index : t -> positive **)

  let index = function
  | Full -> Coq_xH
  | Low -> Coq_xO Coq_xH
  | High -> Coq_xI Coq_xH

  (** val eq : t -> t -> bool **)

  let eq x y =
    match x with
    | Full -> (match y with
               | Full -> true
               | _ -> false)
    | Low -> (match y with
              | Low -> true
              | _ -> false)
    | High -> (match y with
               | High -> true
               | _ -> false)
 end

module OrderedEqKind = OrderedIndexed(IndexedEqKind)

module OrderedEquation =
 struct
  type t = equation

  (** val compare : t -> t -> t OrderedType.coq_Compare **)

  let compare x y =
    let c = OrderedPositive.compare x.ereg y.ereg in
    (match c with
     | OrderedType.LT -> OrderedType.LT
     | OrderedType.EQ ->
       let c0 = OrderedLoc.compare x.eloc y.eloc in
       (match c0 with
        | OrderedType.LT -> OrderedType.LT
        | OrderedType.EQ ->
          let c1 = OrderedEqKind.compare x.ekind y.ekind in
          (match c1 with
           | OrderedType.LT -> OrderedType.LT
           | OrderedType.EQ -> OrderedType.EQ
           | OrderedType.GT -> OrderedType.GT)
        | OrderedType.GT -> OrderedType.GT)
     | OrderedType.GT -> OrderedType.GT)

  (** val eq_dec : t -> t -> bool **)

  let eq_dec x y =
    let { ekind = ekind0; ereg = ereg0; eloc = eloc0 } = x in
    let { ekind = ekind1; ereg = ereg1; eloc = eloc1 } = y in
    if IndexedEqKind.eq ekind0 ekind1
    then if peq ereg0 ereg1 then Loc.eq eloc0 eloc1 else false
    else false
 end

module OrderedEquation' =
 struct
  type t = equation

  (** val compare : t -> t -> t OrderedType.coq_Compare **)

  let compare x y =
    let c = OrderedLoc.compare x.eloc y.eloc in
    (match c with
     | OrderedType.LT -> OrderedType.LT
     | OrderedType.EQ ->
       let c0 = OrderedPositive.compare x.ereg y.ereg in
       (match c0 with
        | OrderedType.LT -> OrderedType.LT
        | OrderedType.EQ ->
          let c1 = OrderedEqKind.compare x.ekind y.ekind in
          (match c1 with
           | OrderedType.LT -> OrderedType.LT
           | OrderedType.EQ -> OrderedType.EQ
           | OrderedType.GT -> OrderedType.GT)
        | OrderedType.GT -> OrderedType.GT)
     | OrderedType.GT -> OrderedType.GT)

  (** val eq_dec : t -> t -> bool **)

  let eq_dec =
    OrderedEquation.eq_dec
 end

module EqSet = FSetAVLplus.Make(OrderedEquation)

module EqSet2 = FSetAVLplus.Make(OrderedEquation')

type eqs = { eqs1 : EqSet.t; eqs2 : EqSet2.t }

(** val empty_eqs : eqs **)

let empty_eqs =
  { eqs1 = EqSet.empty; eqs2 = EqSet2.empty }

(** val add_equation : equation -> eqs -> eqs **)

let add_equation q e =
  { eqs1 = (EqSet.add q e.eqs1); eqs2 = (EqSet2.add q e.eqs2) }

(** val remove_equation : equation -> eqs -> eqs **)

let remove_equation q e =
  { eqs1 = (EqSet.remove q e.eqs1); eqs2 = (EqSet2.remove q e.eqs2) }

(** val select_reg_l : reg -> equation -> bool **)

let select_reg_l r q =
  Pos.leb r q.ereg

(** val select_reg_h : reg -> equation -> bool **)

let select_reg_h r q =
  Pos.leb q.ereg r

(** val reg_unconstrained : reg -> eqs -> bool **)

let reg_unconstrained r e =
  negb (EqSet.mem_between (select_reg_l r) (select_reg_h r) e.eqs1)

(** val select_loc_l : loc -> equation -> bool **)

let select_loc_l l =
  let lb = OrderedLoc.diff_low_bound l in
  (fun q ->
  match OrderedLoc.compare q.eloc lb with
  | OrderedType.LT -> false
  | _ -> true)

(** val select_loc_h : loc -> equation -> bool **)

let select_loc_h l =
  let lh = OrderedLoc.diff_high_bound l in
  (fun q ->
  match OrderedLoc.compare q.eloc lh with
  | OrderedType.GT -> false
  | _ -> true)

(** val loc_unconstrained : loc -> eqs -> bool **)

let loc_unconstrained l e =
  negb (EqSet2.mem_between (select_loc_l l) (select_loc_h l) e.eqs2)

(** val reg_loc_unconstrained : reg -> loc -> eqs -> bool **)

let reg_loc_unconstrained r l e =
  (&&) (reg_unconstrained r e) (loc_unconstrained l e)

(** val subst_reg : reg -> reg -> eqs -> eqs **)

let subst_reg r1 r2 e =
  EqSet.fold (fun q e0 ->
    add_equation { ekind = q.ekind; ereg = r2; eloc = q.eloc }
      (remove_equation q e0))
    (EqSet.elements_between (select_reg_l r1) (select_reg_h r1) e.eqs1) e

(** val subst_reg_kind :
    reg -> equation_kind -> reg -> equation_kind -> eqs -> eqs **)

let subst_reg_kind r1 k1 r2 k2 e =
  EqSet.fold (fun q e0 ->
    if IndexedEqKind.eq q.ekind k1
    then add_equation { ekind = k2; ereg = r2; eloc = q.eloc }
           (remove_equation q e0)
    else e0)
    (EqSet.elements_between (select_reg_l r1) (select_reg_h r1) e.eqs1) e

(** val subst_loc : loc -> loc -> eqs -> eqs option **)

let subst_loc l1 l2 e =
  EqSet2.fold (fun q opte ->
    match opte with
    | Some e0 ->
      if Loc.eq l1 q.eloc
      then Some
             (add_equation { ekind = q.ekind; ereg = q.ereg; eloc = l2 }
               (remove_equation q e0))
      else None
    | None -> None)
    (EqSet2.elements_between (select_loc_l l1) (select_loc_h l1) e.eqs2)
    (Some e)

(** val subst_loc_part : loc -> loc -> equation_kind -> eqs -> eqs option **)

let subst_loc_part l1 l2 k e =
  EqSet2.fold (fun q opte ->
    match opte with
    | Some e0 ->
      if Loc.eq l1 q.eloc
      then if IndexedEqKind.eq q.ekind k
           then Some
                  (add_equation { ekind = Full; ereg = q.ereg; eloc = l2 }
                    (remove_equation q e0))
           else None
      else None
    | None -> None)
    (EqSet2.elements_between (select_loc_l l1) (select_loc_h l1) e.eqs2)
    (Some e)

(** val subst_loc_pair : loc -> loc -> loc -> eqs -> eqs option **)

let subst_loc_pair l1 l2 l2' e =
  EqSet2.fold (fun q opte ->
    match opte with
    | Some e0 ->
      if Loc.eq l1 q.eloc
      then if IndexedEqKind.eq q.ekind Full
           then Some
                  (add_equation { ekind = High; ereg = q.ereg; eloc = l2 }
                    (add_equation { ekind = Low; ereg = q.ereg; eloc = l2' }
                      (remove_equation q e0)))
           else None
      else None
    | None -> None)
    (EqSet2.elements_between (select_loc_l l1) (select_loc_h l1) e.eqs2)
    (Some e)

(** val sel_type : equation_kind -> typ -> typ **)

let sel_type k ty =
  match k with
  | Full -> ty
  | _ -> Tint

(** val loc_type_compat : regenv -> loc -> eqs -> bool **)

let loc_type_compat env l e =
  EqSet2.for_all_between (fun q ->
    subtype (sel_type q.ekind (env q.ereg)) (Loc.coq_type l))
    (select_loc_l l) (select_loc_h l) e.eqs2

(** val long_type_compat : regenv -> loc -> eqs -> bool **)

let long_type_compat env l e =
  EqSet2.for_all_between (fun q -> subtype (env q.ereg) Tlong)
    (select_loc_l l) (select_loc_h l) e.eqs2

(** val add_equations : reg list -> mreg list -> eqs -> eqs option **)

let rec add_equations rl ml e =
  match rl with
  | [] -> (match ml with
           | [] -> Some e
           | _ :: _ -> None)
  | r1 :: rl0 ->
    (match ml with
     | [] -> None
     | m1 :: ml0 ->
       add_equations rl0 ml0
         (add_equation { ekind = Full; ereg = r1; eloc = (R m1) } e))

(** val add_equations_args :
    reg list -> typ list -> loc rpair list -> eqs -> eqs option **)

let rec add_equations_args rl tyl ll e =
  match rl with
  | [] ->
    (match tyl with
     | [] -> (match ll with
              | [] -> Some e
              | _ :: _ -> None)
     | _ :: _ -> None)
  | r1 :: rl0 ->
    (match tyl with
     | [] -> None
     | ty :: tyl0 ->
       (match ty with
        | Tlong ->
          (match ll with
           | [] -> None
           | r :: ll0 ->
             (match r with
              | One l1 ->
                add_equations_args rl0 tyl0 ll0
                  (add_equation { ekind = Full; ereg = r1; eloc = l1 } e)
              | Twolong (l1, l2) ->
                if ptr64
                then None
                else add_equations_args rl0 tyl0 ll0
                       (add_equation { ekind = Low; ereg = r1; eloc = l2 }
                         (add_equation { ekind = High; ereg = r1; eloc = l1 }
                           e))))
        | _ ->
          (match ll with
           | [] -> None
           | r :: ll0 ->
             (match r with
              | One l1 ->
                add_equations_args rl0 tyl0 ll0
                  (add_equation { ekind = Full; ereg = r1; eloc = l1 } e)
              | Twolong (_, _) -> None))))

(** val add_equations_res : reg -> typ -> mreg rpair -> eqs -> eqs option **)

let add_equations_res r ty p e =
  match p with
  | One mr -> Some (add_equation { ekind = Full; ereg = r; eloc = (R mr) } e)
  | Twolong (mr1, mr2) ->
    (match ty with
     | Tlong ->
       if ptr64
       then None
       else Some
              (add_equation { ekind = Low; ereg = r; eloc = (R mr2) }
                (add_equation { ekind = High; ereg = r; eloc = (R mr1) } e))
     | _ -> None)

(** val remove_equations_res : reg -> mreg rpair -> eqs -> eqs option **)

let remove_equations_res r p e =
  match p with
  | One mr ->
    Some (remove_equation { ekind = Full; ereg = r; eloc = (R mr) } e)
  | Twolong (mr1, mr2) ->
    if mreg_eq mr2 mr1
    then None
    else Some
           (remove_equation { ekind = Low; ereg = r; eloc = (R mr2) }
             (remove_equation { ekind = High; ereg = r; eloc = (R mr1) } e))

(** val add_equation_ros :
    (reg, ident) sum -> (mreg, ident) sum -> eqs -> eqs option **)

let add_equation_ros ros ros' e =
  match ros with
  | Coq_inl r ->
    (match ros' with
     | Coq_inl mr ->
       Some (add_equation { ekind = Full; ereg = r; eloc = (R mr) } e)
     | Coq_inr _ -> None)
  | Coq_inr id ->
    (match ros' with
     | Coq_inl _ -> None
     | Coq_inr id' -> if ident_eq id id' then Some e else None)

(** val add_equations_builtin_arg :
    regenv -> reg builtin_arg -> loc builtin_arg -> eqs -> eqs option **)

let rec add_equations_builtin_arg env arg arg' e =
  match arg with
  | BA r ->
    (match arg' with
     | BA l -> Some (add_equation { ekind = Full; ereg = r; eloc = l } e)
     | BA_splitlong (hi, lo) ->
       (match hi with
        | BA lhi ->
          (match lo with
           | BA llo ->
             if typ_eq (env r) Tlong
             then if splitlong
                  then Some
                         (add_equation { ekind = Low; ereg = r; eloc = llo }
                           (add_equation { ekind = High; ereg = r; eloc =
                             lhi } e))
                  else None
             else None
           | _ -> None)
        | _ -> None)
     | _ -> None)
  | BA_int n ->
    (match arg' with
     | BA_int n' -> if Int.eq_dec n n' then Some e else None
     | _ -> None)
  | BA_long n ->
    (match arg' with
     | BA_long n' -> if Int64.eq_dec n n' then Some e else None
     | _ -> None)
  | BA_float f ->
    (match arg' with
     | BA_float f' -> if Float.eq_dec f f' then Some e else None
     | _ -> None)
  | BA_single f ->
    (match arg' with
     | BA_single f' -> if Float32.eq_dec f f' then Some e else None
     | _ -> None)
  | BA_loadstack (chunk, ofs) ->
    (match arg' with
     | BA_loadstack (chunk', ofs') ->
       if chunk_eq chunk chunk'
       then if Ptrofs.eq_dec ofs ofs' then Some e else None
       else None
     | _ -> None)
  | BA_addrstack ofs ->
    (match arg' with
     | BA_addrstack ofs' -> if Ptrofs.eq_dec ofs ofs' then Some e else None
     | _ -> None)
  | BA_loadglobal (chunk, id, ofs) ->
    (match arg' with
     | BA_loadglobal (chunk', id', ofs') ->
       if chunk_eq chunk chunk'
       then if ident_eq id id'
            then if Ptrofs.eq_dec ofs ofs' then Some e else None
            else None
       else None
     | _ -> None)
  | BA_addrglobal (id, ofs) ->
    (match arg' with
     | BA_addrglobal (id', ofs') ->
       if ident_eq id id'
       then if Ptrofs.eq_dec ofs ofs' then Some e else None
       else None
     | _ -> None)
  | BA_splitlong (hi, lo) ->
    (match arg' with
     | BA_splitlong (hi', lo') ->
       (match add_equations_builtin_arg env hi hi' e with
        | Some e1 -> add_equations_builtin_arg env lo lo' e1
        | None -> None)
     | _ -> None)
  | BA_addptr (a1, a2) ->
    (match arg' with
     | BA_addptr (a1', a2') ->
       (match add_equations_builtin_arg env a1 a1' e with
        | Some e1 -> add_equations_builtin_arg env a2 a2' e1
        | None -> None)
     | _ -> None)

(** val add_equations_builtin_args :
    regenv -> reg builtin_arg list -> loc builtin_arg list -> eqs -> eqs
    option **)

let rec add_equations_builtin_args env args args' e =
  match args with
  | [] -> (match args' with
           | [] -> Some e
           | _ :: _ -> None)
  | a1 :: al ->
    (match args' with
     | [] -> None
     | a1' :: al' ->
       (match add_equations_builtin_arg env a1 a1' e with
        | Some e1 -> add_equations_builtin_args env al al' e1
        | None -> None))

(** val add_equations_debug_args :
    regenv -> reg builtin_arg list -> loc builtin_arg list -> eqs -> eqs
    option **)

let rec add_equations_debug_args env args args' e =
  match args with
  | [] -> (match args' with
           | [] -> Some e
           | _ :: _ -> None)
  | a1 :: al ->
    (match args' with
     | [] -> Some e
     | a1' :: al' ->
       (match add_equations_builtin_arg env a1 a1' e with
        | Some e1 -> add_equations_debug_args env al al' e1
        | None -> add_equations_debug_args env al args' e))

(** val remove_equations_builtin_res :
    regenv -> reg builtin_res -> mreg builtin_res -> eqs -> eqs option **)

let remove_equations_builtin_res env res0 res' e =
  match res0 with
  | BR r ->
    (match res' with
     | BR r' ->
       Some (remove_equation { ekind = Full; ereg = r; eloc = (R r') } e)
     | BR_none -> None
     | BR_splitlong (hi, lo) ->
       (match hi with
        | BR rhi ->
          (match lo with
           | BR rlo ->
             if typ_eq (env r) Tlong
             then if mreg_eq rhi rlo
                  then None
                  else Some
                         (remove_equation { ekind = Low; ereg = r; eloc = (R
                           rlo) }
                           (remove_equation { ekind = High; ereg = r; eloc =
                             (R rhi) } e))
             else None
           | _ -> None)
        | _ -> None))
  | BR_none -> (match res' with
                | BR_none -> Some e
                | _ -> None)
  | BR_splitlong (_, _) -> None

(** val can_undef : mreg list -> eqs -> bool **)

let rec can_undef ml e =
  match ml with
  | [] -> true
  | m1 :: ml0 -> (&&) (loc_unconstrained (R m1) e) (can_undef ml0 e)

(** val can_undef_except : loc -> mreg list -> eqs -> bool **)

let rec can_undef_except l ml e =
  match ml with
  | [] -> true
  | m1 :: ml0 ->
    (&&) ((||) ((fun x -> x) (Loc.eq l (R m1))) (loc_unconstrained (R m1) e))
      (can_undef_except l ml0 e)

(** val no_caller_saves : eqs -> bool **)

let no_caller_saves e =
  EqSet.for_all (fun eq0 ->
    match eq0.eloc with
    | R r -> is_callee_save r
    | S (sl, _, _) -> (match sl with
                       | Outgoing -> false
                       | _ -> true)) e.eqs1

(** val compat_left : reg -> loc -> eqs -> bool **)

let compat_left r l e =
  EqSet.for_all_between (fun q ->
    match q.ekind with
    | Full -> (fun x -> x) (Loc.eq l q.eloc)
    | _ -> false) (select_reg_l r) (select_reg_h r) e.eqs1

(** val compat_left2 : reg -> loc -> loc -> eqs -> bool **)

let compat_left2 r l1 l2 e =
  EqSet.for_all_between (fun q ->
    match q.ekind with
    | Full -> false
    | Low -> (fun x -> x) (Loc.eq l2 q.eloc)
    | High -> (fun x -> x) (Loc.eq l1 q.eloc)) (select_reg_l r)
    (select_reg_h r) e.eqs1

(** val ros_compatible_tailcall : (mreg, ident) sum -> bool **)

let ros_compatible_tailcall = function
| Coq_inl r -> negb (is_callee_save r)
| Coq_inr _ -> true

(** val destroyed_by_move : loc -> loc -> mreg list **)

let destroyed_by_move src dst =
  match src with
  | R _ ->
    (match dst with
     | R _ -> destroyed_by_op Omove
     | S (_, _, ty) -> destroyed_by_setstack ty)
  | S (sl, _, _) -> destroyed_by_getstack sl

(** val well_typed_move : regenv -> loc -> eqs -> bool **)

let well_typed_move env dst e =
  match dst with
  | R _ -> true
  | S (_, _, _) -> loc_type_compat env dst e

(** val track_moves : regenv -> moves -> eqs -> eqs option **)

let rec track_moves env mv e =
  match mv with
  | [] -> Some e
  | m :: mv0 ->
    (match m with
     | MV (src, dst) ->
       (match track_moves env mv0 e with
        | Some e1 ->
          if can_undef_except dst (destroyed_by_move src dst) e1
          then if well_typed_move env dst e1
               then subst_loc dst src e1
               else None
          else None
        | None -> None)
     | MVmakelong (src1, src2, dst) ->
       if negb ptr64
       then (match track_moves env mv0 e with
             | Some e1 ->
               if long_type_compat env (R dst) e1
               then subst_loc_pair (R dst) (R src1) (R src2) e1
               else None
             | None -> None)
       else None
     | MVlowlong (src, dst) ->
       if negb ptr64
       then (match track_moves env mv0 e with
             | Some e1 -> subst_loc_part (R dst) (R src) Low e1
             | None -> None)
       else None
     | MVhighlong (src, dst) ->
       if negb ptr64
       then (match track_moves env mv0 e with
             | Some e1 -> subst_loc_part (R dst) (R src) High e1
             | None -> None)
       else None)

(** val transfer_use_def :
    reg list -> reg -> mreg list -> mreg -> mreg list -> eqs -> eqs option **)

let transfer_use_def args res0 args' res' undefs e =
  let e1 = remove_equation { ekind = Full; ereg = res0; eloc = (R res') } e in
  if reg_loc_unconstrained res0 (R res') e1
  then if can_undef undefs e1 then add_equations args args' e1 else None
  else None

(** val kind_first_word : equation_kind **)

let kind_first_word =
  if big_endian then High else Low

(** val kind_second_word : equation_kind **)

let kind_second_word =
  if big_endian then Low else High

(** val transfer_aux :
    coq_function -> regenv -> block_shape -> eqs -> eqs option **)

let transfer_aux f env shape e =
  match shape with
  | BSnop (mv, _) -> track_moves env mv e
  | BSmove (src, dst, mv, _) -> track_moves env mv (subst_reg dst src e)
  | BSmakelong (src1, src2, dst, mv, _) ->
    let e1 = subst_reg_kind dst High src1 Full e in
    let e2 = subst_reg_kind dst Low src2 Full e1 in
    if reg_unconstrained dst e2 then track_moves env mv e2 else None
  | BSlowlong (src, dst, mv, _) ->
    let e1 = subst_reg_kind dst Full src Low e in
    if reg_unconstrained dst e1 then track_moves env mv e1 else None
  | BShighlong (src, dst, mv, _) ->
    let e1 = subst_reg_kind dst Full src High e in
    if reg_unconstrained dst e1 then track_moves env mv e1 else None
  | BSop (op, args, res0, mv1, args', res', mv2, _) ->
    (match track_moves env mv2 e with
     | Some e1 ->
       (match transfer_use_def args res0 args' res' (destroyed_by_op op) e1 with
        | Some e2 -> track_moves env mv1 e2
        | None -> None)
     | None -> None)
  | BSopdead (_, _, res0, mv, _) ->
    if reg_unconstrained res0 e then track_moves env mv e else None
  | BSload (chunk, addr, args, dst, mv1, args', dst', mv2, _) ->
    (match track_moves env mv2 e with
     | Some e1 ->
       (match transfer_use_def args dst args' dst'
                (destroyed_by_load chunk addr) e1 with
        | Some e2 -> track_moves env mv1 e2
        | None -> None)
     | None -> None)
  | BSloaddead (_, _, _, dst, mv, _) ->
    if reg_unconstrained dst e then track_moves env mv e else None
  | BSload2 (addr, addr', args, dst, mv1, args1', dst1', mv2, args2', dst2',
             mv3, _) ->
    (match track_moves env mv3 e with
     | Some e1 ->
       let e2 =
         remove_equation { ekind = kind_second_word; ereg = dst; eloc = (R
           dst2') } e1
       in
       if loc_unconstrained (R dst2') e2
       then if can_undef (destroyed_by_load Mint32 addr') e2
            then (match add_equations args args2' e2 with
                  | Some e3 ->
                    (match track_moves env mv2 e3 with
                     | Some e4 ->
                       let e5 =
                         remove_equation { ekind = kind_first_word; ereg =
                           dst; eloc = (R dst1') } e4
                       in
                       if loc_unconstrained (R dst1') e5
                       then if can_undef (destroyed_by_load Mint32 addr) e5
                            then if reg_unconstrained dst e5
                                 then (match add_equations args args1' e5 with
                                       | Some e6 -> track_moves env mv1 e6
                                       | None -> None)
                                 else None
                            else None
                       else None
                     | None -> None)
                  | None -> None)
            else None
       else None
     | None -> None)
  | BSload2_1 (addr, args, dst, mv1, args', dst', mv2, _) ->
    (match track_moves env mv2 e with
     | Some e1 ->
       let e2 =
         remove_equation { ekind = kind_first_word; ereg = dst; eloc = (R
           dst') } e1
       in
       if reg_loc_unconstrained dst (R dst') e2
       then if can_undef (destroyed_by_load Mint32 addr) e2
            then (match add_equations args args' e2 with
                  | Some e3 -> track_moves env mv1 e3
                  | None -> None)
            else None
       else None
     | None -> None)
  | BSload2_2 (_, addr', args, dst, mv1, args', dst', mv2, _) ->
    (match track_moves env mv2 e with
     | Some e1 ->
       let e2 =
         remove_equation { ekind = kind_second_word; ereg = dst; eloc = (R
           dst') } e1
       in
       if reg_loc_unconstrained dst (R dst') e2
       then if can_undef (destroyed_by_load Mint32 addr') e2
            then (match add_equations args args' e2 with
                  | Some e3 -> track_moves env mv1 e3
                  | None -> None)
            else None
       else None
     | None -> None)
  | BSstore (chunk, addr, args, src, mv, args', src', _) ->
    if can_undef (destroyed_by_store chunk addr) e
    then (match add_equations (src :: args) (src' :: args') e with
          | Some e1 -> track_moves env mv e1
          | None -> None)
    else None
  | BSstore2 (addr, addr', args, src, mv1, args1', src1', mv2, args2', src2',
              _) ->
    if can_undef (destroyed_by_store Mint32 addr') e
    then (match add_equations args args2'
                  (add_equation { ekind = kind_second_word; ereg = src;
                    eloc = (R src2') } e) with
          | Some e1 ->
            (match track_moves env mv2 e1 with
             | Some e2 ->
               if can_undef (destroyed_by_store Mint32 addr) e2
               then (match add_equations args args1'
                             (add_equation { ekind = kind_first_word; ereg =
                               src; eloc = (R src1') } e2) with
                     | Some e3 -> track_moves env mv1 e3
                     | None -> None)
               else None
             | None -> None)
          | None -> None)
    else None
  | BScall (sg, ros, args, res0, mv1, ros', mv2, _) ->
    let args' = loc_arguments sg in
    let res' = loc_result sg in
    (match track_moves env mv2 e with
     | Some e1 ->
       (match remove_equations_res res0 res' e1 with
        | Some e2 ->
          if forallb (fun l -> reg_loc_unconstrained res0 l e2)
               (map (fun x -> R x) (regs_of_rpair res'))
          then if no_caller_saves e2
               then (match add_equation_ros ros ros' e2 with
                     | Some e3 ->
                       (match add_equations_args args sg.sig_args args' e3 with
                        | Some e4 -> track_moves env mv1 e4
                        | None -> None)
                     | None -> None)
               else None
          else None
        | None -> None)
     | None -> None)
  | BStailcall (sg, ros, args, mv1, ros') ->
    let args' = loc_arguments sg in
    if tailcall_is_possible sg
    then if rettype_eq sg.sig_res f.fn_sig.sig_res
         then if ros_compatible_tailcall ros'
              then (match add_equation_ros ros ros' empty_eqs with
                    | Some e1 ->
                      (match add_equations_args args sg.sig_args args' e1 with
                       | Some e2 -> track_moves env mv1 e2
                       | None -> None)
                    | None -> None)
              else None
         else None
    else None
  | BSbuiltin (ef, args, res0, mv1, args', res', mv2, _) ->
    (match track_moves env mv2 e with
     | Some e1 ->
       (match remove_equations_builtin_res env res0 res' e1 with
        | Some e2 ->
          if forallb (fun r -> reg_unconstrained r e2)
               (params_of_builtin_res res0)
          then if forallb (fun mr -> loc_unconstrained (R mr) e2)
                    (params_of_builtin_res res')
               then if can_undef (destroyed_by_builtin ef) e2
                    then (match match ef with
                                | EF_debug (_, _, _) ->
                                  add_equations_debug_args env args args' e2
                                | _ ->
                                  add_equations_builtin_args env args args' e2 with
                          | Some e3 -> track_moves env mv1 e3
                          | None -> None)
                    else None
               else None
          else None
        | None -> None)
     | None -> None)
  | BScond (cond, args, mv, args', _, _) ->
    if can_undef (destroyed_by_cond cond) e
    then (match add_equations args args' e with
          | Some e1 -> track_moves env mv e1
          | None -> None)
    else None
  | BSjumptable (arg, mv, arg', _) ->
    if can_undef destroyed_by_jumptable e
    then track_moves env mv
           (add_equation { ekind = Full; ereg = arg; eloc = (R arg') } e)
    else None
  | BSreturn (arg0, mv) ->
    (match arg0 with
     | Some arg ->
       let arg' = loc_result f.fn_sig in
       (match add_equations_res arg (proj_sig_res f.fn_sig) arg' empty_eqs with
        | Some e1 -> track_moves env mv e1
        | None -> None)
     | None -> track_moves env mv empty_eqs)

(** val transfer :
    coq_function -> regenv -> block_shape PTree.t -> LTL.node -> eqs res ->
    eqs res **)

let transfer f env shapes pc after = match after with
| OK e ->
  (match PTree.get pc shapes with
   | Some shape ->
     (match transfer_aux f env shape e with
      | Some e' -> OK e'
      | None ->
        Error ((MSG ('A'::('t'::(' '::('P'::('C'::(' '::[]))))))) :: ((POS
          pc) :: ((MSG
          (':'::(' '::('i'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::(' '::('a'::('l'::('l'::('o'::('c'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))) :: []))))
   | None ->
     Error ((MSG ('A'::('t'::(' '::('P'::('C'::(' '::[]))))))) :: ((POS
       pc) :: ((MSG
       (':'::(' '::('u'::('n'::('m'::('a'::('t'::('c'::('h'::('e'::('d'::(' '::('b'::('l'::('o'::('c'::('k'::[])))))))))))))))))) :: []))))
| Error _ -> after

module LEq =
 struct
  type t = eqs res

  (** val beq : t -> t -> bool **)

  let beq x y =
    match x with
    | OK a ->
      (match y with
       | OK b -> EqSet.equal a.eqs1 b.eqs1
       | Error _ -> false)
    | Error _ -> (match y with
                  | OK _ -> false
                  | Error _ -> true)

  (** val bot : t **)

  let bot =
    OK empty_eqs

  (** val lub : t -> t -> t **)

  let lub x y =
    match x with
    | OK a ->
      (match y with
       | OK b ->
         OK { eqs1 = (EqSet.union a.eqs1 b.eqs1); eqs2 =
           (EqSet2.union a.eqs2 b.eqs2) }
       | Error _ -> y)
    | Error _ -> x
 end

module DS = Backward_Dataflow_Solver(LEq)(NodeSetBackward)

(** val successors_block_shape : block_shape -> LTL.node list **)

let successors_block_shape = function
| BSnop (_, s) -> s :: []
| BSmove (_, _, _, s) -> s :: []
| BSmakelong (_, _, _, _, s) -> s :: []
| BSlowlong (_, _, _, s) -> s :: []
| BShighlong (_, _, _, s) -> s :: []
| BSop (_, _, _, _, _, _, _, s) -> s :: []
| BSopdead (_, _, _, _, s) -> s :: []
| BSload (_, _, _, _, _, _, _, _, s) -> s :: []
| BSloaddead (_, _, _, _, _, s) -> s :: []
| BSload2 (_, _, _, _, _, _, _, _, _, _, _, s) -> s :: []
| BSload2_1 (_, _, _, _, _, _, _, s) -> s :: []
| BSload2_2 (_, _, _, _, _, _, _, _, s) -> s :: []
| BSstore (_, _, _, _, _, _, _, s) -> s :: []
| BSstore2 (_, _, _, _, _, _, _, _, _, _, s) -> s :: []
| BScall (_, _, _, _, _, _, _, s) -> s :: []
| BSbuiltin (_, _, _, _, _, _, _, s) -> s :: []
| BScond (_, _, _, _, s1, s2) -> s1 :: (s2 :: [])
| BSjumptable (_, _, _, tbl) -> tbl
| _ -> []

(** val analyze :
    coq_function -> regenv -> block_shape PTree.t -> DS.L.t PMap.t option **)

let analyze f env bsh =
  DS.fixpoint_allnodes bsh successors_block_shape (transfer f env bsh)

(** val compat_entry : reg list -> loc rpair list -> eqs -> bool **)

let rec compat_entry rparams lparams e =
  match rparams with
  | [] -> (match lparams with
           | [] -> true
           | _ :: _ -> false)
  | r1 :: rl ->
    (match lparams with
     | [] -> false
     | r :: ll ->
       (match r with
        | One l1 -> (&&) (compat_left r1 l1 e) (compat_entry rl ll e)
        | Twolong (l1, l2) ->
          (&&) (compat_left2 r1 l1 l2 e) (compat_entry rl ll e)))

(** val check_entrypoints_aux :
    coq_function -> LTL.coq_function -> regenv -> eqs -> unit option **)

let check_entrypoints_aux rtl ltl env e1 =
  match pair_entrypoints rtl ltl with
  | Some mv ->
    (match track_moves env mv e1 with
     | Some e2 ->
       if compat_entry rtl.fn_params (loc_parameters rtl.fn_sig) e2
       then if can_undef destroyed_at_function_entry e2
            then if zeq rtl.fn_stacksize ltl.LTL.fn_stacksize
                 then if signature_eq rtl.fn_sig ltl.LTL.fn_sig
                      then Some ()
                      else None
                 else None
            else None
       else None
     | None -> None)
  | None -> None

(** val check_entrypoints :
    coq_function -> LTL.coq_function -> regenv -> block_shape PTree.t ->
    LEq.t PMap.t -> unit res **)

let check_entrypoints rtl ltl env bsh a =
  match transfer rtl env bsh rtl.fn_entrypoint (PMap.get rtl.fn_entrypoint a) with
  | OK x ->
    (match check_entrypoints_aux rtl ltl env x with
     | Some _ -> OK ()
     | None ->
       Error
         (msg
           ('i'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::(' '::('a'::('l'::('l'::('o'::('c'::('a'::('t'::('i'::('o'::('n'::(' '::('a'::('t'::(' '::('e'::('n'::('t'::('r'::('y'::(' '::('p'::('o'::('i'::('n'::('t'::[]))))))))))))))))))))))))))))))))))))))))))))
  | Error msg0 -> Error msg0

(** val check_function :
    coq_function -> LTL.coq_function -> regenv -> unit res **)

let check_function rtl ltl env =
  let bsh = pair_codes rtl ltl in
  (match analyze rtl env bsh with
   | Some a -> check_entrypoints rtl ltl env bsh a
   | None ->
     Error
       (msg
         ('a'::('l'::('l'::('o'::('c'::('a'::('t'::('i'::('o'::('n'::(' '::('a'::('n'::('a'::('l'::('y'::('s'::('i'::('s'::(' '::('d'::('i'::('v'::('e'::('r'::('g'::('e'::('s'::[]))))))))))))))))))))))))))))))

(** val regalloc : coq_function -> LTL.coq_function res **)

let regalloc = Regalloc.regalloc

(** val transf_function : coq_function -> LTL.coq_function res **)

let transf_function f =
  match type_function f with
  | OK env ->
    (match regalloc f with
     | OK tf ->
       (match check_function f tf env with
        | OK _ -> OK tf
        | Error msg0 -> Error msg0)
     | Error m -> Error m)
  | Error m -> Error m

(** val transf_fundef : fundef -> LTL.fundef res **)

let transf_fundef fd =
  transf_partial_fundef transf_function fd

(** val transf_program : program -> LTL.program res **)

let transf_program p =
  transform_partial_program transf_fundef p
