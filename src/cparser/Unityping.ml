open BinNums
open Coqlib
open Datatypes
open Errors
open Maps

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module type TYPE_ALGEBRA =
 sig
  type t

  val eq : t -> t -> bool

  val default : t
 end

module UniSolver =
 functor (T:TYPE_ALGEBRA) ->
 struct
  type coq_constraint = positive * positive

  type typenv = { te_typ : T.t PTree.t; te_equ : coq_constraint list }

  (** val te_typ : typenv -> T.t PTree.t **)

  let te_typ t0 =
    t0.te_typ

  (** val te_equ : typenv -> coq_constraint list **)

  let te_equ t0 =
    t0.te_equ

  (** val initial : typenv **)

  let initial =
    { te_typ = PTree.empty; te_equ = [] }

  (** val set : typenv -> positive -> T.t -> typenv res **)

  let set e x ty =
    match PTree.get x (te_typ e) with
    | Some ty' ->
      if T.eq ty ty'
      then OK e
      else Error ((MSG
             ('b'::('a'::('d'::(' '::('d'::('e'::('f'::('i'::('n'::('i'::('t'::('i'::('o'::('n'::('/'::('u'::('s'::('e'::(' '::('o'::('f'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(' '::[])))))))))))))))))))))))))))))))) :: ((POS
             x) :: []))
    | None -> OK { te_typ = (PTree.set x ty (te_typ e)); te_equ = (te_equ e) }

  (** val set_list : typenv -> positive list -> T.t list -> typenv res **)

  let rec set_list e rl tyl =
    match rl with
    | [] ->
      (match tyl with
       | [] -> OK e
       | _ :: _ ->
         Error
           (msg
             ('a'::('r'::('i'::('t'::('y'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))
    | r1 :: rs ->
      (match tyl with
       | [] ->
         Error
           (msg
             ('a'::('r'::('i'::('t'::('y'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))
       | ty1 :: tys ->
         (match set e r1 ty1 with
          | OK x -> set_list x rs tys
          | Error msg0 -> Error msg0))

  (** val move : typenv -> positive -> positive -> (bool * typenv) res **)

  let move e r1 r2 =
    if peq r1 r2
    then OK (false, e)
    else (match PTree.get r1 (te_typ e) with
          | Some ty1 ->
            (match PTree.get r2 (te_typ e) with
             | Some ty2 ->
               if T.eq ty1 ty2
               then OK (false, e)
               else Error ((MSG
                      ('i'::('l'::('l'::('-'::('t'::('y'::('p'::('e'::('d'::(' '::('m'::('o'::('v'::('e'::(' '::('f'::('r'::('o'::('m'::(' '::[]))))))))))))))))))))) :: ((POS
                      r1) :: ((MSG (' '::('t'::('o'::(' '::[]))))) :: ((POS
                      r2) :: []))))
             | None ->
               OK (true, { te_typ = (PTree.set r2 ty1 (te_typ e)); te_equ =
                 (te_equ e) }))
          | None ->
            (match PTree.get r2 (te_typ e) with
             | Some ty2 ->
               OK (true, { te_typ = (PTree.set r1 ty2 (te_typ e)); te_equ =
                 (te_equ e) })
             | None ->
               OK (false, { te_typ = (te_typ e); te_equ = ((r1,
                 r2) :: (te_equ e)) })))

  (** val solve_rec :
      typenv -> bool -> coq_constraint list -> (typenv * bool) res **)

  let rec solve_rec e changed = function
  | [] -> OK (e, changed)
  | c :: q' ->
    let (r1, r2) = c in
    (match move e r1 r2 with
     | OK p -> let (x, y) = p in solve_rec y ((||) changed x) q'
     | Error msg0 -> Error msg0)

  (** val weight_typenv : typenv -> nat **)

  let weight_typenv e =
    length (te_equ e)

  (** val solve_constraints_F :
      (typenv -> typenv res) -> typenv -> typenv res **)

  let solve_constraints_F solve_constraints0 e =
    match solve_rec { te_typ = (te_typ e); te_equ = [] } false (te_equ e) with
    | OK p -> let (e', b) = p in if b then solve_constraints0 e' else OK e
    | Error msg0 -> Error msg0

  (** val solve_constraints_terminate : typenv -> typenv res **)

  let rec solve_constraints_terminate e =
    match solve_rec { te_typ = (te_typ e); te_equ = [] } false (te_equ e) with
    | OK p ->
      let (e', b) = p in if b then solve_constraints_terminate e' else OK e
    | Error msg0 -> Error msg0

  (** val solve_constraints : typenv -> typenv res **)

  let solve_constraints =
    solve_constraints_terminate

  type coq_R_solve_constraints =
  | R_solve_constraints_0 of typenv * typenv
  | R_solve_constraints_1 of typenv * typenv * typenv res
     * coq_R_solve_constraints
  | R_solve_constraints_2 of typenv * errmsg

  (** val coq_R_solve_constraints_rect :
      (typenv -> typenv -> __ -> 'a1) -> (typenv -> typenv -> __ -> typenv
      res -> coq_R_solve_constraints -> 'a1 -> 'a1) -> (typenv -> errmsg ->
      __ -> 'a1) -> typenv -> typenv res -> coq_R_solve_constraints -> 'a1 **)

  let rec coq_R_solve_constraints_rect f f0 f1 _ _ = function
  | R_solve_constraints_0 (e, e') -> f e e' __
  | R_solve_constraints_1 (e, e', _res, r0) ->
    f0 e e' __ _res r0 (coq_R_solve_constraints_rect f f0 f1 e' _res r0)
  | R_solve_constraints_2 (e, msg0) -> f1 e msg0 __

  (** val coq_R_solve_constraints_rec :
      (typenv -> typenv -> __ -> 'a1) -> (typenv -> typenv -> __ -> typenv
      res -> coq_R_solve_constraints -> 'a1 -> 'a1) -> (typenv -> errmsg ->
      __ -> 'a1) -> typenv -> typenv res -> coq_R_solve_constraints -> 'a1 **)

  let rec coq_R_solve_constraints_rec f f0 f1 _ _ = function
  | R_solve_constraints_0 (e, e') -> f e e' __
  | R_solve_constraints_1 (e, e', _res, r0) ->
    f0 e e' __ _res r0 (coq_R_solve_constraints_rec f f0 f1 e' _res r0)
  | R_solve_constraints_2 (e, msg0) -> f1 e msg0 __

  (** val solve_constraints_rect :
      (typenv -> typenv -> __ -> 'a1) -> (typenv -> typenv -> __ -> 'a1 ->
      'a1) -> (typenv -> errmsg -> __ -> 'a1) -> typenv -> 'a1 **)

  let rec solve_constraints_rect f f0 f1 e =
    let f2 = f1 e in
    let f3 = f0 e in
    let f4 = f e in
    (match solve_rec { te_typ = (te_typ e); te_equ = [] } false (te_equ e) with
     | OK p ->
       let (t0, b) = p in
       if b
       then let f5 = f3 t0 __ in
            let hrec = solve_constraints_rect f f0 f1 t0 in f5 hrec
       else f4 t0 __
     | Error e0 -> f2 e0 __)

  (** val solve_constraints_rec :
      (typenv -> typenv -> __ -> 'a1) -> (typenv -> typenv -> __ -> 'a1 ->
      'a1) -> (typenv -> errmsg -> __ -> 'a1) -> typenv -> 'a1 **)

  let solve_constraints_rec =
    solve_constraints_rect

  (** val coq_R_solve_constraints_correct :
      typenv -> typenv res -> coq_R_solve_constraints **)

  let coq_R_solve_constraints_correct e _res =
    solve_constraints_rect (fun y y0 _ _ _ -> R_solve_constraints_0 (y, y0))
      (fun y y0 _ y2 _ _ -> R_solve_constraints_1 (y, y0,
      (solve_constraints y0), (y2 (solve_constraints y0) __)))
      (fun y y0 _ _ _ -> R_solve_constraints_2 (y, y0)) e _res __

  type typassign = positive -> T.t

  (** val makeassign : typenv -> typassign **)

  let makeassign e x =
    match PTree.get x (te_typ e) with
    | Some ty -> ty
    | None -> T.default

  (** val solve : typenv -> typassign res **)

  let solve e =
    match solve_constraints e with
    | OK x -> OK (makeassign x)
    | Error msg0 -> Error msg0
 end
