open AST
open BinInt
open BinNums
open BinPos
open Clight
open Cop
open Csyntax
open Ctypes
open Datatypes
open Errors
open Integers
open Maps
open Memory
open Values
open Zpower

type generator = { gen_next : ident; gen_trail : (ident * coq_type) list }

type 'a result =
| Err of errmsg
| Res of 'a * generator

type 'a mon = generator -> 'a result

(** val first_unused_ident : unit -> ident **)

let first_unused_ident = Camlcoq.first_unused_ident

(** val initial_generator : unit -> generator **)

let initial_generator x =
  { gen_next = (first_unused_ident x); gen_trail = [] }

(** val gensym : coq_type -> ident mon **)

let gensym ty g =
  Res (g.gen_next, { gen_next = (Pos.succ g.gen_next); gen_trail =
    ((g.gen_next, ty) :: g.gen_trail) })

(** val makeseq_rec :
    Clight.statement -> Clight.statement list -> Clight.statement **)

let rec makeseq_rec s = function
| [] -> s
| s' :: l' -> makeseq_rec (Clight.Ssequence (s, s')) l'

(** val makeseq : Clight.statement list -> Clight.statement **)

let makeseq l =
  makeseq_rec Clight.Sskip l

(** val eval_simpl_expr : Clight.expr -> coq_val option **)

let rec eval_simpl_expr = function
| Econst_int (n, _) -> Some (Vint n)
| Econst_float (n, _) -> Some (Vfloat n)
| Econst_single (n, _) -> Some (Vsingle n)
| Econst_long (n, _) -> Some (Vlong n)
| Clight.Ecast (b, ty) ->
  (match eval_simpl_expr b with
   | Some v -> sem_cast v (Clight.typeof b) ty Mem.empty
   | None -> None)
| _ -> None

(** val makeif :
    Clight.expr -> Clight.statement -> Clight.statement -> Clight.statement **)

let makeif a s1 s2 =
  match eval_simpl_expr a with
  | Some v ->
    (match bool_val v (Clight.typeof a) Mem.empty with
     | Some b -> if b then s1 else s2
     | None -> Clight.Sifthenelse (a, s1, s2))
  | None -> Clight.Sifthenelse (a, s1, s2)

(** val coq_Ederef' : Clight.expr -> coq_type -> Clight.expr **)

let coq_Ederef' a t =
  match a with
  | Clight.Eaddrof (a', _) ->
    if type_eq t (Clight.typeof a') then a' else Clight.Ederef (a, t)
  | _ -> Clight.Ederef (a, t)

(** val coq_Eaddrof' : Clight.expr -> coq_type -> Clight.expr **)

let coq_Eaddrof' a t =
  match a with
  | Clight.Ederef (a', _) ->
    if type_eq t (Clight.typeof a') then a' else Clight.Eaddrof (a, t)
  | _ -> Clight.Eaddrof (a, t)

(** val transl_incrdecr :
    incr_or_decr -> Clight.expr -> coq_type -> Clight.expr **)

let transl_incrdecr id a ty =
  match id with
  | Incr ->
    Clight.Ebinop (Oadd, a, (Econst_int (Int.one, type_int32s)),
      (incrdecr_type ty))
  | Decr ->
    Clight.Ebinop (Osub, a, (Econst_int (Int.one, type_int32s)),
      (incrdecr_type ty))

(** val is_bitfield_access_aux :
    composite_env -> (composite_env -> ident -> members -> (coq_Z * bitfield)
    res) -> ident -> ident -> bitfield mon **)

let is_bitfield_access_aux ce fn id fld g =
  match PTree.get id ce with
  | Some co ->
    (match fn ce fld co.co_members with
     | OK p -> let (_, bf) = p in Res (bf, g)
     | Error _ ->
       Err ((MSG
         ('u'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('f'::('i'::('e'::('l'::('d'::(' '::[]))))))))))))))) :: ((CTX
         fld) :: [])))
  | None ->
    Err ((MSG
      ('u'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('c'::('o'::('m'::('p'::('o'::('s'::('i'::('t'::('e'::(' '::[]))))))))))))))))))) :: ((CTX
      id) :: []))

(** val is_bitfield_access : composite_env -> Clight.expr -> bitfield mon **)

let is_bitfield_access ce = function
| Clight.Efield (r, f, _) ->
  (match Clight.typeof r with
   | Tstruct (id, _) -> is_bitfield_access_aux ce field_offset id f
   | Tunion (id, _) -> is_bitfield_access_aux ce union_field_offset id f
   | _ ->
     (fun _ -> Err
       (msg
         ('i'::('s'::('_'::('b'::('i'::('t'::('f'::('i'::('e'::('l'::('d'::('_'::('a'::('c'::('c'::('e'::('s'::('s'::[])))))))))))))))))))))
| _ -> (fun g -> Res (Full, g))

(** val chunk_for_volatile_type :
    coq_type -> bitfield -> memory_chunk option **)

let chunk_for_volatile_type ty bf =
  if type_is_volatile ty
  then (match access_mode ty with
        | By_value chunk ->
          (match bf with
           | Full -> Some chunk
           | Bits (_, _, _, _) -> None)
        | _ -> None)
  else None

(** val make_set : bitfield -> ident -> Clight.expr -> Clight.statement **)

let make_set bf id l =
  match chunk_for_volatile_type (Clight.typeof l) bf with
  | Some chunk ->
    let typtr = Tpointer ((Clight.typeof l), noattr) in
    Sbuiltin ((Some id), (EF_vload chunk), (Tcons (typtr, Tnil)),
    ((Clight.Eaddrof (l, typtr)) :: []))
  | None -> Sset (id, l)

(** val transl_valof :
    composite_env -> coq_type -> Clight.expr -> (Clight.statement
    list * Clight.expr) mon **)

let transl_valof ce ty l g =
  if type_is_volatile ty
  then (match gensym ty g with
        | Err msg0 -> Err msg0
        | Res (a, g') ->
          (match is_bitfield_access ce l g' with
           | Err msg0 -> Err msg0
           | Res (a0, g'0) ->
             Res ((((make_set a0 a l) :: []), (Etempvar (a, ty))), g'0)))
  else Res (([], l), g)

(** val make_assign :
    bitfield -> Clight.expr -> Clight.expr -> Clight.statement **)

let make_assign bf l r =
  match chunk_for_volatile_type (Clight.typeof l) bf with
  | Some chunk ->
    let ty = Clight.typeof l in
    let typtr = Tpointer (ty, noattr) in
    Sbuiltin (None, (EF_vstore chunk), (Tcons (typtr, (Tcons (ty, Tnil)))),
    ((Clight.Eaddrof (l, typtr)) :: (r :: [])))
  | None -> Sassign (l, r)

(** val make_normalize :
    intsize -> signedness -> coq_Z -> Clight.expr -> Clight.expr **)

let make_normalize sz sg width r =
  let intconst = fun n -> Econst_int ((Int.repr n), type_int32s) in
  if (||) ((fun x -> x) (intsize_eq sz IBool))
       ((fun x -> x) (signedness_eq sg Unsigned))
  then let mask = Z.sub (two_p width) (Zpos Coq_xH) in
       Clight.Ebinop (Oand, r, (intconst mask), (Clight.typeof r))
  else let amount = Z.sub Int.zwordsize width in
       Clight.Ebinop (Oshr, (Clight.Ebinop (Oshl, r, (intconst amount),
       type_int32s)), (intconst amount), (Clight.typeof r))

(** val make_assign_value : bitfield -> Clight.expr -> Clight.expr **)

let make_assign_value bf r =
  match bf with
  | Full -> r
  | Bits (sz, sg, _, width) -> make_normalize sz sg width r

type set_destination =
| SDbase of coq_type * coq_type * ident
| SDcons of coq_type * coq_type * ident * set_destination

type destination =
| For_val
| For_effects
| For_set of set_destination

(** val dummy_expr : Clight.expr **)

let dummy_expr =
  Econst_int (Int.zero, type_int32s)

(** val do_set : set_destination -> Clight.expr -> Clight.statement list **)

let rec do_set sd a =
  match sd with
  | SDbase (tycast, _, tmp) -> (Sset (tmp, (Clight.Ecast (a, tycast)))) :: []
  | SDcons (tycast, ty, tmp, sd') ->
    (Sset (tmp, (Clight.Ecast (a,
      tycast)))) :: (do_set sd' (Etempvar (tmp, ty)))

(** val finish :
    destination -> Clight.statement list -> Clight.expr -> Clight.statement
    list * Clight.expr **)

let finish dst sl a =
  match dst with
  | For_set sd -> ((app sl (do_set sd a)), a)
  | _ -> (sl, a)

(** val sd_temp : set_destination -> ident **)

let sd_temp = function
| SDbase (_, _, tmp) -> tmp
| SDcons (_, _, tmp, _) -> tmp

(** val sd_seqbool_val : ident -> coq_type -> set_destination **)

let sd_seqbool_val tmp ty =
  SDbase (type_bool, ty, tmp)

(** val sd_seqbool_set : coq_type -> set_destination -> set_destination **)

let sd_seqbool_set ty sd =
  let tmp = sd_temp sd in SDcons (type_bool, ty, tmp, sd)

(** val transl_expr :
    composite_env -> destination -> expr -> (Clight.statement
    list * Clight.expr) mon **)

let transl_expr ce =
  let rec transl_expr0 dst = function
  | Eval (v, ty) ->
    (fun g ->
      match v with
      | Vint n -> Res ((finish dst [] (Econst_int (n, ty))), g)
      | Vlong n -> Res ((finish dst [] (Econst_long (n, ty))), g)
      | Vfloat n -> Res ((finish dst [] (Econst_float (n, ty))), g)
      | Vsingle n -> Res ((finish dst [] (Econst_single (n, ty))), g)
      | _ ->
        Err
          (msg
            ('S'::('i'::('m'::('p'::('l'::('E'::('x'::('p'::('r'::('.'::('t'::('r'::('a'::('n'::('s'::('l'::('_'::('e'::('x'::('p'::('r'::(':'::(' '::('E'::('v'::('a'::('l'::[])))))))))))))))))))))))))))))
  | Evar (x, ty) -> (fun g -> Res ((finish dst [] (Clight.Evar (x, ty))), g))
  | Efield (r, f, ty) ->
    (fun g ->
      match transl_expr0 For_val r g with
      | Err msg0 -> Err msg0
      | Res (a0, g') ->
        Res ((finish dst (fst a0) (Clight.Efield ((snd a0), f, ty))), g'))
  | Evalof (l, _) ->
    (fun g ->
      match transl_expr0 For_val l g with
      | Err msg0 -> Err msg0
      | Res (a0, g') ->
        (match transl_valof ce (typeof l) (snd a0) g' with
         | Err msg0 -> Err msg0
         | Res (a1, g'0) ->
           Res ((finish dst (app (fst a0) (fst a1)) (snd a1)), g'0)))
  | Ederef (r, ty) ->
    (fun g ->
      match transl_expr0 For_val r g with
      | Err msg0 -> Err msg0
      | Res (a0, g') ->
        Res ((finish dst (fst a0) (coq_Ederef' (snd a0) ty)), g'))
  | Eaddrof (l, ty) ->
    (fun g ->
      match transl_expr0 For_val l g with
      | Err msg0 -> Err msg0
      | Res (a0, g') ->
        Res ((finish dst (fst a0) (coq_Eaddrof' (snd a0) ty)), g'))
  | Eunop (op, r1, ty) ->
    (fun g ->
      match transl_expr0 For_val r1 g with
      | Err msg0 -> Err msg0
      | Res (a0, g') ->
        Res ((finish dst (fst a0) (Clight.Eunop (op, (snd a0), ty))), g'))
  | Ebinop (op, r1, r2, ty) ->
    (fun g ->
      match transl_expr0 For_val r1 g with
      | Err msg0 -> Err msg0
      | Res (a0, g') ->
        (match transl_expr0 For_val r2 g' with
         | Err msg0 -> Err msg0
         | Res (a1, g'0) ->
           Res
             ((finish dst (app (fst a0) (fst a1)) (Clight.Ebinop (op,
                (snd a0), (snd a1), ty))), g'0)))
  | Ecast (r1, ty) ->
    (match dst with
     | For_effects -> transl_expr0 For_effects r1
     | _ ->
       (fun g ->
         match transl_expr0 For_val r1 g with
         | Err msg0 -> Err msg0
         | Res (a0, g') ->
           Res ((finish dst (fst a0) (Clight.Ecast ((snd a0), ty))), g')))
  | Eseqand (r1, r2, ty) ->
    (fun g ->
      match transl_expr0 For_val r1 g with
      | Err msg0 -> Err msg0
      | Res (a0, g') ->
        (match dst with
         | For_val ->
           (match gensym ty g' with
            | Err msg0 -> Err msg0
            | Res (a1, g'0) ->
              (match transl_expr0 (For_set (sd_seqbool_val a1 ty)) r2 g'0 with
               | Err msg0 -> Err msg0
               | Res (a2, g'1) ->
                 Res
                   (((app (fst a0)
                       ((makeif (snd a0) (makeseq (fst a2)) (Sset (a1,
                          (Econst_int (Int.zero, ty))))) :: [])), (Etempvar
                   (a1, ty))), g'1)))
         | For_effects ->
           (match transl_expr0 For_effects r2 g' with
            | Err msg0 -> Err msg0
            | Res (a1, g'0) ->
              Res
                (((app (fst a0)
                    ((makeif (snd a0) (makeseq (fst a1)) Clight.Sskip) :: [])),
                dummy_expr), g'0))
         | For_set sd ->
           (match transl_expr0 (For_set (sd_seqbool_set ty sd)) r2 g' with
            | Err msg0 -> Err msg0
            | Res (a1, g'0) ->
              Res
                (((app (fst a0)
                    ((makeif (snd a0) (makeseq (fst a1))
                       (makeseq (do_set sd (Econst_int (Int.zero, ty))))) :: [])),
                dummy_expr), g'0))))
  | Eseqor (r1, r2, ty) ->
    (fun g ->
      match transl_expr0 For_val r1 g with
      | Err msg0 -> Err msg0
      | Res (a0, g') ->
        (match dst with
         | For_val ->
           (match gensym ty g' with
            | Err msg0 -> Err msg0
            | Res (a1, g'0) ->
              (match transl_expr0 (For_set (sd_seqbool_val a1 ty)) r2 g'0 with
               | Err msg0 -> Err msg0
               | Res (a2, g'1) ->
                 Res
                   (((app (fst a0)
                       ((makeif (snd a0) (Sset (a1, (Econst_int (Int.one,
                          ty)))) (makeseq (fst a2))) :: [])), (Etempvar (a1,
                   ty))), g'1)))
         | For_effects ->
           (match transl_expr0 For_effects r2 g' with
            | Err msg0 -> Err msg0
            | Res (a1, g'0) ->
              Res
                (((app (fst a0)
                    ((makeif (snd a0) Clight.Sskip (makeseq (fst a1))) :: [])),
                dummy_expr), g'0))
         | For_set sd ->
           (match transl_expr0 (For_set (sd_seqbool_set ty sd)) r2 g' with
            | Err msg0 -> Err msg0
            | Res (a1, g'0) ->
              Res
                (((app (fst a0)
                    ((makeif (snd a0)
                       (makeseq (do_set sd (Econst_int (Int.one, ty))))
                       (makeseq (fst a1))) :: [])), dummy_expr), g'0))))
  | Econdition (r1, r2, r3, ty) ->
    (fun g ->
      match transl_expr0 For_val r1 g with
      | Err msg0 -> Err msg0
      | Res (a0, g') ->
        (match dst with
         | For_val ->
           (match gensym ty g' with
            | Err msg0 -> Err msg0
            | Res (a1, g'0) ->
              (match transl_expr0 (For_set (SDbase (ty, ty, a1))) r2 g'0 with
               | Err msg0 -> Err msg0
               | Res (a2, g'1) ->
                 (match transl_expr0 (For_set (SDbase (ty, ty, a1))) r3 g'1 with
                  | Err msg0 -> Err msg0
                  | Res (a3, g'2) ->
                    Res
                      (((app (fst a0)
                          ((makeif (snd a0) (makeseq (fst a2))
                             (makeseq (fst a3))) :: [])), (Etempvar (a1,
                      ty))), g'2))))
         | For_effects ->
           (match transl_expr0 For_effects r2 g' with
            | Err msg0 -> Err msg0
            | Res (a1, g'0) ->
              (match transl_expr0 For_effects r3 g'0 with
               | Err msg0 -> Err msg0
               | Res (a2, g'1) ->
                 Res
                   (((app (fst a0)
                       ((makeif (snd a0) (makeseq (fst a1))
                          (makeseq (fst a2))) :: [])), dummy_expr), g'1)))
         | For_set sd ->
           (match gensym ty g' with
            | Err msg0 -> Err msg0
            | Res (a1, g'0) ->
              (match transl_expr0 (For_set (SDcons (ty, ty, a1, sd))) r2 g'0 with
               | Err msg0 -> Err msg0
               | Res (a2, g'1) ->
                 (match transl_expr0 (For_set (SDcons (ty, ty, a1, sd))) r3
                          g'1 with
                  | Err msg0 -> Err msg0
                  | Res (a3, g'2) ->
                    Res
                      (((app (fst a0)
                          ((makeif (snd a0) (makeseq (fst a2))
                             (makeseq (fst a3))) :: [])), dummy_expr), g'2))))))
  | Esizeof (ty', ty) ->
    (fun g -> Res ((finish dst [] (Clight.Esizeof (ty', ty))), g))
  | Ealignof (ty', ty) ->
    (fun g -> Res ((finish dst [] (Clight.Ealignof (ty', ty))), g))
  | Eassign (l1, r2, _) ->
    (fun g ->
      match transl_expr0 For_val l1 g with
      | Err msg0 -> Err msg0
      | Res (a0, g') ->
        let a1 = snd a0 in
        (match transl_expr0 For_val r2 g' with
         | Err msg0 -> Err msg0
         | Res (a2, g'0) ->
           (match is_bitfield_access ce a1 g'0 with
            | Err msg0 -> Err msg0
            | Res (a3, g'1) ->
              let ty1 = typeof l1 in
              (match dst with
               | For_effects ->
                 Res
                   (((app (fst a0)
                       (app (fst a2) ((make_assign a3 a1 (snd a2)) :: []))),
                   dummy_expr), g'1)
               | _ ->
                 (match gensym ty1 g'1 with
                  | Err msg0 -> Err msg0
                  | Res (a4, g'2) ->
                    Res
                      ((finish dst
                         (app (fst a0)
                           (app (fst a2) ((Sset (a4, (Clight.Ecast ((snd a2),
                             ty1)))) :: ((make_assign a3 a1 (Etempvar (a4,
                                           ty1))) :: []))))
                         (make_assign_value a3 (Etempvar (a4, ty1)))), g'2))))))
  | Eassignop (op, l1, r2, tyres, _) ->
    let ty1 = typeof l1 in
    (fun g ->
    match transl_expr0 For_val l1 g with
    | Err msg0 -> Err msg0
    | Res (a0, g') ->
      let a1 = snd a0 in
      (match transl_expr0 For_val r2 g' with
       | Err msg0 -> Err msg0
       | Res (a2, g'0) ->
         (match transl_valof ce ty1 a1 g'0 with
          | Err msg0 -> Err msg0
          | Res (a3, g'1) ->
            (match is_bitfield_access ce a1 g'1 with
             | Err msg0 -> Err msg0
             | Res (a4, g'2) ->
               (match dst with
                | For_effects ->
                  Res
                    (((app (fst a0)
                        (app (fst a2)
                          (app (fst a3)
                            ((make_assign a4 a1 (Clight.Ebinop (op, (snd a3),
                               (snd a2), tyres))) :: [])))), dummy_expr), g'2)
                | _ ->
                  (match gensym ty1 g'2 with
                   | Err msg0 -> Err msg0
                   | Res (a5, g'3) ->
                     Res
                       ((finish dst
                          (app (fst a0)
                            (app (fst a2)
                              (app (fst a3) ((Sset (a5, (Clight.Ecast
                                ((Clight.Ebinop (op, (snd a3), (snd a2),
                                tyres)),
                                ty1)))) :: ((make_assign a4 a1 (Etempvar (a5,
                                              ty1))) :: [])))))
                          (make_assign_value a4 (Etempvar (a5, ty1)))), g'3)))))))
  | Epostincr (id, l1, _) ->
    let ty1 = typeof l1 in
    (fun g ->
    match transl_expr0 For_val l1 g with
    | Err msg0 -> Err msg0
    | Res (a0, g') ->
      let a1 = snd a0 in
      (match is_bitfield_access ce a1 g' with
       | Err msg0 -> Err msg0
       | Res (a2, g'0) ->
         (match dst with
          | For_effects ->
            (match transl_valof ce ty1 a1 g'0 with
             | Err msg0 -> Err msg0
             | Res (a3, g'1) ->
               Res
                 (((app (fst a0)
                     (app (fst a3)
                       ((make_assign a2 a1 (transl_incrdecr id (snd a3) ty1)) :: []))),
                 dummy_expr), g'1))
          | _ ->
            (match gensym ty1 g'0 with
             | Err msg0 -> Err msg0
             | Res (a3, g'1) ->
               Res
                 ((finish dst
                    (app (fst a0)
                      ((make_set a2 a3 a1) :: ((make_assign a2 a1
                                                 (transl_incrdecr id
                                                   (Etempvar (a3, ty1)) ty1)) :: [])))
                    (Etempvar (a3, ty1))), g'1)))))
  | Ecomma (r1, r2, _) ->
    (fun g ->
      match transl_expr0 For_effects r1 g with
      | Err msg0 -> Err msg0
      | Res (a0, g') ->
        (match transl_expr0 dst r2 g' with
         | Err msg0 -> Err msg0
         | Res (a1, g'0) -> Res (((app (fst a0) (fst a1)), (snd a1)), g'0)))
  | Ecall (r1, rl2, ty) ->
    (fun g ->
      match transl_expr0 For_val r1 g with
      | Err msg0 -> Err msg0
      | Res (a0, g') ->
        (match transl_exprlist rl2 g' with
         | Err msg0 -> Err msg0
         | Res (a1, g'0) ->
           (match dst with
            | For_effects ->
              Res
                (((app (fst a0)
                    (app (fst a1) ((Scall (None, (snd a0), (snd a1))) :: []))),
                dummy_expr), g'0)
            | _ ->
              (match gensym ty g'0 with
               | Err msg0 -> Err msg0
               | Res (a2, g'1) ->
                 Res
                   ((finish dst
                      (app (fst a0)
                        (app (fst a1) ((Scall ((Some a2), (snd a0),
                          (snd a1))) :: []))) (Etempvar (a2, ty))), g'1)))))
  | Ebuiltin (ef, tyargs, rl, ty) ->
    (fun g ->
      match transl_exprlist rl g with
      | Err msg0 -> Err msg0
      | Res (a0, g') ->
        (match dst with
         | For_effects ->
           Res
             (((app (fst a0) ((Sbuiltin (None, ef, tyargs, (snd a0))) :: [])),
             dummy_expr), g')
         | _ ->
           (match gensym ty g' with
            | Err msg0 -> Err msg0
            | Res (a1, g'0) ->
              Res
                ((finish dst
                   (app (fst a0) ((Sbuiltin ((Some a1), ef, tyargs,
                     (snd a0))) :: [])) (Etempvar (a1, ty))), g'0))))
  | Eloc (_, _, _, _) ->
    (fun _ -> Err
      (msg
        ('S'::('i'::('m'::('p'::('l'::('E'::('x'::('p'::('r'::('.'::('t'::('r'::('a'::('n'::('s'::('l'::('_'::('e'::('x'::('p'::('r'::(':'::(' '::('E'::('l'::('o'::('c'::[])))))))))))))))))))))))))))))
  | Eparen (_, _, _) ->
    (fun _ -> Err
      (msg
        ('S'::('i'::('m'::('p'::('l'::('E'::('x'::('p'::('r'::('.'::('t'::('r'::('a'::('n'::('s'::('l'::('_'::('e'::('x'::('p'::('r'::(':'::(' '::('p'::('a'::('r'::('e'::('n'::[]))))))))))))))))))))))))))))))
  and transl_exprlist rl g =
    match rl with
    | Enil -> Res (([], []), g)
    | Econs (r1, rl2) ->
      (match transl_expr0 For_val r1 g with
       | Err msg0 -> Err msg0
       | Res (a, g') ->
         (match transl_exprlist rl2 g' with
          | Err msg0 -> Err msg0
          | Res (a0, g'0) ->
            Res (((app (fst a) (fst a0)), ((snd a) :: (snd a0))), g'0)))
  in transl_expr0

(** val transl_expression :
    composite_env -> expr -> (Clight.statement * Clight.expr) mon **)

let transl_expression ce r g =
  match transl_expr ce For_val r g with
  | Err msg0 -> Err msg0
  | Res (a, g') -> Res (((makeseq (fst a)), (snd a)), g')

(** val transl_expr_stmt : composite_env -> expr -> Clight.statement mon **)

let transl_expr_stmt ce r g =
  match transl_expr ce For_effects r g with
  | Err msg0 -> Err msg0
  | Res (a, g') -> Res ((makeseq (fst a)), g')

(** val transl_if :
    composite_env -> expr -> Clight.statement -> Clight.statement ->
    Clight.statement mon **)

let transl_if ce r s1 s2 g =
  match transl_expr ce For_val r g with
  | Err msg0 -> Err msg0
  | Res (a, g') ->
    Res ((makeseq (app (fst a) ((makeif (snd a) s1 s2) :: []))), g')

(** val is_Sskip : statement -> bool **)

let is_Sskip = function
| Sskip -> true
| _ -> false

(** val transl_stmt : composite_env -> statement -> Clight.statement mon **)

let transl_stmt ce =
  let rec transl_stmt0 = function
  | Sskip -> (fun g -> Res (Clight.Sskip, g))
  | Sdo e -> transl_expr_stmt ce e
  | Ssequence (s1, s2) ->
    (fun g ->
      match transl_stmt0 s1 g with
      | Err msg0 -> Err msg0
      | Res (a, g') ->
        (match transl_stmt0 s2 g' with
         | Err msg0 -> Err msg0
         | Res (a0, g'0) -> Res ((Clight.Ssequence (a, a0)), g'0)))
  | Sifthenelse (e, s1, s2) ->
    (fun g ->
      match transl_stmt0 s1 g with
      | Err msg0 -> Err msg0
      | Res (a, g') ->
        (match transl_stmt0 s2 g' with
         | Err msg0 -> Err msg0
         | Res (a0, g'0) ->
           (match transl_expression ce e g'0 with
            | Err msg0 -> Err msg0
            | Res (a1, g'1) ->
              if (&&) ((fun x -> x) (is_Sskip s1))
                   ((fun x -> x) (is_Sskip s2))
              then Res ((Clight.Ssequence ((fst a1), Clight.Sskip)), g'1)
              else Res ((Clight.Ssequence ((fst a1), (Clight.Sifthenelse
                     ((snd a1), a, a0)))), g'1))))
  | Swhile (e, s1) ->
    (fun g ->
      match transl_if ce e Clight.Sskip Clight.Sbreak g with
      | Err msg0 -> Err msg0
      | Res (a, g') ->
        (match transl_stmt0 s1 g' with
         | Err msg0 -> Err msg0
         | Res (a0, g'0) ->
           Res ((Sloop ((Clight.Ssequence (a, a0)), Clight.Sskip)), g'0)))
  | Sdowhile (e, s1) ->
    (fun g ->
      match transl_if ce e Clight.Sskip Clight.Sbreak g with
      | Err msg0 -> Err msg0
      | Res (a, g') ->
        (match transl_stmt0 s1 g' with
         | Err msg0 -> Err msg0
         | Res (a0, g'0) -> Res ((Sloop (a0, a)), g'0)))
  | Sfor (s1, e2, s3, s4) ->
    (fun g ->
      match transl_stmt0 s1 g with
      | Err msg0 -> Err msg0
      | Res (a, g') ->
        (match transl_if ce e2 Clight.Sskip Clight.Sbreak g' with
         | Err msg0 -> Err msg0
         | Res (a0, g'0) ->
           (match transl_stmt0 s3 g'0 with
            | Err msg0 -> Err msg0
            | Res (a1, g'1) ->
              (match transl_stmt0 s4 g'1 with
               | Err msg0 -> Err msg0
               | Res (a2, g'2) ->
                 if is_Sskip s1
                 then Res ((Sloop ((Clight.Ssequence (a0, a2)), a1)), g'2)
                 else Res ((Clight.Ssequence (a, (Sloop ((Clight.Ssequence
                        (a0, a2)), a1)))), g'2)))))
  | Sbreak -> (fun g -> Res (Clight.Sbreak, g))
  | Scontinue -> (fun g -> Res (Clight.Scontinue, g))
  | Sreturn o ->
    (fun g ->
      match o with
      | Some e ->
        (match transl_expression ce e g with
         | Err msg0 -> Err msg0
         | Res (a, g') ->
           Res ((Clight.Ssequence ((fst a), (Clight.Sreturn (Some
             (snd a))))), g'))
      | None -> Res ((Clight.Sreturn None), g))
  | Sswitch (e, ls) ->
    (fun g ->
      match transl_expression ce e g with
      | Err msg0 -> Err msg0
      | Res (a, g') ->
        (match transl_lblstmt ls g' with
         | Err msg0 -> Err msg0
         | Res (a0, g'0) ->
           Res ((Clight.Ssequence ((fst a), (Clight.Sswitch ((snd a), a0)))),
             g'0)))
  | Slabel (lbl, s1) ->
    (fun g ->
      match transl_stmt0 s1 g with
      | Err msg0 -> Err msg0
      | Res (a, g') -> Res ((Clight.Slabel (lbl, a)), g'))
  | Sgoto lbl -> (fun g -> Res ((Clight.Sgoto lbl), g))
  and transl_lblstmt ls g =
    match ls with
    | LSnil -> Res (Clight.LSnil, g)
    | LScons (c, s, ls1) ->
      (match transl_stmt0 s g with
       | Err msg0 -> Err msg0
       | Res (a, g') ->
         (match transl_lblstmt ls1 g' with
          | Err msg0 -> Err msg0
          | Res (a0, g'0) -> Res ((Clight.LScons (c, a, a0)), g'0)))
  in transl_stmt0

(** val transl_function :
    composite_env -> coq_function -> Clight.coq_function res **)

let transl_function ce f =
  match transl_stmt ce f.fn_body (initial_generator ()) with
  | Err msg0 -> Error msg0
  | Res (tbody, g) ->
    OK { Clight.fn_return = f.fn_return; Clight.fn_callconv = f.fn_callconv;
      Clight.fn_params = f.fn_params; Clight.fn_vars = f.fn_vars; fn_temps =
      g.gen_trail; Clight.fn_body = tbody }

(** val transl_fundef :
    composite_env -> Csyntax.fundef -> Clight.fundef res **)

let transl_fundef ce = function
| Internal f ->
  (match transl_function ce f with
   | OK x -> OK (Internal x)
   | Error msg0 -> Error msg0)
| External (ef, targs, tres, cc) -> OK (External (ef, targs, tres, cc))

(** val transl_program : Csyntax.program -> Clight.program res **)

let transl_program p =
  match transform_partial_program (transl_fundef p.prog_comp_env)
          (program_of_program p) with
  | OK x ->
    OK { prog_defs = x.AST.prog_defs; prog_public = x.AST.prog_public;
      prog_main = x.AST.prog_main; prog_types = p.prog_types; prog_comp_env =
      p.prog_comp_env }
  | Error msg0 -> Error msg0
