open AST
open Archi
open BinInt
open BinNums
open Builtins1
open CminorSel
open Datatypes
open Floats
open Integers
open Op

(** val symbol_is_external : ident -> bool **)

let symbol_is_external = match Configuration.system with
    | "macos" -> C2C.atom_is_extern
    | "cygwin" when Archi.ptr64 -> C2C.atom_is_extern
    | _ -> (fun _ -> false)

(** val coq_Olea_ptr : addressing -> operation **)

let coq_Olea_ptr a =
  if ptr64 then Oleal a else Olea a

(** val addrsymbol : ident -> Ptrofs.int -> expr **)

let addrsymbol id ofs =
  if symbol_is_external id
  then if Ptrofs.eq ofs Ptrofs.zero
       then Eop ((Oindirectsymbol id), Enil)
       else Eop ((coq_Olea_ptr (Aindexed (Ptrofs.unsigned ofs))), (Econs
              ((Eop ((Oindirectsymbol id), Enil)), Enil)))
  else Eop ((coq_Olea_ptr (Aglobal (id, ofs))), Enil)

(** val addrstack : Ptrofs.int -> expr **)

let addrstack ofs =
  Eop ((coq_Olea_ptr (Ainstack ofs)), Enil)

type notint_cases =
| Coq_notint_case1 of Int.int
| Coq_notint_case2 of Int.int * expr
| Coq_notint_default of expr

(** val notint_match : expr -> notint_cases **)

let notint_match = function
| Eop (o, e0) ->
  (match o with
   | Ointconst n ->
     (match e0 with
      | Enil -> Coq_notint_case1 n
      | Econs (e1, e2) ->
        Coq_notint_default (Eop ((Ointconst n), (Econs (e1, e2)))))
   | Oxorimm n ->
     (match e0 with
      | Enil -> Coq_notint_default (Eop ((Oxorimm n), Enil))
      | Econs (e1, e2) ->
        (match e2 with
         | Enil -> Coq_notint_case2 (n, e1)
         | Econs (e3, e4) ->
           Coq_notint_default (Eop ((Oxorimm n), (Econs (e1, (Econs (e3,
             e4))))))))
   | x -> Coq_notint_default (Eop (x, e0)))
| x -> Coq_notint_default x

(** val notint : expr -> expr **)

let notint e =
  match notint_match e with
  | Coq_notint_case1 n -> Eop ((Ointconst (Int.not n)), Enil)
  | Coq_notint_case2 (n, e1) ->
    Eop ((Oxorimm (Int.not n)), (Econs (e1, Enil)))
  | Coq_notint_default e0 -> Eop (Onot, (Econs (e0, Enil)))

type addimm_cases =
| Coq_addimm_case1 of Int.int
| Coq_addimm_case2 of addressing * exprlist
| Coq_addimm_default of expr

(** val addimm_match : expr -> addimm_cases **)

let addimm_match = function
| Eop (o, args) ->
  (match o with
   | Ointconst m ->
     (match args with
      | Enil -> Coq_addimm_case1 m
      | Econs (e0, e1) ->
        Coq_addimm_default (Eop ((Ointconst m), (Econs (e0, e1)))))
   | Olea addr -> Coq_addimm_case2 (addr, args)
   | x -> Coq_addimm_default (Eop (x, args)))
| x -> Coq_addimm_default x

(** val addimm : Int.int -> expr -> expr **)

let addimm n e =
  if Int.eq n Int.zero
  then e
  else (match addimm_match e with
        | Coq_addimm_case1 m -> Eop ((Ointconst (Int.add n m)), Enil)
        | Coq_addimm_case2 (addr, args) ->
          Eop ((Olea (offset_addressing_total addr (Int.signed n))), args)
        | Coq_addimm_default e0 ->
          Eop ((Olea (Aindexed (Int.signed n))), (Econs (e0, Enil))))

type add_cases =
| Coq_add_case1 of Int.int * expr
| Coq_add_case2 of expr * Int.int
| Coq_add_case3 of coq_Z * expr * coq_Z * expr
| Coq_add_case4 of coq_Z * expr * coq_Z * coq_Z * expr
| Coq_add_case5 of coq_Z * coq_Z * expr * coq_Z * expr
| Coq_add_case6 of coq_Z * expr * ident * Ptrofs.int
| Coq_add_case7 of ident * Ptrofs.int * coq_Z * expr
| Coq_add_case8 of coq_Z * coq_Z * expr * ident * Ptrofs.int
| Coq_add_case9 of ident * Ptrofs.int * coq_Z * coq_Z * expr
| Coq_add_case10 of coq_Z * coq_Z * expr * expr
| Coq_add_case11 of expr * coq_Z * coq_Z * expr
| Coq_add_case12 of coq_Z * expr * expr
| Coq_add_case13 of expr * coq_Z * expr
| Coq_add_default of expr * expr

(** val add_match : expr -> expr -> add_cases **)

let add_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_add_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_add_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | Olea a ->
          (match a with
           | Aindexed n ->
             (match e with
              | Enil ->
                Coq_add_default (e3, (Eop ((Olea (Aindexed n)), Enil)))
              | Econs (t2, e0) ->
                (match e0 with
                 | Enil -> Coq_add_case13 (e3, n, t2)
                 | Econs (e4, e5) ->
                   Coq_add_default (e3, (Eop ((Olea (Aindexed n)), (Econs
                     (t2, (Econs (e4, e5)))))))))
           | Ascaled (sc, n) ->
             (match e with
              | Enil ->
                Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))), Enil)))
              | Econs (t2, e0) ->
                (match e0 with
                 | Enil -> Coq_add_case11 (e3, sc, n, t2)
                 | Econs (e4, e5) ->
                   Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))),
                     (Econs (t2, (Econs (e4, e5)))))))))
           | x -> Coq_add_default (e3, (Eop ((Olea x), e))))
        | x -> Coq_add_default (e3, (Eop (x, e))))
     | x -> Coq_add_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Ointconst n1 ->
       (match e with
        | Enil -> Coq_add_case1 (n1, e2)
        | Econs (e0, e3) ->
          let e4 = Eop ((Ointconst n1), (Econs (e0, e3))) in
          (match e2 with
           | Eop (o0, e5) ->
             (match o0 with
              | Ointconst n2 ->
                (match e5 with
                 | Enil -> Coq_add_case2 (e4, n2)
                 | Econs (e6, e7) ->
                   Coq_add_default (e4, (Eop ((Ointconst n2), (Econs (e6,
                     e7))))))
              | Olea a ->
                (match a with
                 | Aindexed n ->
                   (match e5 with
                    | Enil ->
                      Coq_add_default (e4, (Eop ((Olea (Aindexed n)), Enil)))
                    | Econs (t2, e6) ->
                      (match e6 with
                       | Enil -> Coq_add_case13 (e4, n, t2)
                       | Econs (e7, e8) ->
                         Coq_add_default (e4, (Eop ((Olea (Aindexed n)),
                           (Econs (t2, (Econs (e7, e8)))))))))
                 | Ascaled (sc, n) ->
                   (match e5 with
                    | Enil ->
                      Coq_add_default (e4, (Eop ((Olea (Ascaled (sc, n))),
                        Enil)))
                    | Econs (t2, e6) ->
                      (match e6 with
                       | Enil -> Coq_add_case11 (e4, sc, n, t2)
                       | Econs (e7, e8) ->
                         Coq_add_default (e4, (Eop ((Olea (Ascaled (sc, n))),
                           (Econs (t2, (Econs (e7, e8)))))))))
                 | x -> Coq_add_default (e4, (Eop ((Olea x), e5))))
              | x -> Coq_add_default (e4, (Eop (x, e5))))
           | x -> Coq_add_default (e4, x)))
     | Olea a ->
       (match a with
        | Aindexed n ->
          (match e with
           | Enil ->
             let e3 = Eop ((Olea (Aindexed n)), Enil) in
             (match e2 with
              | Eop (o0, e0) ->
                (match o0 with
                 | Ointconst n2 ->
                   (match e0 with
                    | Enil -> Coq_add_case2 (e3, n2)
                    | Econs (e4, e5) ->
                      Coq_add_default (e3, (Eop ((Ointconst n2), (Econs (e4,
                        e5))))))
                 | Olea a0 ->
                   (match a0 with
                    | Aindexed n0 ->
                      (match e0 with
                       | Enil ->
                         Coq_add_default (e3, (Eop ((Olea (Aindexed n0)),
                           Enil)))
                       | Econs (t2, e4) ->
                         (match e4 with
                          | Enil -> Coq_add_case13 (e3, n0, t2)
                          | Econs (e5, e6) ->
                            Coq_add_default (e3, (Eop ((Olea (Aindexed n0)),
                              (Econs (t2, (Econs (e5, e6)))))))))
                    | Ascaled (sc, n0) ->
                      (match e0 with
                       | Enil ->
                         Coq_add_default (e3, (Eop ((Olea (Ascaled (sc,
                           n0))), Enil)))
                       | Econs (t2, e4) ->
                         (match e4 with
                          | Enil -> Coq_add_case11 (e3, sc, n0, t2)
                          | Econs (e5, e6) ->
                            Coq_add_default (e3, (Eop ((Olea (Ascaled (sc,
                              n0))), (Econs (t2, (Econs (e5, e6)))))))))
                    | x -> Coq_add_default (e3, (Eop ((Olea x), e0))))
                 | x -> Coq_add_default (e3, (Eop (x, e0))))
              | x -> Coq_add_default (e3, x))
           | Econs (t1, e0) ->
             (match e0 with
              | Enil ->
                (match e2 with
                 | Eop (o0, e3) ->
                   (match o0 with
                    | Ointconst n2 ->
                      (match e3 with
                       | Enil ->
                         Coq_add_case2 ((Eop ((Olea (Aindexed n)), (Econs
                           (t1, Enil)))), n2)
                       | Econs (e4, e5) ->
                         Coq_add_case12 (n, t1, (Eop ((Ointconst n2), (Econs
                           (e4, e5))))))
                    | Olea a0 ->
                      (match a0 with
                       | Aindexed n0 ->
                         (match e3 with
                          | Enil ->
                            Coq_add_case12 (n, t1, (Eop ((Olea (Aindexed
                              n0)), Enil)))
                          | Econs (t2, e4) ->
                            (match e4 with
                             | Enil -> Coq_add_case3 (n, t1, n0, t2)
                             | Econs (e5, e6) ->
                               Coq_add_case12 (n, t1, (Eop ((Olea (Aindexed
                                 n0)), (Econs (t2, (Econs (e5, e6)))))))))
                       | Ascaled (sc, n0) ->
                         (match e3 with
                          | Enil ->
                            Coq_add_case12 (n, t1, (Eop ((Olea (Ascaled (sc,
                              n0))), Enil)))
                          | Econs (t2, e4) ->
                            (match e4 with
                             | Enil -> Coq_add_case4 (n, t1, sc, n0, t2)
                             | Econs (e5, e6) ->
                               Coq_add_case12 (n, t1, (Eop ((Olea (Ascaled
                                 (sc, n0))), (Econs (t2, (Econs (e5, e6)))))))))
                       | Aglobal (id, ofs) ->
                         (match e3 with
                          | Enil -> Coq_add_case6 (n, t1, id, ofs)
                          | Econs (e4, e5) ->
                            Coq_add_case12 (n, t1, (Eop ((Olea (Aglobal (id,
                              ofs))), (Econs (e4, e5))))))
                       | x -> Coq_add_case12 (n, t1, (Eop ((Olea x), e3))))
                    | x -> Coq_add_case12 (n, t1, (Eop (x, e3))))
                 | x -> Coq_add_case12 (n, t1, x))
              | Econs (e3, e4) ->
                let e5 = Eop ((Olea (Aindexed n)), (Econs (t1, (Econs (e3,
                  e4)))))
                in
                (match e2 with
                 | Eop (o0, e6) ->
                   (match o0 with
                    | Ointconst n2 ->
                      (match e6 with
                       | Enil -> Coq_add_case2 (e5, n2)
                       | Econs (e7, e8) ->
                         Coq_add_default (e5, (Eop ((Ointconst n2), (Econs
                           (e7, e8))))))
                    | Olea a0 ->
                      (match a0 with
                       | Aindexed n0 ->
                         (match e6 with
                          | Enil ->
                            Coq_add_default (e5, (Eop ((Olea (Aindexed n0)),
                              Enil)))
                          | Econs (t2, e7) ->
                            (match e7 with
                             | Enil -> Coq_add_case13 (e5, n0, t2)
                             | Econs (e8, e9) ->
                               Coq_add_default (e5, (Eop ((Olea (Aindexed
                                 n0)), (Econs (t2, (Econs (e8, e9)))))))))
                       | Ascaled (sc, n0) ->
                         (match e6 with
                          | Enil ->
                            Coq_add_default (e5, (Eop ((Olea (Ascaled (sc,
                              n0))), Enil)))
                          | Econs (t2, e7) ->
                            (match e7 with
                             | Enil -> Coq_add_case11 (e5, sc, n0, t2)
                             | Econs (e8, e9) ->
                               Coq_add_default (e5, (Eop ((Olea (Ascaled (sc,
                                 n0))), (Econs (t2, (Econs (e8, e9)))))))))
                       | x -> Coq_add_default (e5, (Eop ((Olea x), e6))))
                    | x -> Coq_add_default (e5, (Eop (x, e6))))
                 | x -> Coq_add_default (e5, x))))
        | Ascaled (sc, n) ->
          (match e with
           | Enil ->
             let e3 = Eop ((Olea (Ascaled (sc, n))), Enil) in
             (match e2 with
              | Eop (o0, e0) ->
                (match o0 with
                 | Ointconst n2 ->
                   (match e0 with
                    | Enil -> Coq_add_case2 (e3, n2)
                    | Econs (e4, e5) ->
                      Coq_add_default (e3, (Eop ((Ointconst n2), (Econs (e4,
                        e5))))))
                 | Olea a0 ->
                   (match a0 with
                    | Aindexed n0 ->
                      (match e0 with
                       | Enil ->
                         Coq_add_default (e3, (Eop ((Olea (Aindexed n0)),
                           Enil)))
                       | Econs (t2, e4) ->
                         (match e4 with
                          | Enil -> Coq_add_case13 (e3, n0, t2)
                          | Econs (e5, e6) ->
                            Coq_add_default (e3, (Eop ((Olea (Aindexed n0)),
                              (Econs (t2, (Econs (e5, e6)))))))))
                    | Ascaled (sc0, n0) ->
                      (match e0 with
                       | Enil ->
                         Coq_add_default (e3, (Eop ((Olea (Ascaled (sc0,
                           n0))), Enil)))
                       | Econs (t2, e4) ->
                         (match e4 with
                          | Enil -> Coq_add_case11 (e3, sc0, n0, t2)
                          | Econs (e5, e6) ->
                            Coq_add_default (e3, (Eop ((Olea (Ascaled (sc0,
                              n0))), (Econs (t2, (Econs (e5, e6)))))))))
                    | x -> Coq_add_default (e3, (Eop ((Olea x), e0))))
                 | x -> Coq_add_default (e3, (Eop (x, e0))))
              | x -> Coq_add_default (e3, x))
           | Econs (t1, e0) ->
             (match e0 with
              | Enil ->
                (match e2 with
                 | Eop (o0, e3) ->
                   (match o0 with
                    | Ointconst n2 ->
                      (match e3 with
                       | Enil ->
                         Coq_add_case2 ((Eop ((Olea (Ascaled (sc, n))),
                           (Econs (t1, Enil)))), n2)
                       | Econs (e4, e5) ->
                         Coq_add_case10 (sc, n, t1, (Eop ((Ointconst n2),
                           (Econs (e4, e5))))))
                    | Olea a0 ->
                      (match a0 with
                       | Aindexed n0 ->
                         (match e3 with
                          | Enil ->
                            Coq_add_case10 (sc, n, t1, (Eop ((Olea (Aindexed
                              n0)), Enil)))
                          | Econs (t2, e4) ->
                            (match e4 with
                             | Enil -> Coq_add_case5 (sc, n, t1, n0, t2)
                             | Econs (e5, e6) ->
                               Coq_add_case10 (sc, n, t1, (Eop ((Olea
                                 (Aindexed n0)), (Econs (t2, (Econs (e5,
                                 e6)))))))))
                       | Aglobal (id, ofs) ->
                         (match e3 with
                          | Enil -> Coq_add_case8 (sc, n, t1, id, ofs)
                          | Econs (e4, e5) ->
                            Coq_add_case10 (sc, n, t1, (Eop ((Olea (Aglobal
                              (id, ofs))), (Econs (e4, e5))))))
                       | x -> Coq_add_case10 (sc, n, t1, (Eop ((Olea x), e3))))
                    | x -> Coq_add_case10 (sc, n, t1, (Eop (x, e3))))
                 | x -> Coq_add_case10 (sc, n, t1, x))
              | Econs (e3, e4) ->
                let e5 = Eop ((Olea (Ascaled (sc, n))), (Econs (t1, (Econs
                  (e3, e4)))))
                in
                (match e2 with
                 | Eop (o0, e6) ->
                   (match o0 with
                    | Ointconst n2 ->
                      (match e6 with
                       | Enil -> Coq_add_case2 (e5, n2)
                       | Econs (e7, e8) ->
                         Coq_add_default (e5, (Eop ((Ointconst n2), (Econs
                           (e7, e8))))))
                    | Olea a0 ->
                      (match a0 with
                       | Aindexed n0 ->
                         (match e6 with
                          | Enil ->
                            Coq_add_default (e5, (Eop ((Olea (Aindexed n0)),
                              Enil)))
                          | Econs (t2, e7) ->
                            (match e7 with
                             | Enil -> Coq_add_case13 (e5, n0, t2)
                             | Econs (e8, e9) ->
                               Coq_add_default (e5, (Eop ((Olea (Aindexed
                                 n0)), (Econs (t2, (Econs (e8, e9)))))))))
                       | Ascaled (sc0, n0) ->
                         (match e6 with
                          | Enil ->
                            Coq_add_default (e5, (Eop ((Olea (Ascaled (sc0,
                              n0))), Enil)))
                          | Econs (t2, e7) ->
                            (match e7 with
                             | Enil -> Coq_add_case11 (e5, sc0, n0, t2)
                             | Econs (e8, e9) ->
                               Coq_add_default (e5, (Eop ((Olea (Ascaled
                                 (sc0, n0))), (Econs (t2, (Econs (e8,
                                 e9)))))))))
                       | x -> Coq_add_default (e5, (Eop ((Olea x), e6))))
                    | x -> Coq_add_default (e5, (Eop (x, e6))))
                 | x -> Coq_add_default (e5, x))))
        | Aglobal (id, ofs) ->
          (match e with
           | Enil ->
             let e3 = Eop ((Olea (Aglobal (id, ofs))), Enil) in
             (match e2 with
              | Eop (o0, e0) ->
                (match o0 with
                 | Ointconst n2 ->
                   (match e0 with
                    | Enil -> Coq_add_case2 (e3, n2)
                    | Econs (e4, e5) ->
                      Coq_add_default (e3, (Eop ((Ointconst n2), (Econs (e4,
                        e5))))))
                 | Olea a0 ->
                   (match a0 with
                    | Aindexed n ->
                      (match e0 with
                       | Enil ->
                         Coq_add_default (e3, (Eop ((Olea (Aindexed n)),
                           Enil)))
                       | Econs (t2, e4) ->
                         (match e4 with
                          | Enil -> Coq_add_case7 (id, ofs, n, t2)
                          | Econs (e5, e6) ->
                            Coq_add_default (e3, (Eop ((Olea (Aindexed n)),
                              (Econs (t2, (Econs (e5, e6)))))))))
                    | Ascaled (sc, n) ->
                      (match e0 with
                       | Enil ->
                         Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))),
                           Enil)))
                       | Econs (t2, e4) ->
                         (match e4 with
                          | Enil -> Coq_add_case9 (id, ofs, sc, n, t2)
                          | Econs (e5, e6) ->
                            Coq_add_default (e3, (Eop ((Olea (Ascaled (sc,
                              n))), (Econs (t2, (Econs (e5, e6)))))))))
                    | x -> Coq_add_default (e3, (Eop ((Olea x), e0))))
                 | x -> Coq_add_default (e3, (Eop (x, e0))))
              | x -> Coq_add_default (e3, x))
           | Econs (e0, e3) ->
             let e4 = Eop ((Olea (Aglobal (id, ofs))), (Econs (e0, e3))) in
             (match e2 with
              | Eop (o0, e5) ->
                (match o0 with
                 | Ointconst n2 ->
                   (match e5 with
                    | Enil -> Coq_add_case2 (e4, n2)
                    | Econs (e6, e7) ->
                      Coq_add_default (e4, (Eop ((Ointconst n2), (Econs (e6,
                        e7))))))
                 | Olea a0 ->
                   (match a0 with
                    | Aindexed n ->
                      (match e5 with
                       | Enil ->
                         Coq_add_default (e4, (Eop ((Olea (Aindexed n)),
                           Enil)))
                       | Econs (t2, e6) ->
                         (match e6 with
                          | Enil -> Coq_add_case13 (e4, n, t2)
                          | Econs (e7, e8) ->
                            Coq_add_default (e4, (Eop ((Olea (Aindexed n)),
                              (Econs (t2, (Econs (e7, e8)))))))))
                    | Ascaled (sc, n) ->
                      (match e5 with
                       | Enil ->
                         Coq_add_default (e4, (Eop ((Olea (Ascaled (sc, n))),
                           Enil)))
                       | Econs (t2, e6) ->
                         (match e6 with
                          | Enil -> Coq_add_case11 (e4, sc, n, t2)
                          | Econs (e7, e8) ->
                            Coq_add_default (e4, (Eop ((Olea (Ascaled (sc,
                              n))), (Econs (t2, (Econs (e7, e8)))))))))
                    | x -> Coq_add_default (e4, (Eop ((Olea x), e5))))
                 | x -> Coq_add_default (e4, (Eop (x, e5))))
              | x -> Coq_add_default (e4, x)))
        | x ->
          let e3 = Eop ((Olea x), e) in
          (match e2 with
           | Eop (o0, e0) ->
             (match o0 with
              | Ointconst n2 ->
                (match e0 with
                 | Enil -> Coq_add_case2 (e3, n2)
                 | Econs (e4, e5) ->
                   Coq_add_default (e3, (Eop ((Ointconst n2), (Econs (e4,
                     e5))))))
              | Olea a0 ->
                (match a0 with
                 | Aindexed n ->
                   (match e0 with
                    | Enil ->
                      Coq_add_default (e3, (Eop ((Olea (Aindexed n)), Enil)))
                    | Econs (t2, e4) ->
                      (match e4 with
                       | Enil -> Coq_add_case13 (e3, n, t2)
                       | Econs (e5, e6) ->
                         Coq_add_default (e3, (Eop ((Olea (Aindexed n)),
                           (Econs (t2, (Econs (e5, e6)))))))))
                 | Ascaled (sc, n) ->
                   (match e0 with
                    | Enil ->
                      Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))),
                        Enil)))
                    | Econs (t2, e4) ->
                      (match e4 with
                       | Enil -> Coq_add_case11 (e3, sc, n, t2)
                       | Econs (e5, e6) ->
                         Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))),
                           (Econs (t2, (Econs (e5, e6)))))))))
                 | x0 -> Coq_add_default (e3, (Eop ((Olea x0), e0))))
              | x0 -> Coq_add_default (e3, (Eop (x0, e0))))
           | x0 -> Coq_add_default (e3, x0)))
     | Oleal a ->
       let e3 = Eop ((Oleal a), e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Ointconst n2 ->
             (match e0 with
              | Enil -> Coq_add_case2 (e3, n2)
              | Econs (e4, e5) ->
                Coq_add_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
           | Olea a0 ->
             (match a0 with
              | Aindexed n ->
                (match e0 with
                 | Enil ->
                   Coq_add_default (e3, (Eop ((Olea (Aindexed n)), Enil)))
                 | Econs (t2, e4) ->
                   (match e4 with
                    | Enil -> Coq_add_case13 (e3, n, t2)
                    | Econs (e5, e6) ->
                      Coq_add_default (e3, (Eop ((Olea (Aindexed n)), (Econs
                        (t2, (Econs (e5, e6)))))))))
              | Ascaled (sc, n) ->
                (match e0 with
                 | Enil ->
                   Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))),
                     Enil)))
                 | Econs (t2, e4) ->
                   (match e4 with
                    | Enil -> Coq_add_case11 (e3, sc, n, t2)
                    | Econs (e5, e6) ->
                      Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))),
                        (Econs (t2, (Econs (e5, e6)))))))))
              | x -> Coq_add_default (e3, (Eop ((Olea x), e0))))
           | x -> Coq_add_default (e3, (Eop (x, e0))))
        | x -> Coq_add_default (e3, x))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Ointconst n2 ->
             (match e0 with
              | Enil -> Coq_add_case2 (e3, n2)
              | Econs (e4, e5) ->
                Coq_add_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
           | Olea a ->
             (match a with
              | Aindexed n ->
                (match e0 with
                 | Enil ->
                   Coq_add_default (e3, (Eop ((Olea (Aindexed n)), Enil)))
                 | Econs (t2, e4) ->
                   (match e4 with
                    | Enil -> Coq_add_case13 (e3, n, t2)
                    | Econs (e5, e6) ->
                      Coq_add_default (e3, (Eop ((Olea (Aindexed n)), (Econs
                        (t2, (Econs (e5, e6)))))))))
              | Ascaled (sc, n) ->
                (match e0 with
                 | Enil ->
                   Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))),
                     Enil)))
                 | Econs (t2, e4) ->
                   (match e4 with
                    | Enil -> Coq_add_case11 (e3, sc, n, t2)
                    | Econs (e5, e6) ->
                      Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))),
                        (Econs (t2, (Econs (e5, e6)))))))))
              | x0 -> Coq_add_default (e3, (Eop ((Olea x0), e0))))
           | x0 -> Coq_add_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_add_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_add_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_add_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | Olea a0 ->
          (match a0 with
           | Aindexed n ->
             (match e0 with
              | Enil ->
                Coq_add_default (e3, (Eop ((Olea (Aindexed n)), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_add_case13 (e3, n, t2)
                 | Econs (e5, e6) ->
                   Coq_add_default (e3, (Eop ((Olea (Aindexed n)), (Econs
                     (t2, (Econs (e5, e6)))))))))
           | Ascaled (sc, n) ->
             (match e0 with
              | Enil ->
                Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_add_case11 (e3, sc, n, t2)
                 | Econs (e5, e6) ->
                   Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))),
                     (Econs (t2, (Econs (e5, e6)))))))))
           | x -> Coq_add_default (e3, (Eop ((Olea x), e0))))
        | x -> Coq_add_default (e3, (Eop (x, e0))))
     | x -> Coq_add_default (e3, x))
  | Eletvar n0 ->
    let e3 = Eletvar n0 in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_add_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_add_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | Olea a ->
          (match a with
           | Aindexed n ->
             (match e with
              | Enil ->
                Coq_add_default (e3, (Eop ((Olea (Aindexed n)), Enil)))
              | Econs (t2, e0) ->
                (match e0 with
                 | Enil -> Coq_add_case13 (e3, n, t2)
                 | Econs (e4, e5) ->
                   Coq_add_default (e3, (Eop ((Olea (Aindexed n)), (Econs
                     (t2, (Econs (e4, e5)))))))))
           | Ascaled (sc, n) ->
             (match e with
              | Enil ->
                Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))), Enil)))
              | Econs (t2, e0) ->
                (match e0 with
                 | Enil -> Coq_add_case11 (e3, sc, n, t2)
                 | Econs (e4, e5) ->
                   Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))),
                     (Econs (t2, (Econs (e4, e5)))))))))
           | x -> Coq_add_default (e3, (Eop ((Olea x), e))))
        | x -> Coq_add_default (e3, (Eop (x, e))))
     | x -> Coq_add_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_add_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_add_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | Olea a ->
          (match a with
           | Aindexed n ->
             (match e0 with
              | Enil ->
                Coq_add_default (e3, (Eop ((Olea (Aindexed n)), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_add_case13 (e3, n, t2)
                 | Econs (e5, e6) ->
                   Coq_add_default (e3, (Eop ((Olea (Aindexed n)), (Econs
                     (t2, (Econs (e5, e6)))))))))
           | Ascaled (sc, n) ->
             (match e0 with
              | Enil ->
                Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_add_case11 (e3, sc, n, t2)
                 | Econs (e5, e6) ->
                   Coq_add_default (e3, (Eop ((Olea (Ascaled (sc, n))),
                     (Econs (t2, (Econs (e5, e6)))))))))
           | x -> Coq_add_default (e3, (Eop ((Olea x), e0))))
        | x -> Coq_add_default (e3, (Eop (x, e0))))
     | x -> Coq_add_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Ointconst n2 ->
          (match e3 with
           | Enil -> Coq_add_case2 (x, n2)
           | Econs (e4, e5) ->
             Coq_add_default (x, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | Olea a ->
          (match a with
           | Aindexed n ->
             (match e3 with
              | Enil -> Coq_add_default (x, (Eop ((Olea (Aindexed n)), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_add_case13 (x, n, t2)
                 | Econs (e5, e6) ->
                   Coq_add_default (x, (Eop ((Olea (Aindexed n)), (Econs (t2,
                     (Econs (e5, e6)))))))))
           | Ascaled (sc, n) ->
             (match e3 with
              | Enil ->
                Coq_add_default (x, (Eop ((Olea (Ascaled (sc, n))), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_add_case11 (x, sc, n, t2)
                 | Econs (e5, e6) ->
                   Coq_add_default (x, (Eop ((Olea (Ascaled (sc, n))), (Econs
                     (t2, (Econs (e5, e6)))))))))
           | x0 -> Coq_add_default (x, (Eop ((Olea x0), e3))))
        | x0 -> Coq_add_default (x, (Eop (x0, e3))))
     | x0 -> Coq_add_default (x, x0))

(** val add : expr -> expr -> expr **)

let add e1 e2 =
  match add_match e1 e2 with
  | Coq_add_case1 (n1, t2) -> addimm n1 t2
  | Coq_add_case2 (t1, n2) -> addimm n2 t1
  | Coq_add_case3 (n1, t1, n2, t2) ->
    Eop ((Olea (Aindexed2 (Z.add n1 n2))), (Econs (t1, (Econs (t2, Enil)))))
  | Coq_add_case4 (n1, t1, sc, n2, t2) ->
    Eop ((Olea (Aindexed2scaled (sc, (Z.add n1 n2)))), (Econs (t1, (Econs
      (t2, Enil)))))
  | Coq_add_case5 (sc, n1, t1, n2, t2) ->
    Eop ((Olea (Aindexed2scaled (sc, (Z.add n1 n2)))), (Econs (t2, (Econs
      (t1, Enil)))))
  | Coq_add_case6 (n1, t1, id, ofs) ->
    Eop ((Olea (Abased (id, (Ptrofs.add ofs (Ptrofs.repr n1))))), (Econs (t1,
      Enil)))
  | Coq_add_case7 (id, ofs, n2, t2) ->
    Eop ((Olea (Abased (id, (Ptrofs.add ofs (Ptrofs.repr n2))))), (Econs (t2,
      Enil)))
  | Coq_add_case8 (sc, n1, t1, id, ofs) ->
    Eop ((Olea (Abasedscaled (sc, id, (Ptrofs.add ofs (Ptrofs.repr n1))))),
      (Econs (t1, Enil)))
  | Coq_add_case9 (id, ofs, sc, n2, t2) ->
    Eop ((Olea (Abasedscaled (sc, id, (Ptrofs.add ofs (Ptrofs.repr n2))))),
      (Econs (t2, Enil)))
  | Coq_add_case10 (sc, n, t1, t2) ->
    Eop ((Olea (Aindexed2scaled (sc, n))), (Econs (t2, (Econs (t1, Enil)))))
  | Coq_add_case11 (t1, sc, n, t2) ->
    Eop ((Olea (Aindexed2scaled (sc, n))), (Econs (t1, (Econs (t2, Enil)))))
  | Coq_add_case12 (n, t1, t2) ->
    Eop ((Olea (Aindexed2 n)), (Econs (t1, (Econs (t2, Enil)))))
  | Coq_add_case13 (t1, n, t2) ->
    Eop ((Olea (Aindexed2 n)), (Econs (t1, (Econs (t2, Enil)))))
  | Coq_add_default (e3, e4) ->
    Eop ((Olea (Aindexed2 Z0)), (Econs (e3, (Econs (e4, Enil)))))

type negint_cases =
| Coq_negint_case1 of Int.int
| Coq_negint_default of expr

(** val negint_match : expr -> negint_cases **)

let negint_match = function
| Eop (o, e0) ->
  (match o with
   | Ointconst n ->
     (match e0 with
      | Enil -> Coq_negint_case1 n
      | Econs (e1, e2) ->
        Coq_negint_default (Eop ((Ointconst n), (Econs (e1, e2)))))
   | x -> Coq_negint_default (Eop (x, e0)))
| x -> Coq_negint_default x

(** val negint : expr -> expr **)

let negint e =
  match negint_match e with
  | Coq_negint_case1 n -> Eop ((Ointconst (Int.neg n)), Enil)
  | Coq_negint_default e0 -> Eop (Oneg, (Econs (e0, Enil)))

type sub_cases =
| Coq_sub_case1 of expr * Int.int
| Coq_sub_case2 of coq_Z * expr * coq_Z * expr
| Coq_sub_case3 of coq_Z * expr * expr
| Coq_sub_case4 of expr * coq_Z * expr
| Coq_sub_default of expr * expr

(** val sub_match : expr -> expr -> sub_cases **)

let sub_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_sub_case1 (e3, n2)
           | Econs (e0, e4) ->
             Coq_sub_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | Olea a ->
          (match a with
           | Aindexed n2 ->
             (match e with
              | Enil ->
                Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)), Enil)))
              | Econs (t2, e0) ->
                (match e0 with
                 | Enil -> Coq_sub_case4 (e3, n2, t2)
                 | Econs (e4, e5) ->
                   Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)), (Econs
                     (t2, (Econs (e4, e5)))))))))
           | x -> Coq_sub_default (e3, (Eop ((Olea x), e))))
        | x -> Coq_sub_default (e3, (Eop (x, e))))
     | x -> Coq_sub_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Olea a ->
       (match a with
        | Aindexed n1 ->
          (match e with
           | Enil ->
             let e3 = Eop ((Olea (Aindexed n1)), Enil) in
             (match e2 with
              | Eop (o0, e0) ->
                (match o0 with
                 | Ointconst n2 ->
                   (match e0 with
                    | Enil -> Coq_sub_case1 (e3, n2)
                    | Econs (e4, e5) ->
                      Coq_sub_default (e3, (Eop ((Ointconst n2), (Econs (e4,
                        e5))))))
                 | Olea a0 ->
                   (match a0 with
                    | Aindexed n2 ->
                      (match e0 with
                       | Enil ->
                         Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)),
                           Enil)))
                       | Econs (t2, e4) ->
                         (match e4 with
                          | Enil -> Coq_sub_case4 (e3, n2, t2)
                          | Econs (e5, e6) ->
                            Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)),
                              (Econs (t2, (Econs (e5, e6)))))))))
                    | x -> Coq_sub_default (e3, (Eop ((Olea x), e0))))
                 | x -> Coq_sub_default (e3, (Eop (x, e0))))
              | x -> Coq_sub_default (e3, x))
           | Econs (t1, e0) ->
             (match e0 with
              | Enil ->
                (match e2 with
                 | Eop (o0, e3) ->
                   (match o0 with
                    | Ointconst n2 ->
                      (match e3 with
                       | Enil ->
                         Coq_sub_case1 ((Eop ((Olea (Aindexed n1)), (Econs
                           (t1, Enil)))), n2)
                       | Econs (e4, e5) ->
                         Coq_sub_case3 (n1, t1, (Eop ((Ointconst n2), (Econs
                           (e4, e5))))))
                    | Olea a0 ->
                      (match a0 with
                       | Aindexed n2 ->
                         (match e3 with
                          | Enil ->
                            Coq_sub_case3 (n1, t1, (Eop ((Olea (Aindexed
                              n2)), Enil)))
                          | Econs (t2, e4) ->
                            (match e4 with
                             | Enil -> Coq_sub_case2 (n1, t1, n2, t2)
                             | Econs (e5, e6) ->
                               Coq_sub_case3 (n1, t1, (Eop ((Olea (Aindexed
                                 n2)), (Econs (t2, (Econs (e5, e6)))))))))
                       | x -> Coq_sub_case3 (n1, t1, (Eop ((Olea x), e3))))
                    | x -> Coq_sub_case3 (n1, t1, (Eop (x, e3))))
                 | x -> Coq_sub_case3 (n1, t1, x))
              | Econs (e3, e4) ->
                let e5 = Eop ((Olea (Aindexed n1)), (Econs (t1, (Econs (e3,
                  e4)))))
                in
                (match e2 with
                 | Eop (o0, e6) ->
                   (match o0 with
                    | Ointconst n2 ->
                      (match e6 with
                       | Enil -> Coq_sub_case1 (e5, n2)
                       | Econs (e7, e8) ->
                         Coq_sub_default (e5, (Eop ((Ointconst n2), (Econs
                           (e7, e8))))))
                    | Olea a0 ->
                      (match a0 with
                       | Aindexed n2 ->
                         (match e6 with
                          | Enil ->
                            Coq_sub_default (e5, (Eop ((Olea (Aindexed n2)),
                              Enil)))
                          | Econs (t2, e7) ->
                            (match e7 with
                             | Enil -> Coq_sub_case4 (e5, n2, t2)
                             | Econs (e8, e9) ->
                               Coq_sub_default (e5, (Eop ((Olea (Aindexed
                                 n2)), (Econs (t2, (Econs (e8, e9)))))))))
                       | x -> Coq_sub_default (e5, (Eop ((Olea x), e6))))
                    | x -> Coq_sub_default (e5, (Eop (x, e6))))
                 | x -> Coq_sub_default (e5, x))))
        | x ->
          let e3 = Eop ((Olea x), e) in
          (match e2 with
           | Eop (o0, e0) ->
             (match o0 with
              | Ointconst n2 ->
                (match e0 with
                 | Enil -> Coq_sub_case1 (e3, n2)
                 | Econs (e4, e5) ->
                   Coq_sub_default (e3, (Eop ((Ointconst n2), (Econs (e4,
                     e5))))))
              | Olea a0 ->
                (match a0 with
                 | Aindexed n2 ->
                   (match e0 with
                    | Enil ->
                      Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)), Enil)))
                    | Econs (t2, e4) ->
                      (match e4 with
                       | Enil -> Coq_sub_case4 (e3, n2, t2)
                       | Econs (e5, e6) ->
                         Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)),
                           (Econs (t2, (Econs (e5, e6)))))))))
                 | x0 -> Coq_sub_default (e3, (Eop ((Olea x0), e0))))
              | x0 -> Coq_sub_default (e3, (Eop (x0, e0))))
           | x0 -> Coq_sub_default (e3, x0)))
     | Oleal a ->
       let e3 = Eop ((Oleal a), e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Ointconst n2 ->
             (match e0 with
              | Enil -> Coq_sub_case1 (e3, n2)
              | Econs (e4, e5) ->
                Coq_sub_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
           | Olea a0 ->
             (match a0 with
              | Aindexed n2 ->
                (match e0 with
                 | Enil ->
                   Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)), Enil)))
                 | Econs (t2, e4) ->
                   (match e4 with
                    | Enil -> Coq_sub_case4 (e3, n2, t2)
                    | Econs (e5, e6) ->
                      Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)), (Econs
                        (t2, (Econs (e5, e6)))))))))
              | x -> Coq_sub_default (e3, (Eop ((Olea x), e0))))
           | x -> Coq_sub_default (e3, (Eop (x, e0))))
        | x -> Coq_sub_default (e3, x))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Ointconst n2 ->
             (match e0 with
              | Enil -> Coq_sub_case1 (e3, n2)
              | Econs (e4, e5) ->
                Coq_sub_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
           | Olea a ->
             (match a with
              | Aindexed n2 ->
                (match e0 with
                 | Enil ->
                   Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)), Enil)))
                 | Econs (t2, e4) ->
                   (match e4 with
                    | Enil -> Coq_sub_case4 (e3, n2, t2)
                    | Econs (e5, e6) ->
                      Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)), (Econs
                        (t2, (Econs (e5, e6)))))))))
              | x0 -> Coq_sub_default (e3, (Eop ((Olea x0), e0))))
           | x0 -> Coq_sub_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_sub_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_sub_case1 (e3, n2)
           | Econs (e4, e5) ->
             Coq_sub_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | Olea a0 ->
          (match a0 with
           | Aindexed n2 ->
             (match e0 with
              | Enil ->
                Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_sub_case4 (e3, n2, t2)
                 | Econs (e5, e6) ->
                   Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)), (Econs
                     (t2, (Econs (e5, e6)))))))))
           | x -> Coq_sub_default (e3, (Eop ((Olea x), e0))))
        | x -> Coq_sub_default (e3, (Eop (x, e0))))
     | x -> Coq_sub_default (e3, x))
  | Eletvar n ->
    let e3 = Eletvar n in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_sub_case1 (e3, n2)
           | Econs (e0, e4) ->
             Coq_sub_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | Olea a ->
          (match a with
           | Aindexed n2 ->
             (match e with
              | Enil ->
                Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)), Enil)))
              | Econs (t2, e0) ->
                (match e0 with
                 | Enil -> Coq_sub_case4 (e3, n2, t2)
                 | Econs (e4, e5) ->
                   Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)), (Econs
                     (t2, (Econs (e4, e5)))))))))
           | x -> Coq_sub_default (e3, (Eop ((Olea x), e))))
        | x -> Coq_sub_default (e3, (Eop (x, e))))
     | x -> Coq_sub_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_sub_case1 (e3, n2)
           | Econs (e4, e5) ->
             Coq_sub_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | Olea a ->
          (match a with
           | Aindexed n2 ->
             (match e0 with
              | Enil ->
                Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_sub_case4 (e3, n2, t2)
                 | Econs (e5, e6) ->
                   Coq_sub_default (e3, (Eop ((Olea (Aindexed n2)), (Econs
                     (t2, (Econs (e5, e6)))))))))
           | x -> Coq_sub_default (e3, (Eop ((Olea x), e0))))
        | x -> Coq_sub_default (e3, (Eop (x, e0))))
     | x -> Coq_sub_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Ointconst n2 ->
          (match e3 with
           | Enil -> Coq_sub_case1 (x, n2)
           | Econs (e4, e5) ->
             Coq_sub_default (x, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | Olea a ->
          (match a with
           | Aindexed n2 ->
             (match e3 with
              | Enil ->
                Coq_sub_default (x, (Eop ((Olea (Aindexed n2)), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_sub_case4 (x, n2, t2)
                 | Econs (e5, e6) ->
                   Coq_sub_default (x, (Eop ((Olea (Aindexed n2)), (Econs
                     (t2, (Econs (e5, e6)))))))))
           | x0 -> Coq_sub_default (x, (Eop ((Olea x0), e3))))
        | x0 -> Coq_sub_default (x, (Eop (x0, e3))))
     | x0 -> Coq_sub_default (x, x0))

(** val sub : expr -> expr -> expr **)

let sub e1 e2 =
  match sub_match e1 e2 with
  | Coq_sub_case1 (t1, n2) -> addimm (Int.neg n2) t1
  | Coq_sub_case2 (n1, t1, n2, t2) ->
    addimm (Int.repr (Z.sub n1 n2)) (Eop (Osub, (Econs (t1, (Econs (t2,
      Enil))))))
  | Coq_sub_case3 (n1, t1, t2) ->
    addimm (Int.repr n1) (Eop (Osub, (Econs (t1, (Econs (t2, Enil))))))
  | Coq_sub_case4 (t1, n2, t2) ->
    addimm (Int.repr (Z.opp n2)) (Eop (Osub, (Econs (t1, (Econs (t2,
      Enil))))))
  | Coq_sub_default (e3, e4) -> Eop (Osub, (Econs (e3, (Econs (e4, Enil)))))

(** val shift_is_scale : Int.int -> bool **)

let shift_is_scale n =
  (||)
    ((||) (Int.eq n (Int.repr (Zpos Coq_xH)))
      (Int.eq n (Int.repr (Zpos (Coq_xO Coq_xH)))))
    (Int.eq n (Int.repr (Zpos (Coq_xI Coq_xH))))

type shlimm_cases =
| Coq_shlimm_case1 of Int.int
| Coq_shlimm_case2 of Int.int * expr
| Coq_shlimm_case3 of coq_Z * expr
| Coq_shlimm_default of expr

(** val shlimm_match : expr -> shlimm_cases **)

let shlimm_match = function
| Eop (o, e) ->
  (match o with
   | Ointconst n1 ->
     (match e with
      | Enil -> Coq_shlimm_case1 n1
      | Econs (e0, e2) ->
        Coq_shlimm_default (Eop ((Ointconst n1), (Econs (e0, e2)))))
   | Oshlimm n1 ->
     (match e with
      | Enil -> Coq_shlimm_default (Eop ((Oshlimm n1), Enil))
      | Econs (t1, e0) ->
        (match e0 with
         | Enil -> Coq_shlimm_case2 (n1, t1)
         | Econs (e2, e3) ->
           Coq_shlimm_default (Eop ((Oshlimm n1), (Econs (t1, (Econs (e2,
             e3))))))))
   | Olea a ->
     (match a with
      | Aindexed n1 ->
        (match e with
         | Enil -> Coq_shlimm_default (Eop ((Olea (Aindexed n1)), Enil))
         | Econs (t1, e0) ->
           (match e0 with
            | Enil -> Coq_shlimm_case3 (n1, t1)
            | Econs (e2, e3) ->
              Coq_shlimm_default (Eop ((Olea (Aindexed n1)), (Econs (t1,
                (Econs (e2, e3))))))))
      | x -> Coq_shlimm_default (Eop ((Olea x), e)))
   | x -> Coq_shlimm_default (Eop (x, e)))
| x -> Coq_shlimm_default x

(** val shlimm : expr -> Int.int -> expr **)

let shlimm e1 n =
  if Int.eq n Int.zero
  then e1
  else if negb (Int.ltu n Int.iwordsize)
       then Eop (Oshl, (Econs (e1, (Econs ((Eop ((Ointconst n), Enil)),
              Enil)))))
       else (match shlimm_match e1 with
             | Coq_shlimm_case1 n1 -> Eop ((Ointconst (Int.shl n1 n)), Enil)
             | Coq_shlimm_case2 (n1, t1) ->
               if Int.ltu (Int.add n n1) Int.iwordsize
               then Eop ((Oshlimm (Int.add n n1)), (Econs (t1, Enil)))
               else Eop ((Oshlimm n), (Econs (e1, Enil)))
             | Coq_shlimm_case3 (n1, t1) ->
               if shift_is_scale n
               then Eop ((Olea (Ascaled ((Int.unsigned (Int.shl Int.one n)),
                      (Int.unsigned (Int.shl (Int.repr n1) n))))), (Econs
                      (t1, Enil)))
               else Eop ((Oshlimm n), (Econs (e1, Enil)))
             | Coq_shlimm_default e2 ->
               if shift_is_scale n
               then Eop ((Olea (Ascaled ((Int.unsigned (Int.shl Int.one n)),
                      Z0))), (Econs (e2, Enil)))
               else Eop ((Oshlimm n), (Econs (e2, Enil))))

type shruimm_cases =
| Coq_shruimm_case1 of Int.int
| Coq_shruimm_case2 of Int.int * expr
| Coq_shruimm_default of expr

(** val shruimm_match : expr -> shruimm_cases **)

let shruimm_match = function
| Eop (o, e) ->
  (match o with
   | Ointconst n1 ->
     (match e with
      | Enil -> Coq_shruimm_case1 n1
      | Econs (e0, e2) ->
        Coq_shruimm_default (Eop ((Ointconst n1), (Econs (e0, e2)))))
   | Oshruimm n1 ->
     (match e with
      | Enil -> Coq_shruimm_default (Eop ((Oshruimm n1), Enil))
      | Econs (t1, e0) ->
        (match e0 with
         | Enil -> Coq_shruimm_case2 (n1, t1)
         | Econs (e2, e3) ->
           Coq_shruimm_default (Eop ((Oshruimm n1), (Econs (t1, (Econs (e2,
             e3))))))))
   | x -> Coq_shruimm_default (Eop (x, e)))
| x -> Coq_shruimm_default x

(** val shruimm : expr -> Int.int -> expr **)

let shruimm e1 n =
  if Int.eq n Int.zero
  then e1
  else if negb (Int.ltu n Int.iwordsize)
       then Eop (Oshru, (Econs (e1, (Econs ((Eop ((Ointconst n), Enil)),
              Enil)))))
       else (match shruimm_match e1 with
             | Coq_shruimm_case1 n1 -> Eop ((Ointconst (Int.shru n1 n)), Enil)
             | Coq_shruimm_case2 (n1, t1) ->
               if Int.ltu (Int.add n n1) Int.iwordsize
               then Eop ((Oshruimm (Int.add n n1)), (Econs (t1, Enil)))
               else Eop ((Oshruimm n), (Econs (e1, Enil)))
             | Coq_shruimm_default e2 ->
               Eop ((Oshruimm n), (Econs (e2, Enil))))

type shrimm_cases =
| Coq_shrimm_case1 of Int.int
| Coq_shrimm_case2 of Int.int * expr
| Coq_shrimm_default of expr

(** val shrimm_match : expr -> shrimm_cases **)

let shrimm_match = function
| Eop (o, e) ->
  (match o with
   | Ointconst n1 ->
     (match e with
      | Enil -> Coq_shrimm_case1 n1
      | Econs (e0, e2) ->
        Coq_shrimm_default (Eop ((Ointconst n1), (Econs (e0, e2)))))
   | Oshrimm n1 ->
     (match e with
      | Enil -> Coq_shrimm_default (Eop ((Oshrimm n1), Enil))
      | Econs (t1, e0) ->
        (match e0 with
         | Enil -> Coq_shrimm_case2 (n1, t1)
         | Econs (e2, e3) ->
           Coq_shrimm_default (Eop ((Oshrimm n1), (Econs (t1, (Econs (e2,
             e3))))))))
   | x -> Coq_shrimm_default (Eop (x, e)))
| x -> Coq_shrimm_default x

(** val shrimm : expr -> Int.int -> expr **)

let shrimm e1 n =
  if Int.eq n Int.zero
  then e1
  else if negb (Int.ltu n Int.iwordsize)
       then Eop (Oshr, (Econs (e1, (Econs ((Eop ((Ointconst n), Enil)),
              Enil)))))
       else (match shrimm_match e1 with
             | Coq_shrimm_case1 n1 -> Eop ((Ointconst (Int.shr n1 n)), Enil)
             | Coq_shrimm_case2 (n1, t1) ->
               if Int.ltu (Int.add n n1) Int.iwordsize
               then Eop ((Oshrimm (Int.add n n1)), (Econs (t1, Enil)))
               else Eop ((Oshrimm n), (Econs (e1, Enil)))
             | Coq_shrimm_default e2 -> Eop ((Oshrimm n), (Econs (e2, Enil))))

(** val mulimm_base : Int.int -> expr -> expr **)

let mulimm_base n1 e2 =
  match Int.one_bits n1 with
  | [] -> Eop ((Omulimm n1), (Econs (e2, Enil)))
  | i :: l ->
    (match l with
     | [] -> shlimm e2 i
     | j :: l0 ->
       (match l0 with
        | [] -> Elet (e2, (add (shlimm (Eletvar O) i) (shlimm (Eletvar O) j)))
        | _ :: _ -> Eop ((Omulimm n1), (Econs (e2, Enil)))))

type mulimm_cases =
| Coq_mulimm_case1 of Int.int
| Coq_mulimm_case2 of coq_Z * expr
| Coq_mulimm_default of expr

(** val mulimm_match : expr -> mulimm_cases **)

let mulimm_match = function
| Eop (o, e) ->
  (match o with
   | Ointconst n2 ->
     (match e with
      | Enil -> Coq_mulimm_case1 n2
      | Econs (e0, e1) ->
        Coq_mulimm_default (Eop ((Ointconst n2), (Econs (e0, e1)))))
   | Olea a ->
     (match a with
      | Aindexed n2 ->
        (match e with
         | Enil -> Coq_mulimm_default (Eop ((Olea (Aindexed n2)), Enil))
         | Econs (t2, e0) ->
           (match e0 with
            | Enil -> Coq_mulimm_case2 (n2, t2)
            | Econs (e1, e3) ->
              Coq_mulimm_default (Eop ((Olea (Aindexed n2)), (Econs (t2,
                (Econs (e1, e3))))))))
      | x -> Coq_mulimm_default (Eop ((Olea x), e)))
   | x -> Coq_mulimm_default (Eop (x, e)))
| x -> Coq_mulimm_default x

(** val mulimm : Int.int -> expr -> expr **)

let mulimm n1 e2 =
  if Int.eq n1 Int.zero
  then Eop ((Ointconst Int.zero), Enil)
  else if Int.eq n1 Int.one
       then e2
       else (match mulimm_match e2 with
             | Coq_mulimm_case1 n2 -> Eop ((Ointconst (Int.mul n1 n2)), Enil)
             | Coq_mulimm_case2 (n2, t2) ->
               addimm (Int.mul n1 (Int.repr n2)) (mulimm_base n1 t2)
             | Coq_mulimm_default e3 -> mulimm_base n1 e3)

type mul_cases =
| Coq_mul_case1 of Int.int * expr
| Coq_mul_case2 of expr * Int.int
| Coq_mul_default of expr * expr

(** val mul_match : expr -> expr -> mul_cases **)

let mul_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_mul_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_mul_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | x -> Coq_mul_default (e3, (Eop (x, e))))
     | x -> Coq_mul_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Ointconst n1 ->
       (match e with
        | Enil -> Coq_mul_case1 (n1, e2)
        | Econs (e0, e3) ->
          let e4 = Eop ((Ointconst n1), (Econs (e0, e3))) in
          (match e2 with
           | Eop (o0, e5) ->
             (match o0 with
              | Ointconst n2 ->
                (match e5 with
                 | Enil -> Coq_mul_case2 (e4, n2)
                 | Econs (e6, e7) ->
                   Coq_mul_default (e4, (Eop ((Ointconst n2), (Econs (e6,
                     e7))))))
              | x -> Coq_mul_default (e4, (Eop (x, e5))))
           | x -> Coq_mul_default (e4, x)))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Ointconst n2 ->
             (match e0 with
              | Enil -> Coq_mul_case2 (e3, n2)
              | Econs (e4, e5) ->
                Coq_mul_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
           | x0 -> Coq_mul_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_mul_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_mul_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_mul_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x -> Coq_mul_default (e3, (Eop (x, e0))))
     | x -> Coq_mul_default (e3, x))
  | Eletvar n ->
    let e3 = Eletvar n in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_mul_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_mul_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | x -> Coq_mul_default (e3, (Eop (x, e))))
     | x -> Coq_mul_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_mul_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_mul_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x -> Coq_mul_default (e3, (Eop (x, e0))))
     | x -> Coq_mul_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Ointconst n2 ->
          (match e3 with
           | Enil -> Coq_mul_case2 (x, n2)
           | Econs (e4, e5) ->
             Coq_mul_default (x, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x0 -> Coq_mul_default (x, (Eop (x0, e3))))
     | x0 -> Coq_mul_default (x, x0))

(** val mul : expr -> expr -> expr **)

let mul e1 e2 =
  match mul_match e1 e2 with
  | Coq_mul_case1 (n1, t2) -> mulimm n1 t2
  | Coq_mul_case2 (t1, n2) -> mulimm n2 t1
  | Coq_mul_default (e3, e4) -> Eop (Omul, (Econs (e3, (Econs (e4, Enil)))))

(** val mulhs : expr -> expr -> expr **)

let mulhs e1 e2 =
  Eop (Omulhs, (Econs (e1, (Econs (e2, Enil)))))

(** val mulhu : expr -> expr -> expr **)

let mulhu e1 e2 =
  Eop (Omulhu, (Econs (e1, (Econs (e2, Enil)))))

type andimm_cases =
| Coq_andimm_case1 of Int.int
| Coq_andimm_case2 of Int.int * expr
| Coq_andimm_case3 of expr
| Coq_andimm_case4 of expr
| Coq_andimm_default of expr

(** val andimm_match : expr -> andimm_cases **)

let andimm_match = function
| Eop (o, e) ->
  (match o with
   | Ointconst n2 ->
     (match e with
      | Enil -> Coq_andimm_case1 n2
      | Econs (e0, e1) ->
        Coq_andimm_default (Eop ((Ointconst n2), (Econs (e0, e1)))))
   | Ocast8unsigned ->
     (match e with
      | Enil -> Coq_andimm_default (Eop (Ocast8unsigned, Enil))
      | Econs (t2, e0) ->
        (match e0 with
         | Enil -> Coq_andimm_case3 t2
         | Econs (e1, e3) ->
           Coq_andimm_default (Eop (Ocast8unsigned, (Econs (t2, (Econs (e1,
             e3))))))))
   | Ocast16unsigned ->
     (match e with
      | Enil -> Coq_andimm_default (Eop (Ocast16unsigned, Enil))
      | Econs (t2, e0) ->
        (match e0 with
         | Enil -> Coq_andimm_case4 t2
         | Econs (e1, e3) ->
           Coq_andimm_default (Eop (Ocast16unsigned, (Econs (t2, (Econs (e1,
             e3))))))))
   | Oandimm n2 ->
     (match e with
      | Enil -> Coq_andimm_default (Eop ((Oandimm n2), Enil))
      | Econs (t2, e0) ->
        (match e0 with
         | Enil -> Coq_andimm_case2 (n2, t2)
         | Econs (e1, e3) ->
           Coq_andimm_default (Eop ((Oandimm n2), (Econs (t2, (Econs (e1,
             e3))))))))
   | x -> Coq_andimm_default (Eop (x, e)))
| x -> Coq_andimm_default x

(** val andimm : Int.int -> expr -> expr **)

let andimm n1 e2 =
  if Int.eq n1 Int.zero
  then Eop ((Ointconst Int.zero), Enil)
  else if Int.eq n1 Int.mone
       then e2
       else (match andimm_match e2 with
             | Coq_andimm_case1 n2 ->
               Eop ((Ointconst (Int.coq_and n1 n2)), Enil)
             | Coq_andimm_case2 (n2, t2) ->
               Eop ((Oandimm (Int.coq_and n1 n2)), (Econs (t2, Enil)))
             | Coq_andimm_case3 t2 ->
               Eop ((Oandimm
                 (Int.coq_and n1
                   (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
                     (Coq_xI (Coq_xI Coq_xH))))))))))), (Econs (t2, Enil)))
             | Coq_andimm_case4 t2 ->
               Eop ((Oandimm
                 (Int.coq_and n1
                   (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
                     (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
                     (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))))))))))))))),
                 (Econs (t2, Enil)))
             | Coq_andimm_default e3 -> Eop ((Oandimm n1), (Econs (e3, Enil))))

type and_cases =
| Coq_and_case1 of Int.int * expr
| Coq_and_case2 of expr * Int.int
| Coq_and_default of expr * expr

(** val and_match : expr -> expr -> and_cases **)

let and_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_and_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_and_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | x -> Coq_and_default (e3, (Eop (x, e))))
     | x -> Coq_and_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Ointconst n1 ->
       (match e with
        | Enil -> Coq_and_case1 (n1, e2)
        | Econs (e0, e3) ->
          let e4 = Eop ((Ointconst n1), (Econs (e0, e3))) in
          (match e2 with
           | Eop (o0, e5) ->
             (match o0 with
              | Ointconst n2 ->
                (match e5 with
                 | Enil -> Coq_and_case2 (e4, n2)
                 | Econs (e6, e7) ->
                   Coq_and_default (e4, (Eop ((Ointconst n2), (Econs (e6,
                     e7))))))
              | x -> Coq_and_default (e4, (Eop (x, e5))))
           | x -> Coq_and_default (e4, x)))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Ointconst n2 ->
             (match e0 with
              | Enil -> Coq_and_case2 (e3, n2)
              | Econs (e4, e5) ->
                Coq_and_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
           | x0 -> Coq_and_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_and_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_and_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_and_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x -> Coq_and_default (e3, (Eop (x, e0))))
     | x -> Coq_and_default (e3, x))
  | Eletvar n ->
    let e3 = Eletvar n in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_and_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_and_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | x -> Coq_and_default (e3, (Eop (x, e))))
     | x -> Coq_and_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_and_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_and_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x -> Coq_and_default (e3, (Eop (x, e0))))
     | x -> Coq_and_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Ointconst n2 ->
          (match e3 with
           | Enil -> Coq_and_case2 (x, n2)
           | Econs (e4, e5) ->
             Coq_and_default (x, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x0 -> Coq_and_default (x, (Eop (x0, e3))))
     | x0 -> Coq_and_default (x, x0))

(** val coq_and : expr -> expr -> expr **)

let coq_and e1 e2 =
  match and_match e1 e2 with
  | Coq_and_case1 (n1, t2) -> andimm n1 t2
  | Coq_and_case2 (t1, n2) -> andimm n2 t1
  | Coq_and_default (e3, e4) -> Eop (Oand, (Econs (e3, (Econs (e4, Enil)))))

type orimm_cases =
| Coq_orimm_case1 of Int.int
| Coq_orimm_case2 of Int.int * expr
| Coq_orimm_default of expr

(** val orimm_match : expr -> orimm_cases **)

let orimm_match = function
| Eop (o, e) ->
  (match o with
   | Ointconst n2 ->
     (match e with
      | Enil -> Coq_orimm_case1 n2
      | Econs (e0, e1) ->
        Coq_orimm_default (Eop ((Ointconst n2), (Econs (e0, e1)))))
   | Oorimm n2 ->
     (match e with
      | Enil -> Coq_orimm_default (Eop ((Oorimm n2), Enil))
      | Econs (t2, e0) ->
        (match e0 with
         | Enil -> Coq_orimm_case2 (n2, t2)
         | Econs (e1, e3) ->
           Coq_orimm_default (Eop ((Oorimm n2), (Econs (t2, (Econs (e1,
             e3))))))))
   | x -> Coq_orimm_default (Eop (x, e)))
| x -> Coq_orimm_default x

(** val orimm : Int.int -> expr -> expr **)

let orimm n1 e2 =
  if Int.eq n1 Int.zero
  then e2
  else if Int.eq n1 Int.mone
       then Eop ((Ointconst Int.mone), Enil)
       else (match orimm_match e2 with
             | Coq_orimm_case1 n2 ->
               Eop ((Ointconst (Int.coq_or n1 n2)), Enil)
             | Coq_orimm_case2 (n2, t2) ->
               Eop ((Oorimm (Int.coq_or n1 n2)), (Econs (t2, Enil)))
             | Coq_orimm_default e3 -> Eop ((Oorimm n1), (Econs (e3, Enil))))

(** val same_expr_pure : expr -> expr -> bool **)

let same_expr_pure e1 e2 =
  match e1 with
  | Evar v1 ->
    (match e2 with
     | Evar v2 -> if ident_eq v1 v2 then true else false
     | _ -> false)
  | _ -> false

type or_cases =
| Coq_or_case1 of Int.int * expr
| Coq_or_case2 of expr * Int.int
| Coq_or_case3 of Int.int * expr * Int.int * expr
| Coq_or_case4 of Int.int * expr * Int.int * expr
| Coq_or_default of expr * expr

(** val or_match : expr -> expr -> or_cases **)

let or_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_or_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_or_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | x -> Coq_or_default (e3, (Eop (x, e))))
     | x -> Coq_or_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Ointconst n1 ->
       (match e with
        | Enil -> Coq_or_case1 (n1, e2)
        | Econs (e0, e3) ->
          let e4 = Eop ((Ointconst n1), (Econs (e0, e3))) in
          (match e2 with
           | Eop (o0, e5) ->
             (match o0 with
              | Ointconst n2 ->
                (match e5 with
                 | Enil -> Coq_or_case2 (e4, n2)
                 | Econs (e6, e7) ->
                   Coq_or_default (e4, (Eop ((Ointconst n2), (Econs (e6,
                     e7))))))
              | x -> Coq_or_default (e4, (Eop (x, e5))))
           | x -> Coq_or_default (e4, x)))
     | Oshlimm n1 ->
       (match e with
        | Enil ->
          let e3 = Eop ((Oshlimm n1), Enil) in
          (match e2 with
           | Eop (o0, e0) ->
             (match o0 with
              | Ointconst n2 ->
                (match e0 with
                 | Enil -> Coq_or_case2 (e3, n2)
                 | Econs (e4, e5) ->
                   Coq_or_default (e3, (Eop ((Ointconst n2), (Econs (e4,
                     e5))))))
              | x -> Coq_or_default (e3, (Eop (x, e0))))
           | x -> Coq_or_default (e3, x))
        | Econs (t1, e0) ->
          (match e0 with
           | Enil ->
             let e3 = Eop ((Oshlimm n1), (Econs (t1, Enil))) in
             (match e2 with
              | Eop (o0, e4) ->
                (match o0 with
                 | Ointconst n2 ->
                   (match e4 with
                    | Enil -> Coq_or_case2 (e3, n2)
                    | Econs (e5, e6) ->
                      Coq_or_default (e3, (Eop ((Ointconst n2), (Econs (e5,
                        e6))))))
                 | Oshruimm n2 ->
                   (match e4 with
                    | Enil -> Coq_or_default (e3, (Eop ((Oshruimm n2), Enil)))
                    | Econs (t2, e5) ->
                      (match e5 with
                       | Enil -> Coq_or_case3 (n1, t1, n2, t2)
                       | Econs (e6, e7) ->
                         Coq_or_default (e3, (Eop ((Oshruimm n2), (Econs (t2,
                           (Econs (e6, e7)))))))))
                 | x -> Coq_or_default (e3, (Eop (x, e4))))
              | x -> Coq_or_default (e3, x))
           | Econs (e3, e4) ->
             let e5 = Eop ((Oshlimm n1), (Econs (t1, (Econs (e3, e4))))) in
             (match e2 with
              | Eop (o0, e6) ->
                (match o0 with
                 | Ointconst n2 ->
                   (match e6 with
                    | Enil -> Coq_or_case2 (e5, n2)
                    | Econs (e7, e8) ->
                      Coq_or_default (e5, (Eop ((Ointconst n2), (Econs (e7,
                        e8))))))
                 | x -> Coq_or_default (e5, (Eop (x, e6))))
              | x -> Coq_or_default (e5, x))))
     | Oshruimm n2 ->
       (match e with
        | Enil ->
          let e3 = Eop ((Oshruimm n2), Enil) in
          (match e2 with
           | Eop (o0, e0) ->
             (match o0 with
              | Ointconst n3 ->
                (match e0 with
                 | Enil -> Coq_or_case2 (e3, n3)
                 | Econs (e4, e5) ->
                   Coq_or_default (e3, (Eop ((Ointconst n3), (Econs (e4,
                     e5))))))
              | x -> Coq_or_default (e3, (Eop (x, e0))))
           | x -> Coq_or_default (e3, x))
        | Econs (t2, e0) ->
          (match e0 with
           | Enil ->
             let e3 = Eop ((Oshruimm n2), (Econs (t2, Enil))) in
             (match e2 with
              | Eop (o0, e4) ->
                (match o0 with
                 | Ointconst n3 ->
                   (match e4 with
                    | Enil -> Coq_or_case2 (e3, n3)
                    | Econs (e5, e6) ->
                      Coq_or_default (e3, (Eop ((Ointconst n3), (Econs (e5,
                        e6))))))
                 | Oshlimm n1 ->
                   (match e4 with
                    | Enil -> Coq_or_default (e3, (Eop ((Oshlimm n1), Enil)))
                    | Econs (t1, e5) ->
                      (match e5 with
                       | Enil -> Coq_or_case4 (n2, t2, n1, t1)
                       | Econs (e6, e7) ->
                         Coq_or_default (e3, (Eop ((Oshlimm n1), (Econs (t1,
                           (Econs (e6, e7)))))))))
                 | x -> Coq_or_default (e3, (Eop (x, e4))))
              | x -> Coq_or_default (e3, x))
           | Econs (e3, e4) ->
             let e5 = Eop ((Oshruimm n2), (Econs (t2, (Econs (e3, e4))))) in
             (match e2 with
              | Eop (o0, e6) ->
                (match o0 with
                 | Ointconst n3 ->
                   (match e6 with
                    | Enil -> Coq_or_case2 (e5, n3)
                    | Econs (e7, e8) ->
                      Coq_or_default (e5, (Eop ((Ointconst n3), (Econs (e7,
                        e8))))))
                 | x -> Coq_or_default (e5, (Eop (x, e6))))
              | x -> Coq_or_default (e5, x))))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Ointconst n2 ->
             (match e0 with
              | Enil -> Coq_or_case2 (e3, n2)
              | Econs (e4, e5) ->
                Coq_or_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
           | x0 -> Coq_or_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_or_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_or_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_or_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x -> Coq_or_default (e3, (Eop (x, e0))))
     | x -> Coq_or_default (e3, x))
  | Eletvar n ->
    let e3 = Eletvar n in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_or_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_or_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | x -> Coq_or_default (e3, (Eop (x, e))))
     | x -> Coq_or_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_or_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_or_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x -> Coq_or_default (e3, (Eop (x, e0))))
     | x -> Coq_or_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Ointconst n2 ->
          (match e3 with
           | Enil -> Coq_or_case2 (x, n2)
           | Econs (e4, e5) ->
             Coq_or_default (x, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x0 -> Coq_or_default (x, (Eop (x0, e3))))
     | x0 -> Coq_or_default (x, x0))

(** val coq_or : expr -> expr -> expr **)

let coq_or e1 e2 =
  match or_match e1 e2 with
  | Coq_or_case1 (n1, t2) -> orimm n1 t2
  | Coq_or_case2 (t1, n2) -> orimm n2 t1
  | Coq_or_case3 (n1, t1, n2, t2) ->
    if Int.eq (Int.add n1 n2) Int.iwordsize
    then if same_expr_pure t1 t2
         then Eop ((Ororimm n2), (Econs (t1, Enil)))
         else Eop ((Oshldimm n1), (Econs (t1, (Econs (t2, Enil)))))
    else Eop (Oor, (Econs (e1, (Econs (e2, Enil)))))
  | Coq_or_case4 (n2, t2, n1, t1) ->
    if Int.eq (Int.add n1 n2) Int.iwordsize
    then if same_expr_pure t1 t2
         then Eop ((Ororimm n2), (Econs (t1, Enil)))
         else Eop ((Oshldimm n1), (Econs (t1, (Econs (t2, Enil)))))
    else Eop (Oor, (Econs (e1, (Econs (e2, Enil)))))
  | Coq_or_default (e3, e4) -> Eop (Oor, (Econs (e3, (Econs (e4, Enil)))))

type xorimm_cases =
| Coq_xorimm_case1 of Int.int
| Coq_xorimm_case2 of Int.int * expr
| Coq_xorimm_case3 of expr
| Coq_xorimm_default of expr

(** val xorimm_match : expr -> xorimm_cases **)

let xorimm_match = function
| Eop (o, e) ->
  (match o with
   | Ointconst n2 ->
     (match e with
      | Enil -> Coq_xorimm_case1 n2
      | Econs (e0, e1) ->
        Coq_xorimm_default (Eop ((Ointconst n2), (Econs (e0, e1)))))
   | Oxorimm n2 ->
     (match e with
      | Enil -> Coq_xorimm_default (Eop ((Oxorimm n2), Enil))
      | Econs (t2, e0) ->
        (match e0 with
         | Enil -> Coq_xorimm_case2 (n2, t2)
         | Econs (e1, e3) ->
           Coq_xorimm_default (Eop ((Oxorimm n2), (Econs (t2, (Econs (e1,
             e3))))))))
   | Onot ->
     (match e with
      | Enil -> Coq_xorimm_default (Eop (Onot, Enil))
      | Econs (t2, e0) ->
        (match e0 with
         | Enil -> Coq_xorimm_case3 t2
         | Econs (e1, e3) ->
           Coq_xorimm_default (Eop (Onot, (Econs (t2, (Econs (e1, e3))))))))
   | x -> Coq_xorimm_default (Eop (x, e)))
| x -> Coq_xorimm_default x

(** val xorimm : Int.int -> expr -> expr **)

let xorimm n1 e2 =
  if Int.eq n1 Int.zero
  then e2
  else (match xorimm_match e2 with
        | Coq_xorimm_case1 n2 -> Eop ((Ointconst (Int.xor n1 n2)), Enil)
        | Coq_xorimm_case2 (n2, t2) ->
          Eop ((Oxorimm (Int.xor n1 n2)), (Econs (t2, Enil)))
        | Coq_xorimm_case3 t2 ->
          Eop ((Oxorimm (Int.not n1)), (Econs (t2, Enil)))
        | Coq_xorimm_default e3 -> Eop ((Oxorimm n1), (Econs (e3, Enil))))

type xor_cases =
| Coq_xor_case1 of Int.int * expr
| Coq_xor_case2 of expr * Int.int
| Coq_xor_default of expr * expr

(** val xor_match : expr -> expr -> xor_cases **)

let xor_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_xor_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_xor_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | x -> Coq_xor_default (e3, (Eop (x, e))))
     | x -> Coq_xor_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Ointconst n1 ->
       (match e with
        | Enil -> Coq_xor_case1 (n1, e2)
        | Econs (e0, e3) ->
          let e4 = Eop ((Ointconst n1), (Econs (e0, e3))) in
          (match e2 with
           | Eop (o0, e5) ->
             (match o0 with
              | Ointconst n2 ->
                (match e5 with
                 | Enil -> Coq_xor_case2 (e4, n2)
                 | Econs (e6, e7) ->
                   Coq_xor_default (e4, (Eop ((Ointconst n2), (Econs (e6,
                     e7))))))
              | x -> Coq_xor_default (e4, (Eop (x, e5))))
           | x -> Coq_xor_default (e4, x)))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Ointconst n2 ->
             (match e0 with
              | Enil -> Coq_xor_case2 (e3, n2)
              | Econs (e4, e5) ->
                Coq_xor_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
           | x0 -> Coq_xor_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_xor_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_xor_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_xor_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x -> Coq_xor_default (e3, (Eop (x, e0))))
     | x -> Coq_xor_default (e3, x))
  | Eletvar n ->
    let e3 = Eletvar n in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_xor_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_xor_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | x -> Coq_xor_default (e3, (Eop (x, e))))
     | x -> Coq_xor_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_xor_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_xor_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x -> Coq_xor_default (e3, (Eop (x, e0))))
     | x -> Coq_xor_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Ointconst n2 ->
          (match e3 with
           | Enil -> Coq_xor_case2 (x, n2)
           | Econs (e4, e5) ->
             Coq_xor_default (x, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x0 -> Coq_xor_default (x, (Eop (x0, e3))))
     | x0 -> Coq_xor_default (x, x0))

(** val xor : expr -> expr -> expr **)

let xor e1 e2 =
  match xor_match e1 e2 with
  | Coq_xor_case1 (n1, t2) -> xorimm n1 t2
  | Coq_xor_case2 (t1, n2) -> xorimm n2 t1
  | Coq_xor_default (e3, e4) -> Eop (Oxor, (Econs (e3, (Econs (e4, Enil)))))

(** val divu_base : expr -> expr -> expr **)

let divu_base e1 e2 =
  Eop (Odivu, (Econs (e1, (Econs (e2, Enil)))))

(** val modu_base : expr -> expr -> expr **)

let modu_base e1 e2 =
  Eop (Omodu, (Econs (e1, (Econs (e2, Enil)))))

(** val divs_base : expr -> expr -> expr **)

let divs_base e1 e2 =
  Eop (Odiv, (Econs (e1, (Econs (e2, Enil)))))

(** val mods_base : expr -> expr -> expr **)

let mods_base e1 e2 =
  Eop (Omod, (Econs (e1, (Econs (e2, Enil)))))

(** val shrximm : expr -> Int.int -> expr **)

let shrximm e1 n2 =
  if Int.eq n2 Int.zero then e1 else Eop ((Oshrximm n2), (Econs (e1, Enil)))

type shl_cases =
| Coq_shl_case1 of Int.int
| Coq_shl_default of expr

(** val shl_match : expr -> shl_cases **)

let shl_match = function
| Eop (o, e) ->
  (match o with
   | Ointconst n2 ->
     (match e with
      | Enil -> Coq_shl_case1 n2
      | Econs (e0, e1) ->
        Coq_shl_default (Eop ((Ointconst n2), (Econs (e0, e1)))))
   | x -> Coq_shl_default (Eop (x, e)))
| x -> Coq_shl_default x

(** val shl : expr -> expr -> expr **)

let shl e1 e2 =
  match shl_match e2 with
  | Coq_shl_case1 n2 -> shlimm e1 n2
  | Coq_shl_default e3 -> Eop (Oshl, (Econs (e1, (Econs (e3, Enil)))))

type shr_cases =
| Coq_shr_case1 of Int.int
| Coq_shr_default of expr

(** val shr_match : expr -> shr_cases **)

let shr_match = function
| Eop (o, e) ->
  (match o with
   | Ointconst n2 ->
     (match e with
      | Enil -> Coq_shr_case1 n2
      | Econs (e0, e1) ->
        Coq_shr_default (Eop ((Ointconst n2), (Econs (e0, e1)))))
   | x -> Coq_shr_default (Eop (x, e)))
| x -> Coq_shr_default x

(** val shr : expr -> expr -> expr **)

let shr e1 e2 =
  match shr_match e2 with
  | Coq_shr_case1 n2 -> shrimm e1 n2
  | Coq_shr_default e3 -> Eop (Oshr, (Econs (e1, (Econs (e3, Enil)))))

type shru_cases =
| Coq_shru_case1 of Int.int
| Coq_shru_default of expr

(** val shru_match : expr -> shru_cases **)

let shru_match = function
| Eop (o, e) ->
  (match o with
   | Ointconst n2 ->
     (match e with
      | Enil -> Coq_shru_case1 n2
      | Econs (e0, e1) ->
        Coq_shru_default (Eop ((Ointconst n2), (Econs (e0, e1)))))
   | x -> Coq_shru_default (Eop (x, e)))
| x -> Coq_shru_default x

(** val shru : expr -> expr -> expr **)

let shru e1 e2 =
  match shru_match e2 with
  | Coq_shru_case1 n2 -> shruimm e1 n2
  | Coq_shru_default e3 -> Eop (Oshru, (Econs (e1, (Econs (e3, Enil)))))

(** val negf : expr -> expr **)

let negf e =
  Eop (Onegf, (Econs (e, Enil)))

(** val absf : expr -> expr **)

let absf e =
  Eop (Oabsf, (Econs (e, Enil)))

(** val addf : expr -> expr -> expr **)

let addf e1 e2 =
  Eop (Oaddf, (Econs (e1, (Econs (e2, Enil)))))

(** val subf : expr -> expr -> expr **)

let subf e1 e2 =
  Eop (Osubf, (Econs (e1, (Econs (e2, Enil)))))

(** val mulf : expr -> expr -> expr **)

let mulf e1 e2 =
  Eop (Omulf, (Econs (e1, (Econs (e2, Enil)))))

(** val negfs : expr -> expr **)

let negfs e =
  Eop (Onegfs, (Econs (e, Enil)))

(** val absfs : expr -> expr **)

let absfs e =
  Eop (Oabsfs, (Econs (e, Enil)))

(** val addfs : expr -> expr -> expr **)

let addfs e1 e2 =
  Eop (Oaddfs, (Econs (e1, (Econs (e2, Enil)))))

(** val subfs : expr -> expr -> expr **)

let subfs e1 e2 =
  Eop (Osubfs, (Econs (e1, (Econs (e2, Enil)))))

(** val mulfs : expr -> expr -> expr **)

let mulfs e1 e2 =
  Eop (Omulfs, (Econs (e1, (Econs (e2, Enil)))))

type compimm_cases =
| Coq_compimm_case1 of comparison * Int.int
| Coq_compimm_case2 of condition * exprlist
| Coq_compimm_case3 of condition * exprlist
| Coq_compimm_case4 of Int.int * expr
| Coq_compimm_case5 of Int.int * expr
| Coq_compimm_default of comparison * expr

(** val compimm_match : comparison -> expr -> compimm_cases **)

let compimm_match c e1 =
  match c with
  | Ceq ->
    let c0 = Ceq in
    (match e1 with
     | Eop (o, el) ->
       (match o with
        | Ointconst n1 ->
          (match el with
           | Enil -> Coq_compimm_case1 (c0, n1)
           | Econs (e, e0) ->
             Coq_compimm_default (c0, (Eop ((Ointconst n1), (Econs (e, e0))))))
        | Oandimm n1 ->
          (match el with
           | Enil -> Coq_compimm_default (c0, (Eop ((Oandimm n1), Enil)))
           | Econs (t1, e) ->
             (match e with
              | Enil -> Coq_compimm_case4 (n1, t1)
              | Econs (e0, e2) ->
                Coq_compimm_default (c0, (Eop ((Oandimm n1), (Econs (t1,
                  (Econs (e0, e2)))))))))
        | Ocmp c1 -> Coq_compimm_case2 (c1, el)
        | x -> Coq_compimm_default (c0, (Eop (x, el))))
     | x -> Coq_compimm_default (c0, x))
  | Cne ->
    let c0 = Cne in
    (match e1 with
     | Eop (o, el) ->
       (match o with
        | Ointconst n1 ->
          (match el with
           | Enil -> Coq_compimm_case1 (c0, n1)
           | Econs (e, e0) ->
             Coq_compimm_default (c0, (Eop ((Ointconst n1), (Econs (e, e0))))))
        | Oandimm n1 ->
          (match el with
           | Enil -> Coq_compimm_default (c0, (Eop ((Oandimm n1), Enil)))
           | Econs (t1, e) ->
             (match e with
              | Enil -> Coq_compimm_case5 (n1, t1)
              | Econs (e0, e2) ->
                Coq_compimm_default (c0, (Eop ((Oandimm n1), (Econs (t1,
                  (Econs (e0, e2)))))))))
        | Ocmp c1 -> Coq_compimm_case3 (c1, el)
        | x -> Coq_compimm_default (c0, (Eop (x, el))))
     | x -> Coq_compimm_default (c0, x))
  | x ->
    (match e1 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n1 ->
          (match e with
           | Enil -> Coq_compimm_case1 (x, n1)
           | Econs (e0, e2) ->
             Coq_compimm_default (x, (Eop ((Ointconst n1), (Econs (e0, e2))))))
        | x0 -> Coq_compimm_default (x, (Eop (x0, e))))
     | x0 -> Coq_compimm_default (x, x0))

(** val compimm :
    (comparison -> Int.int -> condition) -> (comparison -> Int.int -> Int.int
    -> bool) -> comparison -> expr -> Int.int -> expr **)

let compimm default sem c e1 n2 =
  match compimm_match c e1 with
  | Coq_compimm_case1 (c0, n1) ->
    Eop ((Ointconst (if sem c0 n1 n2 then Int.one else Int.zero)), Enil)
  | Coq_compimm_case2 (c0, el) ->
    if Int.eq_dec n2 Int.zero
    then Eop ((Ocmp (negate_condition c0)), el)
    else if Int.eq_dec n2 Int.one
         then Eop ((Ocmp c0), el)
         else Eop ((Ointconst Int.zero), Enil)
  | Coq_compimm_case3 (c0, el) ->
    if Int.eq_dec n2 Int.zero
    then Eop ((Ocmp c0), el)
    else if Int.eq_dec n2 Int.one
         then Eop ((Ocmp (negate_condition c0)), el)
         else Eop ((Ointconst Int.one), Enil)
  | Coq_compimm_case4 (n1, t1) ->
    if Int.eq_dec n2 Int.zero
    then Eop ((Ocmp (Cmaskzero n1)), (Econs (t1, Enil)))
    else Eop ((Ocmp (default c n2)), (Econs (e1, Enil)))
  | Coq_compimm_case5 (n1, t1) ->
    if Int.eq_dec n2 Int.zero
    then Eop ((Ocmp (Cmasknotzero n1)), (Econs (t1, Enil)))
    else Eop ((Ocmp (default c n2)), (Econs (e1, Enil)))
  | Coq_compimm_default (c0, e2) ->
    Eop ((Ocmp (default c0 n2)), (Econs (e2, Enil)))

type comp_cases =
| Coq_comp_case1 of Int.int * expr
| Coq_comp_case2 of expr * Int.int
| Coq_comp_default of expr * expr

(** val comp_match : expr -> expr -> comp_cases **)

let comp_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_comp_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_comp_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | x -> Coq_comp_default (e3, (Eop (x, e))))
     | x -> Coq_comp_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Ointconst n1 ->
       (match e with
        | Enil -> Coq_comp_case1 (n1, e2)
        | Econs (e0, e3) ->
          let e4 = Eop ((Ointconst n1), (Econs (e0, e3))) in
          (match e2 with
           | Eop (o0, e5) ->
             (match o0 with
              | Ointconst n2 ->
                (match e5 with
                 | Enil -> Coq_comp_case2 (e4, n2)
                 | Econs (e6, e7) ->
                   Coq_comp_default (e4, (Eop ((Ointconst n2), (Econs (e6,
                     e7))))))
              | x -> Coq_comp_default (e4, (Eop (x, e5))))
           | x -> Coq_comp_default (e4, x)))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Ointconst n2 ->
             (match e0 with
              | Enil -> Coq_comp_case2 (e3, n2)
              | Econs (e4, e5) ->
                Coq_comp_default (e3, (Eop ((Ointconst n2), (Econs (e4,
                  e5))))))
           | x0 -> Coq_comp_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_comp_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_comp_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_comp_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x -> Coq_comp_default (e3, (Eop (x, e0))))
     | x -> Coq_comp_default (e3, x))
  | Eletvar n ->
    let e3 = Eletvar n in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_comp_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_comp_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | x -> Coq_comp_default (e3, (Eop (x, e))))
     | x -> Coq_comp_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_comp_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_comp_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x -> Coq_comp_default (e3, (Eop (x, e0))))
     | x -> Coq_comp_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Ointconst n2 ->
          (match e3 with
           | Enil -> Coq_comp_case2 (x, n2)
           | Econs (e4, e5) ->
             Coq_comp_default (x, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x0 -> Coq_comp_default (x, (Eop (x0, e3))))
     | x0 -> Coq_comp_default (x, x0))

(** val comp : comparison -> expr -> expr -> expr **)

let comp c e1 e2 =
  match comp_match e1 e2 with
  | Coq_comp_case1 (n1, t2) ->
    compimm (fun x x0 -> Ccompimm (x, x0)) Int.cmp (swap_comparison c) t2 n1
  | Coq_comp_case2 (t1, n2) ->
    compimm (fun x x0 -> Ccompimm (x, x0)) Int.cmp c t1 n2
  | Coq_comp_default (e3, e4) ->
    Eop ((Ocmp (Ccomp c)), (Econs (e3, (Econs (e4, Enil)))))

type compu_cases =
| Coq_compu_case1 of Int.int * expr
| Coq_compu_case2 of expr * Int.int
| Coq_compu_default of expr * expr

(** val compu_match : expr -> expr -> compu_cases **)

let compu_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_compu_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_compu_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | x -> Coq_compu_default (e3, (Eop (x, e))))
     | x -> Coq_compu_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Ointconst n1 ->
       (match e with
        | Enil -> Coq_compu_case1 (n1, e2)
        | Econs (e0, e3) ->
          let e4 = Eop ((Ointconst n1), (Econs (e0, e3))) in
          (match e2 with
           | Eop (o0, e5) ->
             (match o0 with
              | Ointconst n2 ->
                (match e5 with
                 | Enil -> Coq_compu_case2 (e4, n2)
                 | Econs (e6, e7) ->
                   Coq_compu_default (e4, (Eop ((Ointconst n2), (Econs (e6,
                     e7))))))
              | x -> Coq_compu_default (e4, (Eop (x, e5))))
           | x -> Coq_compu_default (e4, x)))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Ointconst n2 ->
             (match e0 with
              | Enil -> Coq_compu_case2 (e3, n2)
              | Econs (e4, e5) ->
                Coq_compu_default (e3, (Eop ((Ointconst n2), (Econs (e4,
                  e5))))))
           | x0 -> Coq_compu_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_compu_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_compu_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_compu_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x -> Coq_compu_default (e3, (Eop (x, e0))))
     | x -> Coq_compu_default (e3, x))
  | Eletvar n ->
    let e3 = Eletvar n in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Ointconst n2 ->
          (match e with
           | Enil -> Coq_compu_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_compu_default (e3, (Eop ((Ointconst n2), (Econs (e0, e4))))))
        | x -> Coq_compu_default (e3, (Eop (x, e))))
     | x -> Coq_compu_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Ointconst n2 ->
          (match e0 with
           | Enil -> Coq_compu_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_compu_default (e3, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x -> Coq_compu_default (e3, (Eop (x, e0))))
     | x -> Coq_compu_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Ointconst n2 ->
          (match e3 with
           | Enil -> Coq_compu_case2 (x, n2)
           | Econs (e4, e5) ->
             Coq_compu_default (x, (Eop ((Ointconst n2), (Econs (e4, e5))))))
        | x0 -> Coq_compu_default (x, (Eop (x0, e3))))
     | x0 -> Coq_compu_default (x, x0))

(** val compu : comparison -> expr -> expr -> expr **)

let compu c e1 e2 =
  match compu_match e1 e2 with
  | Coq_compu_case1 (n1, t2) ->
    compimm (fun x x0 -> Ccompuimm (x, x0)) Int.cmpu (swap_comparison c) t2 n1
  | Coq_compu_case2 (t1, n2) ->
    compimm (fun x x0 -> Ccompuimm (x, x0)) Int.cmpu c t1 n2
  | Coq_compu_default (e3, e4) ->
    Eop ((Ocmp (Ccompu c)), (Econs (e3, (Econs (e4, Enil)))))

(** val compf : comparison -> expr -> expr -> expr **)

let compf c e1 e2 =
  Eop ((Ocmp (Ccompf c)), (Econs (e1, (Econs (e2, Enil)))))

(** val compfs : comparison -> expr -> expr -> expr **)

let compfs c e1 e2 =
  Eop ((Ocmp (Ccompfs c)), (Econs (e1, (Econs (e2, Enil)))))

type cast8unsigned_cases =
| Coq_cast8unsigned_case1 of Int.int
| Coq_cast8unsigned_case2 of Int.int * expr
| Coq_cast8unsigned_default of expr

(** val cast8unsigned_match : expr -> cast8unsigned_cases **)

let cast8unsigned_match = function
| Eop (o, e0) ->
  (match o with
   | Ointconst n ->
     (match e0 with
      | Enil -> Coq_cast8unsigned_case1 n
      | Econs (e1, e2) ->
        Coq_cast8unsigned_default (Eop ((Ointconst n), (Econs (e1, e2)))))
   | Oandimm n ->
     (match e0 with
      | Enil -> Coq_cast8unsigned_default (Eop ((Oandimm n), Enil))
      | Econs (t, e1) ->
        (match e1 with
         | Enil -> Coq_cast8unsigned_case2 (n, t)
         | Econs (e2, e3) ->
           Coq_cast8unsigned_default (Eop ((Oandimm n), (Econs (t, (Econs
             (e2, e3))))))))
   | x -> Coq_cast8unsigned_default (Eop (x, e0)))
| x -> Coq_cast8unsigned_default x

(** val cast8unsigned : expr -> expr **)

let cast8unsigned e =
  match cast8unsigned_match e with
  | Coq_cast8unsigned_case1 n ->
    Eop ((Ointconst
      (Int.zero_ext (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))) n)), Enil)
  | Coq_cast8unsigned_case2 (n, t) ->
    andimm
      (Int.coq_and
        (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
          (Coq_xI Coq_xH))))))))) n) t
  | Coq_cast8unsigned_default e0 -> Eop (Ocast8unsigned, (Econs (e0, Enil)))

type cast8signed_cases =
| Coq_cast8signed_case1 of Int.int
| Coq_cast8signed_default of expr

(** val cast8signed_match : expr -> cast8signed_cases **)

let cast8signed_match = function
| Eop (o, e0) ->
  (match o with
   | Ointconst n ->
     (match e0 with
      | Enil -> Coq_cast8signed_case1 n
      | Econs (e1, e2) ->
        Coq_cast8signed_default (Eop ((Ointconst n), (Econs (e1, e2)))))
   | x -> Coq_cast8signed_default (Eop (x, e0)))
| x -> Coq_cast8signed_default x

(** val cast8signed : expr -> expr **)

let cast8signed e =
  match cast8signed_match e with
  | Coq_cast8signed_case1 n ->
    Eop ((Ointconst
      (Int.sign_ext (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))) n)), Enil)
  | Coq_cast8signed_default e0 -> Eop (Ocast8signed, (Econs (e0, Enil)))

type cast16unsigned_cases =
| Coq_cast16unsigned_case1 of Int.int
| Coq_cast16unsigned_case2 of Int.int * expr
| Coq_cast16unsigned_default of expr

(** val cast16unsigned_match : expr -> cast16unsigned_cases **)

let cast16unsigned_match = function
| Eop (o, e0) ->
  (match o with
   | Ointconst n ->
     (match e0 with
      | Enil -> Coq_cast16unsigned_case1 n
      | Econs (e1, e2) ->
        Coq_cast16unsigned_default (Eop ((Ointconst n), (Econs (e1, e2)))))
   | Oandimm n ->
     (match e0 with
      | Enil -> Coq_cast16unsigned_default (Eop ((Oandimm n), Enil))
      | Econs (t, e1) ->
        (match e1 with
         | Enil -> Coq_cast16unsigned_case2 (n, t)
         | Econs (e2, e3) ->
           Coq_cast16unsigned_default (Eop ((Oandimm n), (Econs (t, (Econs
             (e2, e3))))))))
   | x -> Coq_cast16unsigned_default (Eop (x, e0)))
| x -> Coq_cast16unsigned_default x

(** val cast16unsigned : expr -> expr **)

let cast16unsigned e =
  match cast16unsigned_match e with
  | Coq_cast16unsigned_case1 n ->
    Eop ((Ointconst
      (Int.zero_ext (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))) n)),
      Enil)
  | Coq_cast16unsigned_case2 (n, t) ->
    andimm
      (Int.coq_and
        (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
          (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
          (Coq_xI Coq_xH))))))))))))))))) n) t
  | Coq_cast16unsigned_default e0 -> Eop (Ocast16unsigned, (Econs (e0, Enil)))

type cast16signed_cases =
| Coq_cast16signed_case1 of Int.int
| Coq_cast16signed_default of expr

(** val cast16signed_match : expr -> cast16signed_cases **)

let cast16signed_match = function
| Eop (o, e0) ->
  (match o with
   | Ointconst n ->
     (match e0 with
      | Enil -> Coq_cast16signed_case1 n
      | Econs (e1, e2) ->
        Coq_cast16signed_default (Eop ((Ointconst n), (Econs (e1, e2)))))
   | x -> Coq_cast16signed_default (Eop (x, e0)))
| x -> Coq_cast16signed_default x

(** val cast16signed : expr -> expr **)

let cast16signed e =
  match cast16signed_match e with
  | Coq_cast16signed_case1 n ->
    Eop ((Ointconst
      (Int.sign_ext (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))) n)),
      Enil)
  | Coq_cast16signed_default e0 -> Eop (Ocast16signed, (Econs (e0, Enil)))

(** val select_supported : typ -> bool **)

let select_supported = function
| Tint -> true
| Tlong -> ptr64
| _ -> false

(** val select_swap : condition -> bool **)

let select_swap = function
| Ccompf c -> (match c with
               | Cne -> true
               | _ -> false)
| Cnotcompf c -> (match c with
                  | Ceq -> true
                  | _ -> false)
| Ccompfs c -> (match c with
                | Cne -> true
                | _ -> false)
| Cnotcompfs c -> (match c with
                   | Ceq -> true
                   | _ -> false)
| _ -> false

(** val select :
    typ -> condition -> exprlist -> expr -> expr -> expr option **)

let select ty cond args e1 e2 =
  if select_supported ty
  then if select_swap cond
       then Some (Eop ((Osel ((negate_condition cond), ty)), (Econs (e2,
              (Econs (e1, args))))))
       else Some (Eop ((Osel (cond, ty)), (Econs (e1, (Econs (e2, args))))))
  else None

(** val singleoffloat : expr -> expr **)

let singleoffloat e =
  Eop (Osingleoffloat, (Econs (e, Enil)))

(** val floatofsingle : expr -> expr **)

let floatofsingle e =
  Eop (Ofloatofsingle, (Econs (e, Enil)))

(** val intoffloat : expr -> expr **)

let intoffloat e =
  Eop (Ointoffloat, (Econs (e, Enil)))

type floatofint_cases =
| Coq_floatofint_case1 of Int.int
| Coq_floatofint_default of expr

(** val floatofint_match : expr -> floatofint_cases **)

let floatofint_match = function
| Eop (o, e0) ->
  (match o with
   | Ointconst n ->
     (match e0 with
      | Enil -> Coq_floatofint_case1 n
      | Econs (e1, e2) ->
        Coq_floatofint_default (Eop ((Ointconst n), (Econs (e1, e2)))))
   | x -> Coq_floatofint_default (Eop (x, e0)))
| x -> Coq_floatofint_default x

(** val floatofint : expr -> expr **)

let floatofint e =
  match floatofint_match e with
  | Coq_floatofint_case1 n -> Eop ((Ofloatconst (Float.of_int n)), Enil)
  | Coq_floatofint_default e0 -> Eop (Ofloatofint, (Econs (e0, Enil)))

(** val intuoffloat : expr -> expr **)

let intuoffloat e =
  if splitlong
  then Elet (e, (Elet ((Eop ((Ofloatconst (Float.of_intu Float.ox8000_0000)),
         Enil)), (Econdition ((CEcond ((Ccompf Clt), (Econs ((Eletvar (S O)),
         (Econs ((Eletvar O), Enil)))))), (intoffloat (Eletvar (S O))),
         (addimm Float.ox8000_0000
           (intoffloat (subf (Eletvar (S O)) (Eletvar O)))))))))
  else Eop (Olowlong, (Econs ((Eop (Olongoffloat, (Econs (e, Enil)))), Enil)))

type floatofintu_cases =
| Coq_floatofintu_case1 of Int.int
| Coq_floatofintu_default of expr

(** val floatofintu_match : expr -> floatofintu_cases **)

let floatofintu_match = function
| Eop (o, e0) ->
  (match o with
   | Ointconst n ->
     (match e0 with
      | Enil -> Coq_floatofintu_case1 n
      | Econs (e1, e2) ->
        Coq_floatofintu_default (Eop ((Ointconst n), (Econs (e1, e2)))))
   | x -> Coq_floatofintu_default (Eop (x, e0)))
| x -> Coq_floatofintu_default x

(** val floatofintu : expr -> expr **)

let floatofintu e =
  match floatofintu_match e with
  | Coq_floatofintu_case1 n -> Eop ((Ofloatconst (Float.of_intu n)), Enil)
  | Coq_floatofintu_default e0 ->
    if splitlong
    then let f = Eop ((Ofloatconst (Float.of_intu Float.ox8000_0000)), Enil)
         in
         Elet (e0, (Econdition ((CEcond ((Ccompuimm (Clt,
         Float.ox8000_0000)), (Econs ((Eletvar O), Enil)))),
         (floatofint (Eletvar O)),
         (addf (floatofint (addimm (Int.neg Float.ox8000_0000) (Eletvar O)))
           f))))
    else Eop (Ofloatoflong, (Econs ((Eop (Ocast32unsigned, (Econs (e0,
           Enil)))), Enil)))

(** val intofsingle : expr -> expr **)

let intofsingle e =
  Eop (Ointofsingle, (Econs (e, Enil)))

type singleofint_cases =
| Coq_singleofint_case1 of Int.int
| Coq_singleofint_default of expr

(** val singleofint_match : expr -> singleofint_cases **)

let singleofint_match = function
| Eop (o, e0) ->
  (match o with
   | Ointconst n ->
     (match e0 with
      | Enil -> Coq_singleofint_case1 n
      | Econs (e1, e2) ->
        Coq_singleofint_default (Eop ((Ointconst n), (Econs (e1, e2)))))
   | x -> Coq_singleofint_default (Eop (x, e0)))
| x -> Coq_singleofint_default x

(** val singleofint : expr -> expr **)

let singleofint e =
  match singleofint_match e with
  | Coq_singleofint_case1 n -> Eop ((Osingleconst (Float32.of_int n)), Enil)
  | Coq_singleofint_default e0 -> Eop (Osingleofint, (Econs (e0, Enil)))

(** val intuofsingle : expr -> expr **)

let intuofsingle e =
  intuoffloat (floatofsingle e)

type singleofintu_cases =
| Coq_singleofintu_case1 of Int.int
| Coq_singleofintu_default of expr

(** val singleofintu_match : expr -> singleofintu_cases **)

let singleofintu_match = function
| Eop (o, e0) ->
  (match o with
   | Ointconst n ->
     (match e0 with
      | Enil -> Coq_singleofintu_case1 n
      | Econs (e1, e2) ->
        Coq_singleofintu_default (Eop ((Ointconst n), (Econs (e1, e2)))))
   | x -> Coq_singleofintu_default (Eop (x, e0)))
| x -> Coq_singleofintu_default x

(** val singleofintu : expr -> expr **)

let singleofintu e =
  match singleofintu_match e with
  | Coq_singleofintu_case1 n -> Eop ((Osingleconst (Float32.of_intu n)), Enil)
  | Coq_singleofintu_default e0 -> singleoffloat (floatofintu e0)

type addressing_cases =
| Coq_addressing_case1 of addressing * exprlist
| Coq_addressing_case2 of addressing * exprlist
| Coq_addressing_default of expr

(** val addressing_match : expr -> addressing_cases **)

let addressing_match = function
| Eop (o, args) ->
  (match o with
   | Olea addr -> Coq_addressing_case1 (addr, args)
   | Oleal addr -> Coq_addressing_case2 (addr, args)
   | x -> Coq_addressing_default (Eop (x, args)))
| x -> Coq_addressing_default x

(** val addressing : memory_chunk -> expr -> addressing * exprlist **)

let addressing _ e =
  match addressing_match e with
  | Coq_addressing_case1 (addr, args) ->
    if (&&) (negb ptr64) (addressing_valid addr)
    then (addr, args)
    else ((Aindexed Z0), (Econs (e, Enil)))
  | Coq_addressing_case2 (addr, args) ->
    if (&&) ptr64 (addressing_valid addr)
    then (addr, args)
    else ((Aindexed Z0), (Econs (e, Enil)))
  | Coq_addressing_default e0 -> ((Aindexed Z0), (Econs (e0, Enil)))

type builtin_arg_addr_cases =
| Coq_builtin_arg_addr_case1 of coq_Z * expr
| Coq_builtin_arg_addr_case2 of ident * Ptrofs.int
| Coq_builtin_arg_addr_case3 of Ptrofs.int
| Coq_builtin_arg_addr_default of addressing * exprlist

(** val builtin_arg_addr_match :
    addressing -> exprlist -> builtin_arg_addr_cases **)

let builtin_arg_addr_match addr el =
  match addr with
  | Aindexed n ->
    let addr0 = Aindexed n in
    (match el with
     | Enil -> Coq_builtin_arg_addr_default (addr0, Enil)
     | Econs (e1, e) ->
       (match e with
        | Enil -> Coq_builtin_arg_addr_case1 (n, e1)
        | Econs (e0, e2) ->
          Coq_builtin_arg_addr_default (addr0, (Econs (e1, (Econs (e0, e2)))))))
  | Aglobal (id, ofs) ->
    (match el with
     | Enil -> Coq_builtin_arg_addr_case2 (id, ofs)
     | Econs (e, e0) ->
       Coq_builtin_arg_addr_default ((Aglobal (id, ofs)), (Econs (e, e0))))
  | Ainstack ofs ->
    (match el with
     | Enil -> Coq_builtin_arg_addr_case3 ofs
     | Econs (e, e0) ->
       Coq_builtin_arg_addr_default ((Ainstack ofs), (Econs (e, e0))))
  | x -> Coq_builtin_arg_addr_default (x, el)

(** val builtin_arg_addr : addressing -> exprlist -> expr builtin_arg **)

let builtin_arg_addr addr el =
  match builtin_arg_addr_match addr el with
  | Coq_builtin_arg_addr_case1 (n, e1) ->
    BA_addptr ((BA e1),
      (if ptr64 then BA_long (Int64.repr n) else BA_int (Int.repr n)))
  | Coq_builtin_arg_addr_case2 (id, ofs) -> BA_addrglobal (id, ofs)
  | Coq_builtin_arg_addr_case3 ofs -> BA_addrstack ofs
  | Coq_builtin_arg_addr_default (addr0, el0) ->
    BA (Eop ((coq_Olea_ptr addr0), el0))

type builtin_arg_cases =
| Coq_builtin_arg_case1 of Int.int
| Coq_builtin_arg_case2 of Int64.int
| Coq_builtin_arg_case3 of addressing * exprlist
| Coq_builtin_arg_case4 of addressing * exprlist
| Coq_builtin_arg_case5 of Int.int * Int.int
| Coq_builtin_arg_case6 of expr * expr
| Coq_builtin_arg_case7 of memory_chunk * ident * Ptrofs.int
| Coq_builtin_arg_case8 of memory_chunk * Ptrofs.int
| Coq_builtin_arg_default of expr

(** val builtin_arg_match : expr -> builtin_arg_cases **)

let builtin_arg_match = function
| Eop (o, el) ->
  (match o with
   | Ointconst n ->
     (match el with
      | Enil -> Coq_builtin_arg_case1 n
      | Econs (e0, e1) ->
        Coq_builtin_arg_default (Eop ((Ointconst n), (Econs (e0, e1)))))
   | Olongconst n ->
     (match el with
      | Enil -> Coq_builtin_arg_case2 n
      | Econs (e0, e1) ->
        Coq_builtin_arg_default (Eop ((Olongconst n), (Econs (e0, e1)))))
   | Olea addr -> Coq_builtin_arg_case3 (addr, el)
   | Omakelong ->
     (match el with
      | Enil -> Coq_builtin_arg_default (Eop (Omakelong, Enil))
      | Econs (h, e0) ->
        (match h with
         | Evar i ->
           (match e0 with
            | Enil ->
              Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Evar i),
                Enil))))
            | Econs (l, e1) ->
              (match e1 with
               | Enil -> Coq_builtin_arg_case6 ((Evar i), l)
               | Econs (e2, e3) ->
                 Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Evar i),
                   (Econs (l, (Econs (e2, e3))))))))))
         | Eop (o0, e1) ->
           (match o0 with
            | Ointconst h0 ->
              (match e1 with
               | Enil ->
                 let h1 = Eop ((Ointconst h0), Enil) in
                 (match e0 with
                  | Enil ->
                    Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Eop
                      ((Ointconst h0), Enil)), Enil))))
                  | Econs (l, e2) ->
                    (match l with
                     | Evar i ->
                       (match e2 with
                        | Enil -> Coq_builtin_arg_case6 (h1, (Evar i))
                        | Econs (e3, e4) ->
                          Coq_builtin_arg_default (Eop (Omakelong, (Econs
                            ((Eop ((Ointconst h0), Enil)), (Econs ((Evar i),
                            (Econs (e3, e4)))))))))
                     | Eop (o1, e3) ->
                       (match o1 with
                        | Ointconst l0 ->
                          (match e3 with
                           | Enil ->
                             (match e2 with
                              | Enil -> Coq_builtin_arg_case5 (h0, l0)
                              | Econs (e4, e5) ->
                                Coq_builtin_arg_default (Eop (Omakelong,
                                  (Econs ((Eop ((Ointconst h0), Enil)),
                                  (Econs ((Eop ((Ointconst l0), Enil)),
                                  (Econs (e4, e5)))))))))
                           | Econs (e4, e5) ->
                             (match e2 with
                              | Enil ->
                                Coq_builtin_arg_case6 (h1, (Eop ((Ointconst
                                  l0), (Econs (e4, e5)))))
                              | Econs (e6, e7) ->
                                Coq_builtin_arg_default (Eop (Omakelong,
                                  (Econs ((Eop ((Ointconst h0), Enil)),
                                  (Econs ((Eop ((Ointconst l0), (Econs (e4,
                                  e5)))), (Econs (e6, e7))))))))))
                        | Omakelong ->
                          (match e2 with
                           | Enil ->
                             Coq_builtin_arg_case6 (h1, (Eop (Omakelong, e3)))
                           | Econs (e4, e5) ->
                             Coq_builtin_arg_default (Eop (Omakelong, (Econs
                               ((Eop ((Ointconst h0), Enil)), (Econs ((Eop
                               (Omakelong, e3)), (Econs (e4, e5)))))))))
                        | x ->
                          (match e2 with
                           | Enil -> Coq_builtin_arg_case6 (h1, (Eop (x, e3)))
                           | Econs (e4, e5) ->
                             Coq_builtin_arg_default (Eop (Omakelong, (Econs
                               ((Eop ((Ointconst h0), Enil)), (Econs ((Eop
                               (x, e3)), (Econs (e4, e5))))))))))
                     | Eload (m, a, e3) ->
                       (match e2 with
                        | Enil ->
                          Coq_builtin_arg_case6 (h1, (Eload (m, a, e3)))
                        | Econs (e4, e5) ->
                          Coq_builtin_arg_default (Eop (Omakelong, (Econs
                            ((Eop ((Ointconst h0), Enil)), (Econs ((Eload (m,
                            a, e3)), (Econs (e4, e5)))))))))
                     | Eletvar n ->
                       (match e2 with
                        | Enil -> Coq_builtin_arg_case6 (h1, (Eletvar n))
                        | Econs (e3, e4) ->
                          Coq_builtin_arg_default (Eop (Omakelong, (Econs
                            ((Eop ((Ointconst h0), Enil)), (Econs ((Eletvar
                            n), (Econs (e3, e4)))))))))
                     | Eexternal (i, s, e3) ->
                       (match e2 with
                        | Enil ->
                          Coq_builtin_arg_case6 (h1, (Eexternal (i, s, e3)))
                        | Econs (e4, e5) ->
                          Coq_builtin_arg_default (Eop (Omakelong, (Econs
                            ((Eop ((Ointconst h0), Enil)), (Econs ((Eexternal
                            (i, s, e3)), (Econs (e4, e5)))))))))
                     | x ->
                       (match e2 with
                        | Enil -> Coq_builtin_arg_case6 (h1, x)
                        | Econs (e5, e6) ->
                          Coq_builtin_arg_default (Eop (Omakelong, (Econs
                            ((Eop ((Ointconst h0), Enil)), (Econs (x, (Econs
                            (e5, e6)))))))))))
               | Econs (e2, e3) ->
                 (match e0 with
                  | Enil ->
                    Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Eop
                      ((Ointconst h0), (Econs (e2, e3)))), Enil))))
                  | Econs (l, e4) ->
                    (match e4 with
                     | Enil ->
                       Coq_builtin_arg_case6 ((Eop ((Ointconst h0), (Econs
                         (e2, e3)))), l)
                     | Econs (e5, e6) ->
                       Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Eop
                         ((Ointconst h0), (Econs (e2, e3)))), (Econs (l,
                         (Econs (e5, e6)))))))))))
            | Omakelong ->
              (match e0 with
               | Enil ->
                 Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Eop
                   (Omakelong, e1)), Enil))))
               | Econs (l, e2) ->
                 (match e2 with
                  | Enil -> Coq_builtin_arg_case6 ((Eop (Omakelong, e1)), l)
                  | Econs (e3, e4) ->
                    Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Eop
                      (Omakelong, e1)), (Econs (l, (Econs (e3, e4))))))))))
            | x ->
              (match e0 with
               | Enil ->
                 Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Eop (x,
                   e1)), Enil))))
               | Econs (l, e2) ->
                 (match e2 with
                  | Enil -> Coq_builtin_arg_case6 ((Eop (x, e1)), l)
                  | Econs (e3, e4) ->
                    Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Eop (x,
                      e1)), (Econs (l, (Econs (e3, e4)))))))))))
         | Eload (m, a, e1) ->
           (match e0 with
            | Enil ->
              Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Eload (m, a,
                e1)), Enil))))
            | Econs (l, e2) ->
              (match e2 with
               | Enil -> Coq_builtin_arg_case6 ((Eload (m, a, e1)), l)
               | Econs (e3, e4) ->
                 Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Eload (m,
                   a, e1)), (Econs (l, (Econs (e3, e4))))))))))
         | Eletvar n ->
           (match e0 with
            | Enil ->
              Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Eletvar n),
                Enil))))
            | Econs (l, e1) ->
              (match e1 with
               | Enil -> Coq_builtin_arg_case6 ((Eletvar n), l)
               | Econs (e2, e3) ->
                 Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Eletvar
                   n), (Econs (l, (Econs (e2, e3))))))))))
         | Eexternal (i, s, e1) ->
           (match e0 with
            | Enil ->
              Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Eexternal (i,
                s, e1)), Enil))))
            | Econs (l, e2) ->
              (match e2 with
               | Enil -> Coq_builtin_arg_case6 ((Eexternal (i, s, e1)), l)
               | Econs (e3, e4) ->
                 Coq_builtin_arg_default (Eop (Omakelong, (Econs ((Eexternal
                   (i, s, e1)), (Econs (l, (Econs (e3, e4))))))))))
         | x ->
           (match e0 with
            | Enil ->
              Coq_builtin_arg_default (Eop (Omakelong, (Econs (x, Enil))))
            | Econs (l, e3) ->
              (match e3 with
               | Enil -> Coq_builtin_arg_case6 (x, l)
               | Econs (e4, e5) ->
                 Coq_builtin_arg_default (Eop (Omakelong, (Econs (x, (Econs
                   (l, (Econs (e4, e5))))))))))))
   | Oleal addr -> Coq_builtin_arg_case4 (addr, el)
   | x -> Coq_builtin_arg_default (Eop (x, el)))
| Eload (chunk, a, e0) ->
  (match a with
   | Aglobal (id, ofs) ->
     (match e0 with
      | Enil -> Coq_builtin_arg_case7 (chunk, id, ofs)
      | Econs (e1, e2) ->
        Coq_builtin_arg_default (Eload (chunk, (Aglobal (id, ofs)), (Econs
          (e1, e2)))))
   | Ainstack ofs ->
     (match e0 with
      | Enil -> Coq_builtin_arg_case8 (chunk, ofs)
      | Econs (e1, e2) ->
        Coq_builtin_arg_default (Eload (chunk, (Ainstack ofs), (Econs (e1,
          e2)))))
   | x -> Coq_builtin_arg_default (Eload (chunk, x, e0)))
| x -> Coq_builtin_arg_default x

(** val builtin_arg : expr -> expr builtin_arg **)

let builtin_arg e =
  match builtin_arg_match e with
  | Coq_builtin_arg_case1 n -> BA_int n
  | Coq_builtin_arg_case2 n -> BA_long n
  | Coq_builtin_arg_case3 (addr, el) ->
    if ptr64 then BA e else builtin_arg_addr addr el
  | Coq_builtin_arg_case4 (addr, el) ->
    if ptr64 then builtin_arg_addr addr el else BA e
  | Coq_builtin_arg_case5 (h, l) -> BA_long (Int64.ofwords h l)
  | Coq_builtin_arg_case6 (h, l) -> BA_splitlong ((BA h), (BA l))
  | Coq_builtin_arg_case7 (chunk, id, ofs) -> BA_loadglobal (chunk, id, ofs)
  | Coq_builtin_arg_case8 (chunk, ofs) -> BA_loadstack (chunk, ofs)
  | Coq_builtin_arg_default e0 -> BA e0

(** val platform_builtin : platform_builtin -> exprlist -> expr option **)

let platform_builtin _ _ =
  None
