open Archi
open BinInt
open BinNums
open CminorSel
open Datatypes
open Integers
open Op
open SelectOp
open SplitLong

(** val longconst : Int64.int -> expr **)

let longconst n =
  if Archi.splitlong then longconst n else Eop ((Olongconst n), Enil)

(** val is_longconst : expr -> Int64.int option **)

let is_longconst e =
  if Archi.splitlong
  then is_longconst e
  else (match e with
        | Eop (o, e0) ->
          (match o with
           | Olongconst n ->
             (match e0 with
              | Enil -> Some n
              | Econs (_, _) -> None)
           | _ -> None)
        | _ -> None)

(** val intoflong : expr -> expr **)

let intoflong e =
  if Archi.splitlong
  then intoflong e
  else (match is_longconst e with
        | Some n -> Eop ((Ointconst (Int.repr (Int64.unsigned n))), Enil)
        | None -> Eop (Olowlong, (Econs (e, Enil))))

(** val longofint : expr -> expr **)

let longofint e =
  if Archi.splitlong
  then longofint e
  else (match is_intconst e with
        | Some n -> longconst (Int64.repr (Int.signed n))
        | None -> Eop (Ocast32signed, (Econs (e, Enil))))

(** val longofintu : expr -> expr **)

let longofintu e =
  if Archi.splitlong
  then longofintu e
  else (match is_intconst e with
        | Some n -> longconst (Int64.repr (Int.unsigned n))
        | None -> Eop (Ocast32unsigned, (Econs (e, Enil))))

type notl_cases =
| Coq_notl_case1 of Int64.int
| Coq_notl_case2 of expr
| Coq_notl_default of expr

(** val notl_match : expr -> notl_cases **)

let notl_match = function
| Eop (o, e0) ->
  (match o with
   | Olongconst n ->
     (match e0 with
      | Enil -> Coq_notl_case1 n
      | Econs (e1, e2) ->
        Coq_notl_default (Eop ((Olongconst n), (Econs (e1, e2)))))
   | Onotl ->
     (match e0 with
      | Enil -> Coq_notl_default (Eop (Onotl, Enil))
      | Econs (t1, e1) ->
        (match e1 with
         | Enil -> Coq_notl_case2 t1
         | Econs (e2, e3) ->
           Coq_notl_default (Eop (Onotl, (Econs (t1, (Econs (e2, e3))))))))
   | x -> Coq_notl_default (Eop (x, e0)))
| x -> Coq_notl_default x

(** val notl : expr -> expr **)

let notl e =
  if Archi.splitlong
  then notl e
  else (match notl_match e with
        | Coq_notl_case1 n -> longconst (Int64.not n)
        | Coq_notl_case2 t1 -> t1
        | Coq_notl_default e0 -> Eop (Onotl, (Econs (e0, Enil))))

type andlimm_cases =
| Coq_andlimm_case1 of Int64.int
| Coq_andlimm_case2 of Int64.int * expr
| Coq_andlimm_default of expr

(** val andlimm_match : expr -> andlimm_cases **)

let andlimm_match = function
| Eop (o, e) ->
  (match o with
   | Olongconst n2 ->
     (match e with
      | Enil -> Coq_andlimm_case1 n2
      | Econs (e0, e1) ->
        Coq_andlimm_default (Eop ((Olongconst n2), (Econs (e0, e1)))))
   | Oandlimm n2 ->
     (match e with
      | Enil -> Coq_andlimm_default (Eop ((Oandlimm n2), Enil))
      | Econs (t2, e0) ->
        (match e0 with
         | Enil -> Coq_andlimm_case2 (n2, t2)
         | Econs (e1, e3) ->
           Coq_andlimm_default (Eop ((Oandlimm n2), (Econs (t2, (Econs (e1,
             e3))))))))
   | x -> Coq_andlimm_default (Eop (x, e)))
| x -> Coq_andlimm_default x

(** val andlimm : Int64.int -> expr -> expr **)

let andlimm n1 e2 =
  if Int64.eq n1 Int64.zero
  then longconst Int64.zero
  else if Int64.eq n1 Int64.mone
       then e2
       else (match andlimm_match e2 with
             | Coq_andlimm_case1 n2 -> longconst (Int64.coq_and n1 n2)
             | Coq_andlimm_case2 (n2, t2) ->
               Eop ((Oandlimm (Int64.coq_and n1 n2)), (Econs (t2, Enil)))
             | Coq_andlimm_default e3 ->
               Eop ((Oandlimm n1), (Econs (e3, Enil))))

type andl_cases =
| Coq_andl_case1 of Int64.int * expr
| Coq_andl_case2 of expr * Int64.int
| Coq_andl_default of expr * expr

(** val andl_match : expr -> expr -> andl_cases **)

let andl_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Olongconst n2 ->
          (match e with
           | Enil -> Coq_andl_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_andl_default (e3, (Eop ((Olongconst n2), (Econs (e0, e4))))))
        | x -> Coq_andl_default (e3, (Eop (x, e))))
     | x -> Coq_andl_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Olongconst n1 ->
       (match e with
        | Enil -> Coq_andl_case1 (n1, e2)
        | Econs (e0, e3) ->
          let e4 = Eop ((Olongconst n1), (Econs (e0, e3))) in
          (match e2 with
           | Eop (o0, e5) ->
             (match o0 with
              | Olongconst n2 ->
                (match e5 with
                 | Enil -> Coq_andl_case2 (e4, n2)
                 | Econs (e6, e7) ->
                   Coq_andl_default (e4, (Eop ((Olongconst n2), (Econs (e6,
                     e7))))))
              | x -> Coq_andl_default (e4, (Eop (x, e5))))
           | x -> Coq_andl_default (e4, x)))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Olongconst n2 ->
             (match e0 with
              | Enil -> Coq_andl_case2 (e3, n2)
              | Econs (e4, e5) ->
                Coq_andl_default (e3, (Eop ((Olongconst n2), (Econs (e4,
                  e5))))))
           | x0 -> Coq_andl_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_andl_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Olongconst n2 ->
          (match e0 with
           | Enil -> Coq_andl_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_andl_default (e3, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | x -> Coq_andl_default (e3, (Eop (x, e0))))
     | x -> Coq_andl_default (e3, x))
  | Eletvar n ->
    let e3 = Eletvar n in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Olongconst n2 ->
          (match e with
           | Enil -> Coq_andl_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_andl_default (e3, (Eop ((Olongconst n2), (Econs (e0, e4))))))
        | x -> Coq_andl_default (e3, (Eop (x, e))))
     | x -> Coq_andl_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Olongconst n2 ->
          (match e0 with
           | Enil -> Coq_andl_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_andl_default (e3, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | x -> Coq_andl_default (e3, (Eop (x, e0))))
     | x -> Coq_andl_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Olongconst n2 ->
          (match e3 with
           | Enil -> Coq_andl_case2 (x, n2)
           | Econs (e4, e5) ->
             Coq_andl_default (x, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | x0 -> Coq_andl_default (x, (Eop (x0, e3))))
     | x0 -> Coq_andl_default (x, x0))

(** val andl : expr -> expr -> expr **)

let andl e1 e2 =
  if Archi.splitlong
  then andl e1 e2
  else (match andl_match e1 e2 with
        | Coq_andl_case1 (n1, t2) -> andlimm n1 t2
        | Coq_andl_case2 (t1, n2) -> andlimm n2 t1
        | Coq_andl_default (e3, e4) ->
          Eop (Oandl, (Econs (e3, (Econs (e4, Enil))))))

type orlimm_cases =
| Coq_orlimm_case1 of Int64.int
| Coq_orlimm_case2 of Int64.int * expr
| Coq_orlimm_default of expr

(** val orlimm_match : expr -> orlimm_cases **)

let orlimm_match = function
| Eop (o, e) ->
  (match o with
   | Olongconst n2 ->
     (match e with
      | Enil -> Coq_orlimm_case1 n2
      | Econs (e0, e1) ->
        Coq_orlimm_default (Eop ((Olongconst n2), (Econs (e0, e1)))))
   | Oorlimm n2 ->
     (match e with
      | Enil -> Coq_orlimm_default (Eop ((Oorlimm n2), Enil))
      | Econs (t2, e0) ->
        (match e0 with
         | Enil -> Coq_orlimm_case2 (n2, t2)
         | Econs (e1, e3) ->
           Coq_orlimm_default (Eop ((Oorlimm n2), (Econs (t2, (Econs (e1,
             e3))))))))
   | x -> Coq_orlimm_default (Eop (x, e)))
| x -> Coq_orlimm_default x

(** val orlimm : Int64.int -> expr -> expr **)

let orlimm n1 e2 =
  if Int64.eq n1 Int64.zero
  then e2
  else if Int64.eq n1 Int64.mone
       then longconst Int64.mone
       else (match orlimm_match e2 with
             | Coq_orlimm_case1 n2 -> longconst (Int64.coq_or n1 n2)
             | Coq_orlimm_case2 (n2, t2) ->
               Eop ((Oorlimm (Int64.coq_or n1 n2)), (Econs (t2, Enil)))
             | Coq_orlimm_default e3 -> Eop ((Oorlimm n1), (Econs (e3, Enil))))

type orl_cases =
| Coq_orl_case1 of Int64.int * expr
| Coq_orl_case2 of expr * Int64.int
| Coq_orl_case3 of Int.int * expr * Int.int * expr
| Coq_orl_case4 of Int.int * expr * Int.int * expr
| Coq_orl_default of expr * expr

(** val orl_match : expr -> expr -> orl_cases **)

let orl_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Olongconst n2 ->
          (match e with
           | Enil -> Coq_orl_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_orl_default (e3, (Eop ((Olongconst n2), (Econs (e0, e4))))))
        | x -> Coq_orl_default (e3, (Eop (x, e))))
     | x -> Coq_orl_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Olongconst n1 ->
       (match e with
        | Enil -> Coq_orl_case1 (n1, e2)
        | Econs (e0, e3) ->
          let e4 = Eop ((Olongconst n1), (Econs (e0, e3))) in
          (match e2 with
           | Eop (o0, e5) ->
             (match o0 with
              | Olongconst n2 ->
                (match e5 with
                 | Enil -> Coq_orl_case2 (e4, n2)
                 | Econs (e6, e7) ->
                   Coq_orl_default (e4, (Eop ((Olongconst n2), (Econs (e6,
                     e7))))))
              | x -> Coq_orl_default (e4, (Eop (x, e5))))
           | x -> Coq_orl_default (e4, x)))
     | Oshllimm n1 ->
       (match e with
        | Enil ->
          let e3 = Eop ((Oshllimm n1), Enil) in
          (match e2 with
           | Eop (o0, e0) ->
             (match o0 with
              | Olongconst n2 ->
                (match e0 with
                 | Enil -> Coq_orl_case2 (e3, n2)
                 | Econs (e4, e5) ->
                   Coq_orl_default (e3, (Eop ((Olongconst n2), (Econs (e4,
                     e5))))))
              | x -> Coq_orl_default (e3, (Eop (x, e0))))
           | x -> Coq_orl_default (e3, x))
        | Econs (t1, e0) ->
          (match e0 with
           | Enil ->
             let e3 = Eop ((Oshllimm n1), (Econs (t1, Enil))) in
             (match e2 with
              | Eop (o0, e4) ->
                (match o0 with
                 | Olongconst n2 ->
                   (match e4 with
                    | Enil -> Coq_orl_case2 (e3, n2)
                    | Econs (e5, e6) ->
                      Coq_orl_default (e3, (Eop ((Olongconst n2), (Econs (e5,
                        e6))))))
                 | Oshrluimm n2 ->
                   (match e4 with
                    | Enil ->
                      Coq_orl_default (e3, (Eop ((Oshrluimm n2), Enil)))
                    | Econs (t2, e5) ->
                      (match e5 with
                       | Enil -> Coq_orl_case3 (n1, t1, n2, t2)
                       | Econs (e6, e7) ->
                         Coq_orl_default (e3, (Eop ((Oshrluimm n2), (Econs
                           (t2, (Econs (e6, e7)))))))))
                 | x -> Coq_orl_default (e3, (Eop (x, e4))))
              | x -> Coq_orl_default (e3, x))
           | Econs (e3, e4) ->
             let e5 = Eop ((Oshllimm n1), (Econs (t1, (Econs (e3, e4))))) in
             (match e2 with
              | Eop (o0, e6) ->
                (match o0 with
                 | Olongconst n2 ->
                   (match e6 with
                    | Enil -> Coq_orl_case2 (e5, n2)
                    | Econs (e7, e8) ->
                      Coq_orl_default (e5, (Eop ((Olongconst n2), (Econs (e7,
                        e8))))))
                 | x -> Coq_orl_default (e5, (Eop (x, e6))))
              | x -> Coq_orl_default (e5, x))))
     | Oshrluimm n2 ->
       (match e with
        | Enil ->
          let e3 = Eop ((Oshrluimm n2), Enil) in
          (match e2 with
           | Eop (o0, e0) ->
             (match o0 with
              | Olongconst n3 ->
                (match e0 with
                 | Enil -> Coq_orl_case2 (e3, n3)
                 | Econs (e4, e5) ->
                   Coq_orl_default (e3, (Eop ((Olongconst n3), (Econs (e4,
                     e5))))))
              | x -> Coq_orl_default (e3, (Eop (x, e0))))
           | x -> Coq_orl_default (e3, x))
        | Econs (t2, e0) ->
          (match e0 with
           | Enil ->
             let e3 = Eop ((Oshrluimm n2), (Econs (t2, Enil))) in
             (match e2 with
              | Eop (o0, e4) ->
                (match o0 with
                 | Olongconst n3 ->
                   (match e4 with
                    | Enil -> Coq_orl_case2 (e3, n3)
                    | Econs (e5, e6) ->
                      Coq_orl_default (e3, (Eop ((Olongconst n3), (Econs (e5,
                        e6))))))
                 | Oshllimm n1 ->
                   (match e4 with
                    | Enil ->
                      Coq_orl_default (e3, (Eop ((Oshllimm n1), Enil)))
                    | Econs (t1, e5) ->
                      (match e5 with
                       | Enil -> Coq_orl_case4 (n2, t2, n1, t1)
                       | Econs (e6, e7) ->
                         Coq_orl_default (e3, (Eop ((Oshllimm n1), (Econs
                           (t1, (Econs (e6, e7)))))))))
                 | x -> Coq_orl_default (e3, (Eop (x, e4))))
              | x -> Coq_orl_default (e3, x))
           | Econs (e3, e4) ->
             let e5 = Eop ((Oshrluimm n2), (Econs (t2, (Econs (e3, e4))))) in
             (match e2 with
              | Eop (o0, e6) ->
                (match o0 with
                 | Olongconst n3 ->
                   (match e6 with
                    | Enil -> Coq_orl_case2 (e5, n3)
                    | Econs (e7, e8) ->
                      Coq_orl_default (e5, (Eop ((Olongconst n3), (Econs (e7,
                        e8))))))
                 | x -> Coq_orl_default (e5, (Eop (x, e6))))
              | x -> Coq_orl_default (e5, x))))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Olongconst n2 ->
             (match e0 with
              | Enil -> Coq_orl_case2 (e3, n2)
              | Econs (e4, e5) ->
                Coq_orl_default (e3, (Eop ((Olongconst n2), (Econs (e4,
                  e5))))))
           | x0 -> Coq_orl_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_orl_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Olongconst n2 ->
          (match e0 with
           | Enil -> Coq_orl_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_orl_default (e3, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | x -> Coq_orl_default (e3, (Eop (x, e0))))
     | x -> Coq_orl_default (e3, x))
  | Eletvar n ->
    let e3 = Eletvar n in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Olongconst n2 ->
          (match e with
           | Enil -> Coq_orl_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_orl_default (e3, (Eop ((Olongconst n2), (Econs (e0, e4))))))
        | x -> Coq_orl_default (e3, (Eop (x, e))))
     | x -> Coq_orl_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Olongconst n2 ->
          (match e0 with
           | Enil -> Coq_orl_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_orl_default (e3, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | x -> Coq_orl_default (e3, (Eop (x, e0))))
     | x -> Coq_orl_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Olongconst n2 ->
          (match e3 with
           | Enil -> Coq_orl_case2 (x, n2)
           | Econs (e4, e5) ->
             Coq_orl_default (x, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | x0 -> Coq_orl_default (x, (Eop (x0, e3))))
     | x0 -> Coq_orl_default (x, x0))

(** val orl : expr -> expr -> expr **)

let orl e1 e2 =
  if Archi.splitlong
  then orl e1 e2
  else (match orl_match e1 e2 with
        | Coq_orl_case1 (n1, t2) -> orlimm n1 t2
        | Coq_orl_case2 (t1, n2) -> orlimm n2 t1
        | Coq_orl_case3 (n1, t1, n2, t2) ->
          if (&&) (Int.eq (Int.add n1 n2) Int64.iwordsize')
               (same_expr_pure t1 t2)
          then Eop ((Ororlimm n2), (Econs (t1, Enil)))
          else Eop (Oorl, (Econs (e1, (Econs (e2, Enil)))))
        | Coq_orl_case4 (n2, t2, n1, t1) ->
          if (&&) (Int.eq (Int.add n1 n2) Int64.iwordsize')
               (same_expr_pure t1 t2)
          then Eop ((Ororlimm n2), (Econs (t1, Enil)))
          else Eop (Oorl, (Econs (e1, (Econs (e2, Enil)))))
        | Coq_orl_default (e3, e4) ->
          Eop (Oorl, (Econs (e3, (Econs (e4, Enil))))))

type xorlimm_cases =
| Coq_xorlimm_case1 of Int64.int
| Coq_xorlimm_case2 of Int64.int * expr
| Coq_xorlimm_case3 of expr
| Coq_xorlimm_default of expr

(** val xorlimm_match : expr -> xorlimm_cases **)

let xorlimm_match = function
| Eop (o, e) ->
  (match o with
   | Olongconst n2 ->
     (match e with
      | Enil -> Coq_xorlimm_case1 n2
      | Econs (e0, e1) ->
        Coq_xorlimm_default (Eop ((Olongconst n2), (Econs (e0, e1)))))
   | Oxorlimm n2 ->
     (match e with
      | Enil -> Coq_xorlimm_default (Eop ((Oxorlimm n2), Enil))
      | Econs (t2, e0) ->
        (match e0 with
         | Enil -> Coq_xorlimm_case2 (n2, t2)
         | Econs (e1, e3) ->
           Coq_xorlimm_default (Eop ((Oxorlimm n2), (Econs (t2, (Econs (e1,
             e3))))))))
   | Onotl ->
     (match e with
      | Enil -> Coq_xorlimm_default (Eop (Onotl, Enil))
      | Econs (t2, e0) ->
        (match e0 with
         | Enil -> Coq_xorlimm_case3 t2
         | Econs (e1, e3) ->
           Coq_xorlimm_default (Eop (Onotl, (Econs (t2, (Econs (e1, e3))))))))
   | x -> Coq_xorlimm_default (Eop (x, e)))
| x -> Coq_xorlimm_default x

(** val xorlimm : Int64.int -> expr -> expr **)

let xorlimm n1 e2 =
  if Int64.eq n1 Int64.zero
  then e2
  else if Int64.eq n1 Int64.mone
       then notl e2
       else (match xorlimm_match e2 with
             | Coq_xorlimm_case1 n2 -> longconst (Int64.xor n1 n2)
             | Coq_xorlimm_case2 (n2, t2) ->
               Eop ((Oxorlimm (Int64.xor n1 n2)), (Econs (t2, Enil)))
             | Coq_xorlimm_case3 t2 ->
               Eop ((Oxorlimm (Int64.not n1)), (Econs (t2, Enil)))
             | Coq_xorlimm_default e3 ->
               Eop ((Oxorlimm n1), (Econs (e3, Enil))))

type xorl_cases =
| Coq_xorl_case1 of Int64.int * expr
| Coq_xorl_case2 of expr * Int64.int
| Coq_xorl_default of expr * expr

(** val xorl_match : expr -> expr -> xorl_cases **)

let xorl_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Olongconst n2 ->
          (match e with
           | Enil -> Coq_xorl_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_xorl_default (e3, (Eop ((Olongconst n2), (Econs (e0, e4))))))
        | x -> Coq_xorl_default (e3, (Eop (x, e))))
     | x -> Coq_xorl_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Olongconst n1 ->
       (match e with
        | Enil -> Coq_xorl_case1 (n1, e2)
        | Econs (e0, e3) ->
          let e4 = Eop ((Olongconst n1), (Econs (e0, e3))) in
          (match e2 with
           | Eop (o0, e5) ->
             (match o0 with
              | Olongconst n2 ->
                (match e5 with
                 | Enil -> Coq_xorl_case2 (e4, n2)
                 | Econs (e6, e7) ->
                   Coq_xorl_default (e4, (Eop ((Olongconst n2), (Econs (e6,
                     e7))))))
              | x -> Coq_xorl_default (e4, (Eop (x, e5))))
           | x -> Coq_xorl_default (e4, x)))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Olongconst n2 ->
             (match e0 with
              | Enil -> Coq_xorl_case2 (e3, n2)
              | Econs (e4, e5) ->
                Coq_xorl_default (e3, (Eop ((Olongconst n2), (Econs (e4,
                  e5))))))
           | x0 -> Coq_xorl_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_xorl_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Olongconst n2 ->
          (match e0 with
           | Enil -> Coq_xorl_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_xorl_default (e3, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | x -> Coq_xorl_default (e3, (Eop (x, e0))))
     | x -> Coq_xorl_default (e3, x))
  | Eletvar n ->
    let e3 = Eletvar n in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Olongconst n2 ->
          (match e with
           | Enil -> Coq_xorl_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_xorl_default (e3, (Eop ((Olongconst n2), (Econs (e0, e4))))))
        | x -> Coq_xorl_default (e3, (Eop (x, e))))
     | x -> Coq_xorl_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Olongconst n2 ->
          (match e0 with
           | Enil -> Coq_xorl_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_xorl_default (e3, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | x -> Coq_xorl_default (e3, (Eop (x, e0))))
     | x -> Coq_xorl_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Olongconst n2 ->
          (match e3 with
           | Enil -> Coq_xorl_case2 (x, n2)
           | Econs (e4, e5) ->
             Coq_xorl_default (x, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | x0 -> Coq_xorl_default (x, (Eop (x0, e3))))
     | x0 -> Coq_xorl_default (x, x0))

(** val xorl : expr -> expr -> expr **)

let xorl e1 e2 =
  if Archi.splitlong
  then xorl e1 e2
  else (match xorl_match e1 e2 with
        | Coq_xorl_case1 (n1, t2) -> xorlimm n1 t2
        | Coq_xorl_case2 (t1, n2) -> xorlimm n2 t1
        | Coq_xorl_default (e3, e4) ->
          Eop (Oxorl, (Econs (e3, (Econs (e4, Enil))))))

type shllimm_cases =
| Coq_shllimm_case1 of Int64.int
| Coq_shllimm_case2 of Int.int * expr
| Coq_shllimm_case3 of coq_Z * expr
| Coq_shllimm_default of expr

(** val shllimm_match : expr -> shllimm_cases **)

let shllimm_match = function
| Eop (o, e) ->
  (match o with
   | Olongconst n1 ->
     (match e with
      | Enil -> Coq_shllimm_case1 n1
      | Econs (e0, e2) ->
        Coq_shllimm_default (Eop ((Olongconst n1), (Econs (e0, e2)))))
   | Oshllimm n1 ->
     (match e with
      | Enil -> Coq_shllimm_default (Eop ((Oshllimm n1), Enil))
      | Econs (t1, e0) ->
        (match e0 with
         | Enil -> Coq_shllimm_case2 (n1, t1)
         | Econs (e2, e3) ->
           Coq_shllimm_default (Eop ((Oshllimm n1), (Econs (t1, (Econs (e2,
             e3))))))))
   | Oleal a ->
     (match a with
      | Aindexed n1 ->
        (match e with
         | Enil -> Coq_shllimm_default (Eop ((Oleal (Aindexed n1)), Enil))
         | Econs (t1, e0) ->
           (match e0 with
            | Enil -> Coq_shllimm_case3 (n1, t1)
            | Econs (e2, e3) ->
              Coq_shllimm_default (Eop ((Oleal (Aindexed n1)), (Econs (t1,
                (Econs (e2, e3))))))))
      | x -> Coq_shllimm_default (Eop ((Oleal x), e)))
   | x -> Coq_shllimm_default (Eop (x, e)))
| x -> Coq_shllimm_default x

(** val shllimm : helper_functions -> expr -> Int.int -> expr **)

let shllimm hf e1 n =
  if Archi.splitlong
  then shllimm hf e1 n
  else if Int.eq n Int.zero
       then e1
       else if negb (Int.ltu n Int64.iwordsize')
            then Eop (Oshll, (Econs (e1, (Econs ((Eop ((Ointconst n), Enil)),
                   Enil)))))
            else (match shllimm_match e1 with
                  | Coq_shllimm_case1 n1 ->
                    Eop ((Olongconst (Int64.shl' n1 n)), Enil)
                  | Coq_shllimm_case2 (n1, t1) ->
                    if Int.ltu (Int.add n n1) Int64.iwordsize'
                    then Eop ((Oshllimm (Int.add n n1)), (Econs (t1, Enil)))
                    else Eop ((Oshllimm n), (Econs (e1, Enil)))
                  | Coq_shllimm_case3 (n1, t1) ->
                    if shift_is_scale n
                    then Eop ((Oleal (Ascaled
                           ((Int64.unsigned (Int64.shl' Int64.one n)),
                           (Int64.unsigned (Int64.shl' (Int64.repr n1) n))))),
                           (Econs (t1, Enil)))
                    else Eop ((Oshllimm n), (Econs (e1, Enil)))
                  | Coq_shllimm_default e2 ->
                    if shift_is_scale n
                    then Eop ((Oleal (Ascaled
                           ((Int64.unsigned (Int64.shl' Int64.one n)), Z0))),
                           (Econs (e2, Enil)))
                    else Eop ((Oshllimm n), (Econs (e2, Enil))))

type shrluimm_cases =
| Coq_shrluimm_case1 of Int64.int
| Coq_shrluimm_case2 of Int.int * expr
| Coq_shrluimm_default of expr

(** val shrluimm_match : expr -> shrluimm_cases **)

let shrluimm_match = function
| Eop (o, e) ->
  (match o with
   | Olongconst n1 ->
     (match e with
      | Enil -> Coq_shrluimm_case1 n1
      | Econs (e0, e2) ->
        Coq_shrluimm_default (Eop ((Olongconst n1), (Econs (e0, e2)))))
   | Oshrluimm n1 ->
     (match e with
      | Enil -> Coq_shrluimm_default (Eop ((Oshrluimm n1), Enil))
      | Econs (t1, e0) ->
        (match e0 with
         | Enil -> Coq_shrluimm_case2 (n1, t1)
         | Econs (e2, e3) ->
           Coq_shrluimm_default (Eop ((Oshrluimm n1), (Econs (t1, (Econs (e2,
             e3))))))))
   | x -> Coq_shrluimm_default (Eop (x, e)))
| x -> Coq_shrluimm_default x

(** val shrluimm : helper_functions -> expr -> Int.int -> expr **)

let shrluimm hf e1 n =
  if Archi.splitlong
  then shrluimm hf e1 n
  else if Int.eq n Int.zero
       then e1
       else if negb (Int.ltu n Int64.iwordsize')
            then Eop (Oshrlu, (Econs (e1, (Econs ((Eop ((Ointconst n),
                   Enil)), Enil)))))
            else (match shrluimm_match e1 with
                  | Coq_shrluimm_case1 n1 ->
                    Eop ((Olongconst (Int64.shru' n1 n)), Enil)
                  | Coq_shrluimm_case2 (n1, t1) ->
                    if Int.ltu (Int.add n n1) Int64.iwordsize'
                    then Eop ((Oshrluimm (Int.add n n1)), (Econs (t1, Enil)))
                    else Eop ((Oshrluimm n), (Econs (e1, Enil)))
                  | Coq_shrluimm_default e2 ->
                    Eop ((Oshrluimm n), (Econs (e2, Enil))))

type shrlimm_cases =
| Coq_shrlimm_case1 of Int64.int
| Coq_shrlimm_case2 of Int.int * expr
| Coq_shrlimm_default of expr

(** val shrlimm_match : expr -> shrlimm_cases **)

let shrlimm_match = function
| Eop (o, e) ->
  (match o with
   | Olongconst n1 ->
     (match e with
      | Enil -> Coq_shrlimm_case1 n1
      | Econs (e0, e2) ->
        Coq_shrlimm_default (Eop ((Olongconst n1), (Econs (e0, e2)))))
   | Oshrlimm n1 ->
     (match e with
      | Enil -> Coq_shrlimm_default (Eop ((Oshrlimm n1), Enil))
      | Econs (t1, e0) ->
        (match e0 with
         | Enil -> Coq_shrlimm_case2 (n1, t1)
         | Econs (e2, e3) ->
           Coq_shrlimm_default (Eop ((Oshrlimm n1), (Econs (t1, (Econs (e2,
             e3))))))))
   | x -> Coq_shrlimm_default (Eop (x, e)))
| x -> Coq_shrlimm_default x

(** val shrlimm : helper_functions -> expr -> Int.int -> expr **)

let shrlimm hf e1 n =
  if Archi.splitlong
  then shrlimm hf e1 n
  else if Int.eq n Int.zero
       then e1
       else if negb (Int.ltu n Int64.iwordsize')
            then Eop (Oshrl, (Econs (e1, (Econs ((Eop ((Ointconst n), Enil)),
                   Enil)))))
            else (match shrlimm_match e1 with
                  | Coq_shrlimm_case1 n1 ->
                    Eop ((Olongconst (Int64.shr' n1 n)), Enil)
                  | Coq_shrlimm_case2 (n1, t1) ->
                    if Int.ltu (Int.add n n1) Int64.iwordsize'
                    then Eop ((Oshrlimm (Int.add n n1)), (Econs (t1, Enil)))
                    else Eop ((Oshrlimm n), (Econs (e1, Enil)))
                  | Coq_shrlimm_default e2 ->
                    Eop ((Oshrlimm n), (Econs (e2, Enil))))

(** val shll : helper_functions -> expr -> expr -> expr **)

let shll hf e1 e2 =
  if Archi.splitlong
  then shll hf e1 e2
  else (match is_intconst e2 with
        | Some n2 -> shllimm hf e1 n2
        | None -> Eop (Oshll, (Econs (e1, (Econs (e2, Enil))))))

(** val shrl : helper_functions -> expr -> expr -> expr **)

let shrl hf e1 e2 =
  if Archi.splitlong
  then shrl hf e1 e2
  else (match is_intconst e2 with
        | Some n2 -> shrlimm hf e1 n2
        | None -> Eop (Oshrl, (Econs (e1, (Econs (e2, Enil))))))

(** val shrlu : helper_functions -> expr -> expr -> expr **)

let shrlu hf e1 e2 =
  if Archi.splitlong
  then shrlu hf e1 e2
  else (match is_intconst e2 with
        | Some n2 -> shrluimm hf e1 n2
        | None -> Eop (Oshrlu, (Econs (e1, (Econs (e2, Enil))))))

type addlimm_cases =
| Coq_addlimm_case1 of Int64.int
| Coq_addlimm_case2 of addressing * exprlist
| Coq_addlimm_default of expr

(** val addlimm_match : expr -> addlimm_cases **)

let addlimm_match = function
| Eop (o, args) ->
  (match o with
   | Olongconst m ->
     (match args with
      | Enil -> Coq_addlimm_case1 m
      | Econs (e0, e1) ->
        Coq_addlimm_default (Eop ((Olongconst m), (Econs (e0, e1)))))
   | Oleal addr -> Coq_addlimm_case2 (addr, args)
   | x -> Coq_addlimm_default (Eop (x, args)))
| x -> Coq_addlimm_default x

(** val addlimm : Int64.int -> expr -> expr **)

let addlimm n e =
  if Int64.eq n Int64.zero
  then e
  else (match addlimm_match e with
        | Coq_addlimm_case1 m -> longconst (Int64.add n m)
        | Coq_addlimm_case2 (addr, args) ->
          Eop ((Oleal (offset_addressing_total addr (Int64.signed n))), args)
        | Coq_addlimm_default e0 ->
          Eop ((Oleal (Aindexed (Int64.signed n))), (Econs (e0, Enil))))

type addl_cases =
| Coq_addl_case1 of Int64.int * expr
| Coq_addl_case2 of expr * Int64.int
| Coq_addl_case3 of coq_Z * expr * coq_Z * expr
| Coq_addl_case4 of coq_Z * expr * coq_Z * coq_Z * expr
| Coq_addl_case5 of coq_Z * coq_Z * expr * coq_Z * expr
| Coq_addl_case6 of coq_Z * coq_Z * expr * expr
| Coq_addl_case7 of expr * coq_Z * coq_Z * expr
| Coq_addl_case8 of coq_Z * expr * expr
| Coq_addl_case9 of expr * coq_Z * expr
| Coq_addl_default of expr * expr

(** val addl_match : expr -> expr -> addl_cases **)

let addl_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Olongconst n2 ->
          (match e with
           | Enil -> Coq_addl_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_addl_default (e3, (Eop ((Olongconst n2), (Econs (e0, e4))))))
        | Oleal a ->
          (match a with
           | Aindexed n ->
             (match e with
              | Enil ->
                Coq_addl_default (e3, (Eop ((Oleal (Aindexed n)), Enil)))
              | Econs (t2, e0) ->
                (match e0 with
                 | Enil -> Coq_addl_case9 (e3, n, t2)
                 | Econs (e4, e5) ->
                   Coq_addl_default (e3, (Eop ((Oleal (Aindexed n)), (Econs
                     (t2, (Econs (e4, e5)))))))))
           | Ascaled (sc, n) ->
             (match e with
              | Enil ->
                Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc, n))), Enil)))
              | Econs (t2, e0) ->
                (match e0 with
                 | Enil -> Coq_addl_case7 (e3, sc, n, t2)
                 | Econs (e4, e5) ->
                   Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc, n))),
                     (Econs (t2, (Econs (e4, e5)))))))))
           | x -> Coq_addl_default (e3, (Eop ((Oleal x), e))))
        | x -> Coq_addl_default (e3, (Eop (x, e))))
     | x -> Coq_addl_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Olongconst n1 ->
       (match e with
        | Enil -> Coq_addl_case1 (n1, e2)
        | Econs (e0, e3) ->
          let e4 = Eop ((Olongconst n1), (Econs (e0, e3))) in
          (match e2 with
           | Eop (o0, e5) ->
             (match o0 with
              | Olongconst n2 ->
                (match e5 with
                 | Enil -> Coq_addl_case2 (e4, n2)
                 | Econs (e6, e7) ->
                   Coq_addl_default (e4, (Eop ((Olongconst n2), (Econs (e6,
                     e7))))))
              | Oleal a ->
                (match a with
                 | Aindexed n ->
                   (match e5 with
                    | Enil ->
                      Coq_addl_default (e4, (Eop ((Oleal (Aindexed n)),
                        Enil)))
                    | Econs (t2, e6) ->
                      (match e6 with
                       | Enil -> Coq_addl_case9 (e4, n, t2)
                       | Econs (e7, e8) ->
                         Coq_addl_default (e4, (Eop ((Oleal (Aindexed n)),
                           (Econs (t2, (Econs (e7, e8)))))))))
                 | Ascaled (sc, n) ->
                   (match e5 with
                    | Enil ->
                      Coq_addl_default (e4, (Eop ((Oleal (Ascaled (sc, n))),
                        Enil)))
                    | Econs (t2, e6) ->
                      (match e6 with
                       | Enil -> Coq_addl_case7 (e4, sc, n, t2)
                       | Econs (e7, e8) ->
                         Coq_addl_default (e4, (Eop ((Oleal (Ascaled (sc,
                           n))), (Econs (t2, (Econs (e7, e8)))))))))
                 | x -> Coq_addl_default (e4, (Eop ((Oleal x), e5))))
              | x -> Coq_addl_default (e4, (Eop (x, e5))))
           | x -> Coq_addl_default (e4, x)))
     | Olea a ->
       let e3 = Eop ((Olea a), e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Olongconst n2 ->
             (match e0 with
              | Enil -> Coq_addl_case2 (e3, n2)
              | Econs (e4, e5) ->
                Coq_addl_default (e3, (Eop ((Olongconst n2), (Econs (e4,
                  e5))))))
           | Oleal a0 ->
             (match a0 with
              | Aindexed n ->
                (match e0 with
                 | Enil ->
                   Coq_addl_default (e3, (Eop ((Oleal (Aindexed n)), Enil)))
                 | Econs (t2, e4) ->
                   (match e4 with
                    | Enil -> Coq_addl_case9 (e3, n, t2)
                    | Econs (e5, e6) ->
                      Coq_addl_default (e3, (Eop ((Oleal (Aindexed n)),
                        (Econs (t2, (Econs (e5, e6)))))))))
              | Ascaled (sc, n) ->
                (match e0 with
                 | Enil ->
                   Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc, n))),
                     Enil)))
                 | Econs (t2, e4) ->
                   (match e4 with
                    | Enil -> Coq_addl_case7 (e3, sc, n, t2)
                    | Econs (e5, e6) ->
                      Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc, n))),
                        (Econs (t2, (Econs (e5, e6)))))))))
              | x -> Coq_addl_default (e3, (Eop ((Oleal x), e0))))
           | x -> Coq_addl_default (e3, (Eop (x, e0))))
        | x -> Coq_addl_default (e3, x))
     | Oleal a ->
       (match a with
        | Aindexed n ->
          (match e with
           | Enil ->
             let e3 = Eop ((Oleal (Aindexed n)), Enil) in
             (match e2 with
              | Eop (o0, e0) ->
                (match o0 with
                 | Olongconst n2 ->
                   (match e0 with
                    | Enil -> Coq_addl_case2 (e3, n2)
                    | Econs (e4, e5) ->
                      Coq_addl_default (e3, (Eop ((Olongconst n2), (Econs
                        (e4, e5))))))
                 | Oleal a0 ->
                   (match a0 with
                    | Aindexed n0 ->
                      (match e0 with
                       | Enil ->
                         Coq_addl_default (e3, (Eop ((Oleal (Aindexed n0)),
                           Enil)))
                       | Econs (t2, e4) ->
                         (match e4 with
                          | Enil -> Coq_addl_case9 (e3, n0, t2)
                          | Econs (e5, e6) ->
                            Coq_addl_default (e3, (Eop ((Oleal (Aindexed
                              n0)), (Econs (t2, (Econs (e5, e6)))))))))
                    | Ascaled (sc, n0) ->
                      (match e0 with
                       | Enil ->
                         Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc,
                           n0))), Enil)))
                       | Econs (t2, e4) ->
                         (match e4 with
                          | Enil -> Coq_addl_case7 (e3, sc, n0, t2)
                          | Econs (e5, e6) ->
                            Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc,
                              n0))), (Econs (t2, (Econs (e5, e6)))))))))
                    | x -> Coq_addl_default (e3, (Eop ((Oleal x), e0))))
                 | x -> Coq_addl_default (e3, (Eop (x, e0))))
              | x -> Coq_addl_default (e3, x))
           | Econs (t1, e0) ->
             (match e0 with
              | Enil ->
                (match e2 with
                 | Eop (o0, e3) ->
                   (match o0 with
                    | Olongconst n2 ->
                      (match e3 with
                       | Enil ->
                         Coq_addl_case2 ((Eop ((Oleal (Aindexed n)), (Econs
                           (t1, Enil)))), n2)
                       | Econs (e4, e5) ->
                         Coq_addl_case8 (n, t1, (Eop ((Olongconst n2), (Econs
                           (e4, e5))))))
                    | Oleal a0 ->
                      (match a0 with
                       | Aindexed n0 ->
                         (match e3 with
                          | Enil ->
                            Coq_addl_case8 (n, t1, (Eop ((Oleal (Aindexed
                              n0)), Enil)))
                          | Econs (t2, e4) ->
                            (match e4 with
                             | Enil -> Coq_addl_case3 (n, t1, n0, t2)
                             | Econs (e5, e6) ->
                               Coq_addl_case8 (n, t1, (Eop ((Oleal (Aindexed
                                 n0)), (Econs (t2, (Econs (e5, e6)))))))))
                       | Ascaled (sc, n0) ->
                         (match e3 with
                          | Enil ->
                            Coq_addl_case8 (n, t1, (Eop ((Oleal (Ascaled (sc,
                              n0))), Enil)))
                          | Econs (t2, e4) ->
                            (match e4 with
                             | Enil -> Coq_addl_case4 (n, t1, sc, n0, t2)
                             | Econs (e5, e6) ->
                               Coq_addl_case8 (n, t1, (Eop ((Oleal (Ascaled
                                 (sc, n0))), (Econs (t2, (Econs (e5, e6)))))))))
                       | x -> Coq_addl_case8 (n, t1, (Eop ((Oleal x), e3))))
                    | x -> Coq_addl_case8 (n, t1, (Eop (x, e3))))
                 | x -> Coq_addl_case8 (n, t1, x))
              | Econs (e3, e4) ->
                let e5 = Eop ((Oleal (Aindexed n)), (Econs (t1, (Econs (e3,
                  e4)))))
                in
                (match e2 with
                 | Eop (o0, e6) ->
                   (match o0 with
                    | Olongconst n2 ->
                      (match e6 with
                       | Enil -> Coq_addl_case2 (e5, n2)
                       | Econs (e7, e8) ->
                         Coq_addl_default (e5, (Eop ((Olongconst n2), (Econs
                           (e7, e8))))))
                    | Oleal a0 ->
                      (match a0 with
                       | Aindexed n0 ->
                         (match e6 with
                          | Enil ->
                            Coq_addl_default (e5, (Eop ((Oleal (Aindexed
                              n0)), Enil)))
                          | Econs (t2, e7) ->
                            (match e7 with
                             | Enil -> Coq_addl_case9 (e5, n0, t2)
                             | Econs (e8, e9) ->
                               Coq_addl_default (e5, (Eop ((Oleal (Aindexed
                                 n0)), (Econs (t2, (Econs (e8, e9)))))))))
                       | Ascaled (sc, n0) ->
                         (match e6 with
                          | Enil ->
                            Coq_addl_default (e5, (Eop ((Oleal (Ascaled (sc,
                              n0))), Enil)))
                          | Econs (t2, e7) ->
                            (match e7 with
                             | Enil -> Coq_addl_case7 (e5, sc, n0, t2)
                             | Econs (e8, e9) ->
                               Coq_addl_default (e5, (Eop ((Oleal (Ascaled
                                 (sc, n0))), (Econs (t2, (Econs (e8, e9)))))))))
                       | x -> Coq_addl_default (e5, (Eop ((Oleal x), e6))))
                    | x -> Coq_addl_default (e5, (Eop (x, e6))))
                 | x -> Coq_addl_default (e5, x))))
        | Ascaled (sc, n) ->
          (match e with
           | Enil ->
             let e3 = Eop ((Oleal (Ascaled (sc, n))), Enil) in
             (match e2 with
              | Eop (o0, e0) ->
                (match o0 with
                 | Olongconst n2 ->
                   (match e0 with
                    | Enil -> Coq_addl_case2 (e3, n2)
                    | Econs (e4, e5) ->
                      Coq_addl_default (e3, (Eop ((Olongconst n2), (Econs
                        (e4, e5))))))
                 | Oleal a0 ->
                   (match a0 with
                    | Aindexed n0 ->
                      (match e0 with
                       | Enil ->
                         Coq_addl_default (e3, (Eop ((Oleal (Aindexed n0)),
                           Enil)))
                       | Econs (t2, e4) ->
                         (match e4 with
                          | Enil -> Coq_addl_case9 (e3, n0, t2)
                          | Econs (e5, e6) ->
                            Coq_addl_default (e3, (Eop ((Oleal (Aindexed
                              n0)), (Econs (t2, (Econs (e5, e6)))))))))
                    | Ascaled (sc0, n0) ->
                      (match e0 with
                       | Enil ->
                         Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc0,
                           n0))), Enil)))
                       | Econs (t2, e4) ->
                         (match e4 with
                          | Enil -> Coq_addl_case7 (e3, sc0, n0, t2)
                          | Econs (e5, e6) ->
                            Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc0,
                              n0))), (Econs (t2, (Econs (e5, e6)))))))))
                    | x -> Coq_addl_default (e3, (Eop ((Oleal x), e0))))
                 | x -> Coq_addl_default (e3, (Eop (x, e0))))
              | x -> Coq_addl_default (e3, x))
           | Econs (t1, e0) ->
             (match e0 with
              | Enil ->
                (match e2 with
                 | Eop (o0, e3) ->
                   (match o0 with
                    | Olongconst n2 ->
                      (match e3 with
                       | Enil ->
                         Coq_addl_case2 ((Eop ((Oleal (Ascaled (sc, n))),
                           (Econs (t1, Enil)))), n2)
                       | Econs (e4, e5) ->
                         Coq_addl_case6 (sc, n, t1, (Eop ((Olongconst n2),
                           (Econs (e4, e5))))))
                    | Oleal a0 ->
                      (match a0 with
                       | Aindexed n0 ->
                         (match e3 with
                          | Enil ->
                            Coq_addl_case6 (sc, n, t1, (Eop ((Oleal (Aindexed
                              n0)), Enil)))
                          | Econs (t2, e4) ->
                            (match e4 with
                             | Enil -> Coq_addl_case5 (sc, n, t1, n0, t2)
                             | Econs (e5, e6) ->
                               Coq_addl_case6 (sc, n, t1, (Eop ((Oleal
                                 (Aindexed n0)), (Econs (t2, (Econs (e5,
                                 e6)))))))))
                       | x ->
                         Coq_addl_case6 (sc, n, t1, (Eop ((Oleal x), e3))))
                    | x -> Coq_addl_case6 (sc, n, t1, (Eop (x, e3))))
                 | x -> Coq_addl_case6 (sc, n, t1, x))
              | Econs (e3, e4) ->
                let e5 = Eop ((Oleal (Ascaled (sc, n))), (Econs (t1, (Econs
                  (e3, e4)))))
                in
                (match e2 with
                 | Eop (o0, e6) ->
                   (match o0 with
                    | Olongconst n2 ->
                      (match e6 with
                       | Enil -> Coq_addl_case2 (e5, n2)
                       | Econs (e7, e8) ->
                         Coq_addl_default (e5, (Eop ((Olongconst n2), (Econs
                           (e7, e8))))))
                    | Oleal a0 ->
                      (match a0 with
                       | Aindexed n0 ->
                         (match e6 with
                          | Enil ->
                            Coq_addl_default (e5, (Eop ((Oleal (Aindexed
                              n0)), Enil)))
                          | Econs (t2, e7) ->
                            (match e7 with
                             | Enil -> Coq_addl_case9 (e5, n0, t2)
                             | Econs (e8, e9) ->
                               Coq_addl_default (e5, (Eop ((Oleal (Aindexed
                                 n0)), (Econs (t2, (Econs (e8, e9)))))))))
                       | Ascaled (sc0, n0) ->
                         (match e6 with
                          | Enil ->
                            Coq_addl_default (e5, (Eop ((Oleal (Ascaled (sc0,
                              n0))), Enil)))
                          | Econs (t2, e7) ->
                            (match e7 with
                             | Enil -> Coq_addl_case7 (e5, sc0, n0, t2)
                             | Econs (e8, e9) ->
                               Coq_addl_default (e5, (Eop ((Oleal (Ascaled
                                 (sc0, n0))), (Econs (t2, (Econs (e8,
                                 e9)))))))))
                       | x -> Coq_addl_default (e5, (Eop ((Oleal x), e6))))
                    | x -> Coq_addl_default (e5, (Eop (x, e6))))
                 | x -> Coq_addl_default (e5, x))))
        | x ->
          let e3 = Eop ((Oleal x), e) in
          (match e2 with
           | Eop (o0, e0) ->
             (match o0 with
              | Olongconst n2 ->
                (match e0 with
                 | Enil -> Coq_addl_case2 (e3, n2)
                 | Econs (e4, e5) ->
                   Coq_addl_default (e3, (Eop ((Olongconst n2), (Econs (e4,
                     e5))))))
              | Oleal a0 ->
                (match a0 with
                 | Aindexed n ->
                   (match e0 with
                    | Enil ->
                      Coq_addl_default (e3, (Eop ((Oleal (Aindexed n)),
                        Enil)))
                    | Econs (t2, e4) ->
                      (match e4 with
                       | Enil -> Coq_addl_case9 (e3, n, t2)
                       | Econs (e5, e6) ->
                         Coq_addl_default (e3, (Eop ((Oleal (Aindexed n)),
                           (Econs (t2, (Econs (e5, e6)))))))))
                 | Ascaled (sc, n) ->
                   (match e0 with
                    | Enil ->
                      Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc, n))),
                        Enil)))
                    | Econs (t2, e4) ->
                      (match e4 with
                       | Enil -> Coq_addl_case7 (e3, sc, n, t2)
                       | Econs (e5, e6) ->
                         Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc,
                           n))), (Econs (t2, (Econs (e5, e6)))))))))
                 | x0 -> Coq_addl_default (e3, (Eop ((Oleal x0), e0))))
              | x0 -> Coq_addl_default (e3, (Eop (x0, e0))))
           | x0 -> Coq_addl_default (e3, x0)))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Olongconst n2 ->
             (match e0 with
              | Enil -> Coq_addl_case2 (e3, n2)
              | Econs (e4, e5) ->
                Coq_addl_default (e3, (Eop ((Olongconst n2), (Econs (e4,
                  e5))))))
           | Oleal a ->
             (match a with
              | Aindexed n ->
                (match e0 with
                 | Enil ->
                   Coq_addl_default (e3, (Eop ((Oleal (Aindexed n)), Enil)))
                 | Econs (t2, e4) ->
                   (match e4 with
                    | Enil -> Coq_addl_case9 (e3, n, t2)
                    | Econs (e5, e6) ->
                      Coq_addl_default (e3, (Eop ((Oleal (Aindexed n)),
                        (Econs (t2, (Econs (e5, e6)))))))))
              | Ascaled (sc, n) ->
                (match e0 with
                 | Enil ->
                   Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc, n))),
                     Enil)))
                 | Econs (t2, e4) ->
                   (match e4 with
                    | Enil -> Coq_addl_case7 (e3, sc, n, t2)
                    | Econs (e5, e6) ->
                      Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc, n))),
                        (Econs (t2, (Econs (e5, e6)))))))))
              | x0 -> Coq_addl_default (e3, (Eop ((Oleal x0), e0))))
           | x0 -> Coq_addl_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_addl_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Olongconst n2 ->
          (match e0 with
           | Enil -> Coq_addl_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_addl_default (e3, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | Oleal a0 ->
          (match a0 with
           | Aindexed n ->
             (match e0 with
              | Enil ->
                Coq_addl_default (e3, (Eop ((Oleal (Aindexed n)), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_addl_case9 (e3, n, t2)
                 | Econs (e5, e6) ->
                   Coq_addl_default (e3, (Eop ((Oleal (Aindexed n)), (Econs
                     (t2, (Econs (e5, e6)))))))))
           | Ascaled (sc, n) ->
             (match e0 with
              | Enil ->
                Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc, n))), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_addl_case7 (e3, sc, n, t2)
                 | Econs (e5, e6) ->
                   Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc, n))),
                     (Econs (t2, (Econs (e5, e6)))))))))
           | x -> Coq_addl_default (e3, (Eop ((Oleal x), e0))))
        | x -> Coq_addl_default (e3, (Eop (x, e0))))
     | x -> Coq_addl_default (e3, x))
  | Eletvar n0 ->
    let e3 = Eletvar n0 in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Olongconst n2 ->
          (match e with
           | Enil -> Coq_addl_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_addl_default (e3, (Eop ((Olongconst n2), (Econs (e0, e4))))))
        | Oleal a ->
          (match a with
           | Aindexed n ->
             (match e with
              | Enil ->
                Coq_addl_default (e3, (Eop ((Oleal (Aindexed n)), Enil)))
              | Econs (t2, e0) ->
                (match e0 with
                 | Enil -> Coq_addl_case9 (e3, n, t2)
                 | Econs (e4, e5) ->
                   Coq_addl_default (e3, (Eop ((Oleal (Aindexed n)), (Econs
                     (t2, (Econs (e4, e5)))))))))
           | Ascaled (sc, n) ->
             (match e with
              | Enil ->
                Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc, n))), Enil)))
              | Econs (t2, e0) ->
                (match e0 with
                 | Enil -> Coq_addl_case7 (e3, sc, n, t2)
                 | Econs (e4, e5) ->
                   Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc, n))),
                     (Econs (t2, (Econs (e4, e5)))))))))
           | x -> Coq_addl_default (e3, (Eop ((Oleal x), e))))
        | x -> Coq_addl_default (e3, (Eop (x, e))))
     | x -> Coq_addl_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Olongconst n2 ->
          (match e0 with
           | Enil -> Coq_addl_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_addl_default (e3, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | Oleal a ->
          (match a with
           | Aindexed n ->
             (match e0 with
              | Enil ->
                Coq_addl_default (e3, (Eop ((Oleal (Aindexed n)), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_addl_case9 (e3, n, t2)
                 | Econs (e5, e6) ->
                   Coq_addl_default (e3, (Eop ((Oleal (Aindexed n)), (Econs
                     (t2, (Econs (e5, e6)))))))))
           | Ascaled (sc, n) ->
             (match e0 with
              | Enil ->
                Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc, n))), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_addl_case7 (e3, sc, n, t2)
                 | Econs (e5, e6) ->
                   Coq_addl_default (e3, (Eop ((Oleal (Ascaled (sc, n))),
                     (Econs (t2, (Econs (e5, e6)))))))))
           | x -> Coq_addl_default (e3, (Eop ((Oleal x), e0))))
        | x -> Coq_addl_default (e3, (Eop (x, e0))))
     | x -> Coq_addl_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Olongconst n2 ->
          (match e3 with
           | Enil -> Coq_addl_case2 (x, n2)
           | Econs (e4, e5) ->
             Coq_addl_default (x, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | Oleal a ->
          (match a with
           | Aindexed n ->
             (match e3 with
              | Enil ->
                Coq_addl_default (x, (Eop ((Oleal (Aindexed n)), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_addl_case9 (x, n, t2)
                 | Econs (e5, e6) ->
                   Coq_addl_default (x, (Eop ((Oleal (Aindexed n)), (Econs
                     (t2, (Econs (e5, e6)))))))))
           | Ascaled (sc, n) ->
             (match e3 with
              | Enil ->
                Coq_addl_default (x, (Eop ((Oleal (Ascaled (sc, n))), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_addl_case7 (x, sc, n, t2)
                 | Econs (e5, e6) ->
                   Coq_addl_default (x, (Eop ((Oleal (Ascaled (sc, n))),
                     (Econs (t2, (Econs (e5, e6)))))))))
           | x0 -> Coq_addl_default (x, (Eop ((Oleal x0), e3))))
        | x0 -> Coq_addl_default (x, (Eop (x0, e3))))
     | x0 -> Coq_addl_default (x, x0))

(** val addl : expr -> expr -> expr **)

let addl e1 e2 =
  if Archi.splitlong
  then addl e1 e2
  else (match addl_match e1 e2 with
        | Coq_addl_case1 (n1, t2) -> addlimm n1 t2
        | Coq_addl_case2 (t1, n2) -> addlimm n2 t1
        | Coq_addl_case3 (n1, t1, n2, t2) ->
          Eop ((Oleal (Aindexed2 (Z.add n1 n2))), (Econs (t1, (Econs (t2,
            Enil)))))
        | Coq_addl_case4 (n1, t1, sc, n2, t2) ->
          Eop ((Oleal (Aindexed2scaled (sc, (Z.add n1 n2)))), (Econs (t1,
            (Econs (t2, Enil)))))
        | Coq_addl_case5 (sc, n1, t1, n2, t2) ->
          Eop ((Oleal (Aindexed2scaled (sc, (Z.add n1 n2)))), (Econs (t2,
            (Econs (t1, Enil)))))
        | Coq_addl_case6 (sc, n, t1, t2) ->
          Eop ((Oleal (Aindexed2scaled (sc, n))), (Econs (t2, (Econs (t1,
            Enil)))))
        | Coq_addl_case7 (t1, sc, n, t2) ->
          Eop ((Oleal (Aindexed2scaled (sc, n))), (Econs (t1, (Econs (t2,
            Enil)))))
        | Coq_addl_case8 (n, t1, t2) ->
          Eop ((Oleal (Aindexed2 n)), (Econs (t1, (Econs (t2, Enil)))))
        | Coq_addl_case9 (t1, n, t2) ->
          Eop ((Oleal (Aindexed2 n)), (Econs (t1, (Econs (t2, Enil)))))
        | Coq_addl_default (e3, e4) ->
          Eop ((Oleal (Aindexed2 Z0)), (Econs (e3, (Econs (e4, Enil))))))

(** val negl : expr -> expr **)

let negl e =
  if Archi.splitlong
  then negl e
  else (match is_longconst e with
        | Some n -> longconst (Int64.neg n)
        | None -> Eop (Onegl, (Econs (e, Enil))))

type subl_cases =
| Coq_subl_case1 of expr * Int64.int
| Coq_subl_case2 of coq_Z * expr * coq_Z * expr
| Coq_subl_case3 of coq_Z * expr * expr
| Coq_subl_case4 of expr * coq_Z * expr
| Coq_subl_default of expr * expr

(** val subl_match : expr -> expr -> subl_cases **)

let subl_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Olongconst n2 ->
          (match e with
           | Enil -> Coq_subl_case1 (e3, n2)
           | Econs (e0, e4) ->
             Coq_subl_default (e3, (Eop ((Olongconst n2), (Econs (e0, e4))))))
        | Oleal a ->
          (match a with
           | Aindexed n2 ->
             (match e with
              | Enil ->
                Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)), Enil)))
              | Econs (t2, e0) ->
                (match e0 with
                 | Enil -> Coq_subl_case4 (e3, n2, t2)
                 | Econs (e4, e5) ->
                   Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)), (Econs
                     (t2, (Econs (e4, e5)))))))))
           | x -> Coq_subl_default (e3, (Eop ((Oleal x), e))))
        | x -> Coq_subl_default (e3, (Eop (x, e))))
     | x -> Coq_subl_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Olea a ->
       let e3 = Eop ((Olea a), e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Olongconst n2 ->
             (match e0 with
              | Enil -> Coq_subl_case1 (e3, n2)
              | Econs (e4, e5) ->
                Coq_subl_default (e3, (Eop ((Olongconst n2), (Econs (e4,
                  e5))))))
           | Oleal a0 ->
             (match a0 with
              | Aindexed n2 ->
                (match e0 with
                 | Enil ->
                   Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)), Enil)))
                 | Econs (t2, e4) ->
                   (match e4 with
                    | Enil -> Coq_subl_case4 (e3, n2, t2)
                    | Econs (e5, e6) ->
                      Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)),
                        (Econs (t2, (Econs (e5, e6)))))))))
              | x -> Coq_subl_default (e3, (Eop ((Oleal x), e0))))
           | x -> Coq_subl_default (e3, (Eop (x, e0))))
        | x -> Coq_subl_default (e3, x))
     | Oleal a ->
       (match a with
        | Aindexed n1 ->
          (match e with
           | Enil ->
             let e3 = Eop ((Oleal (Aindexed n1)), Enil) in
             (match e2 with
              | Eop (o0, e0) ->
                (match o0 with
                 | Olongconst n2 ->
                   (match e0 with
                    | Enil -> Coq_subl_case1 (e3, n2)
                    | Econs (e4, e5) ->
                      Coq_subl_default (e3, (Eop ((Olongconst n2), (Econs
                        (e4, e5))))))
                 | Oleal a0 ->
                   (match a0 with
                    | Aindexed n2 ->
                      (match e0 with
                       | Enil ->
                         Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)),
                           Enil)))
                       | Econs (t2, e4) ->
                         (match e4 with
                          | Enil -> Coq_subl_case4 (e3, n2, t2)
                          | Econs (e5, e6) ->
                            Coq_subl_default (e3, (Eop ((Oleal (Aindexed
                              n2)), (Econs (t2, (Econs (e5, e6)))))))))
                    | x -> Coq_subl_default (e3, (Eop ((Oleal x), e0))))
                 | x -> Coq_subl_default (e3, (Eop (x, e0))))
              | x -> Coq_subl_default (e3, x))
           | Econs (t1, e0) ->
             (match e0 with
              | Enil ->
                (match e2 with
                 | Eop (o0, e3) ->
                   (match o0 with
                    | Olongconst n2 ->
                      (match e3 with
                       | Enil ->
                         Coq_subl_case1 ((Eop ((Oleal (Aindexed n1)), (Econs
                           (t1, Enil)))), n2)
                       | Econs (e4, e5) ->
                         Coq_subl_case3 (n1, t1, (Eop ((Olongconst n2),
                           (Econs (e4, e5))))))
                    | Oleal a0 ->
                      (match a0 with
                       | Aindexed n2 ->
                         (match e3 with
                          | Enil ->
                            Coq_subl_case3 (n1, t1, (Eop ((Oleal (Aindexed
                              n2)), Enil)))
                          | Econs (t2, e4) ->
                            (match e4 with
                             | Enil -> Coq_subl_case2 (n1, t1, n2, t2)
                             | Econs (e5, e6) ->
                               Coq_subl_case3 (n1, t1, (Eop ((Oleal (Aindexed
                                 n2)), (Econs (t2, (Econs (e5, e6)))))))))
                       | x -> Coq_subl_case3 (n1, t1, (Eop ((Oleal x), e3))))
                    | x -> Coq_subl_case3 (n1, t1, (Eop (x, e3))))
                 | x -> Coq_subl_case3 (n1, t1, x))
              | Econs (e3, e4) ->
                let e5 = Eop ((Oleal (Aindexed n1)), (Econs (t1, (Econs (e3,
                  e4)))))
                in
                (match e2 with
                 | Eop (o0, e6) ->
                   (match o0 with
                    | Olongconst n2 ->
                      (match e6 with
                       | Enil -> Coq_subl_case1 (e5, n2)
                       | Econs (e7, e8) ->
                         Coq_subl_default (e5, (Eop ((Olongconst n2), (Econs
                           (e7, e8))))))
                    | Oleal a0 ->
                      (match a0 with
                       | Aindexed n2 ->
                         (match e6 with
                          | Enil ->
                            Coq_subl_default (e5, (Eop ((Oleal (Aindexed
                              n2)), Enil)))
                          | Econs (t2, e7) ->
                            (match e7 with
                             | Enil -> Coq_subl_case4 (e5, n2, t2)
                             | Econs (e8, e9) ->
                               Coq_subl_default (e5, (Eop ((Oleal (Aindexed
                                 n2)), (Econs (t2, (Econs (e8, e9)))))))))
                       | x -> Coq_subl_default (e5, (Eop ((Oleal x), e6))))
                    | x -> Coq_subl_default (e5, (Eop (x, e6))))
                 | x -> Coq_subl_default (e5, x))))
        | x ->
          let e3 = Eop ((Oleal x), e) in
          (match e2 with
           | Eop (o0, e0) ->
             (match o0 with
              | Olongconst n2 ->
                (match e0 with
                 | Enil -> Coq_subl_case1 (e3, n2)
                 | Econs (e4, e5) ->
                   Coq_subl_default (e3, (Eop ((Olongconst n2), (Econs (e4,
                     e5))))))
              | Oleal a0 ->
                (match a0 with
                 | Aindexed n2 ->
                   (match e0 with
                    | Enil ->
                      Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)),
                        Enil)))
                    | Econs (t2, e4) ->
                      (match e4 with
                       | Enil -> Coq_subl_case4 (e3, n2, t2)
                       | Econs (e5, e6) ->
                         Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)),
                           (Econs (t2, (Econs (e5, e6)))))))))
                 | x0 -> Coq_subl_default (e3, (Eop ((Oleal x0), e0))))
              | x0 -> Coq_subl_default (e3, (Eop (x0, e0))))
           | x0 -> Coq_subl_default (e3, x0)))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Olongconst n2 ->
             (match e0 with
              | Enil -> Coq_subl_case1 (e3, n2)
              | Econs (e4, e5) ->
                Coq_subl_default (e3, (Eop ((Olongconst n2), (Econs (e4,
                  e5))))))
           | Oleal a ->
             (match a with
              | Aindexed n2 ->
                (match e0 with
                 | Enil ->
                   Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)), Enil)))
                 | Econs (t2, e4) ->
                   (match e4 with
                    | Enil -> Coq_subl_case4 (e3, n2, t2)
                    | Econs (e5, e6) ->
                      Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)),
                        (Econs (t2, (Econs (e5, e6)))))))))
              | x0 -> Coq_subl_default (e3, (Eop ((Oleal x0), e0))))
           | x0 -> Coq_subl_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_subl_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Olongconst n2 ->
          (match e0 with
           | Enil -> Coq_subl_case1 (e3, n2)
           | Econs (e4, e5) ->
             Coq_subl_default (e3, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | Oleal a0 ->
          (match a0 with
           | Aindexed n2 ->
             (match e0 with
              | Enil ->
                Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_subl_case4 (e3, n2, t2)
                 | Econs (e5, e6) ->
                   Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)), (Econs
                     (t2, (Econs (e5, e6)))))))))
           | x -> Coq_subl_default (e3, (Eop ((Oleal x), e0))))
        | x -> Coq_subl_default (e3, (Eop (x, e0))))
     | x -> Coq_subl_default (e3, x))
  | Eletvar n ->
    let e3 = Eletvar n in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Olongconst n2 ->
          (match e with
           | Enil -> Coq_subl_case1 (e3, n2)
           | Econs (e0, e4) ->
             Coq_subl_default (e3, (Eop ((Olongconst n2), (Econs (e0, e4))))))
        | Oleal a ->
          (match a with
           | Aindexed n2 ->
             (match e with
              | Enil ->
                Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)), Enil)))
              | Econs (t2, e0) ->
                (match e0 with
                 | Enil -> Coq_subl_case4 (e3, n2, t2)
                 | Econs (e4, e5) ->
                   Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)), (Econs
                     (t2, (Econs (e4, e5)))))))))
           | x -> Coq_subl_default (e3, (Eop ((Oleal x), e))))
        | x -> Coq_subl_default (e3, (Eop (x, e))))
     | x -> Coq_subl_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Olongconst n2 ->
          (match e0 with
           | Enil -> Coq_subl_case1 (e3, n2)
           | Econs (e4, e5) ->
             Coq_subl_default (e3, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | Oleal a ->
          (match a with
           | Aindexed n2 ->
             (match e0 with
              | Enil ->
                Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_subl_case4 (e3, n2, t2)
                 | Econs (e5, e6) ->
                   Coq_subl_default (e3, (Eop ((Oleal (Aindexed n2)), (Econs
                     (t2, (Econs (e5, e6)))))))))
           | x -> Coq_subl_default (e3, (Eop ((Oleal x), e0))))
        | x -> Coq_subl_default (e3, (Eop (x, e0))))
     | x -> Coq_subl_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Olongconst n2 ->
          (match e3 with
           | Enil -> Coq_subl_case1 (x, n2)
           | Econs (e4, e5) ->
             Coq_subl_default (x, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | Oleal a ->
          (match a with
           | Aindexed n2 ->
             (match e3 with
              | Enil ->
                Coq_subl_default (x, (Eop ((Oleal (Aindexed n2)), Enil)))
              | Econs (t2, e4) ->
                (match e4 with
                 | Enil -> Coq_subl_case4 (x, n2, t2)
                 | Econs (e5, e6) ->
                   Coq_subl_default (x, (Eop ((Oleal (Aindexed n2)), (Econs
                     (t2, (Econs (e5, e6)))))))))
           | x0 -> Coq_subl_default (x, (Eop ((Oleal x0), e3))))
        | x0 -> Coq_subl_default (x, (Eop (x0, e3))))
     | x0 -> Coq_subl_default (x, x0))

(** val subl : expr -> expr -> expr **)

let subl e1 e2 =
  if Archi.splitlong
  then subl e1 e2
  else (match subl_match e1 e2 with
        | Coq_subl_case1 (t1, n2) -> addlimm (Int64.neg n2) t1
        | Coq_subl_case2 (n1, t1, n2, t2) ->
          addlimm (Int64.repr (Z.sub n1 n2)) (Eop (Osubl, (Econs (t1, (Econs
            (t2, Enil))))))
        | Coq_subl_case3 (n1, t1, t2) ->
          addlimm (Int64.repr n1) (Eop (Osubl, (Econs (t1, (Econs (t2,
            Enil))))))
        | Coq_subl_case4 (t1, n2, t2) ->
          addlimm (Int64.repr (Z.opp n2)) (Eop (Osubl, (Econs (t1, (Econs
            (t2, Enil))))))
        | Coq_subl_default (e3, e4) ->
          Eop (Osubl, (Econs (e3, (Econs (e4, Enil))))))

(** val mullimm_base : helper_functions -> Int64.int -> expr -> expr **)

let mullimm_base hf n1 e2 =
  match Int64.one_bits' n1 with
  | [] -> Eop ((Omullimm n1), (Econs (e2, Enil)))
  | i :: l ->
    (match l with
     | [] -> shllimm hf e2 i
     | j :: l0 ->
       (match l0 with
        | [] ->
          Elet (e2,
            (addl (shllimm hf (Eletvar O) i) (shllimm hf (Eletvar O) j)))
        | _ :: _ -> Eop ((Omullimm n1), (Econs (e2, Enil)))))

type mullimm_cases =
| Coq_mullimm_case1 of Int64.int
| Coq_mullimm_case2 of coq_Z * expr
| Coq_mullimm_default of expr

(** val mullimm_match : expr -> mullimm_cases **)

let mullimm_match = function
| Eop (o, e) ->
  (match o with
   | Olongconst n2 ->
     (match e with
      | Enil -> Coq_mullimm_case1 n2
      | Econs (e0, e1) ->
        Coq_mullimm_default (Eop ((Olongconst n2), (Econs (e0, e1)))))
   | Oleal a ->
     (match a with
      | Aindexed n2 ->
        (match e with
         | Enil -> Coq_mullimm_default (Eop ((Oleal (Aindexed n2)), Enil))
         | Econs (t2, e0) ->
           (match e0 with
            | Enil -> Coq_mullimm_case2 (n2, t2)
            | Econs (e1, e3) ->
              Coq_mullimm_default (Eop ((Oleal (Aindexed n2)), (Econs (t2,
                (Econs (e1, e3))))))))
      | x -> Coq_mullimm_default (Eop ((Oleal x), e)))
   | x -> Coq_mullimm_default (Eop (x, e)))
| x -> Coq_mullimm_default x

(** val mullimm : helper_functions -> Int64.int -> expr -> expr **)

let mullimm hf n1 e2 =
  if Archi.splitlong
  then mullimm hf n1 e2
  else if Int64.eq n1 Int64.zero
       then longconst Int64.zero
       else if Int64.eq n1 Int64.one
            then e2
            else (match mullimm_match e2 with
                  | Coq_mullimm_case1 n2 -> longconst (Int64.mul n1 n2)
                  | Coq_mullimm_case2 (n2, t2) ->
                    addlimm (Int64.mul n1 (Int64.repr n2))
                      (mullimm_base hf n1 t2)
                  | Coq_mullimm_default e3 -> mullimm_base hf n1 e3)

type mull_cases =
| Coq_mull_case1 of Int64.int * expr
| Coq_mull_case2 of expr * Int64.int
| Coq_mull_default of expr * expr

(** val mull_match : expr -> expr -> mull_cases **)

let mull_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Olongconst n2 ->
          (match e with
           | Enil -> Coq_mull_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_mull_default (e3, (Eop ((Olongconst n2), (Econs (e0, e4))))))
        | x -> Coq_mull_default (e3, (Eop (x, e))))
     | x -> Coq_mull_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Olongconst n1 ->
       (match e with
        | Enil -> Coq_mull_case1 (n1, e2)
        | Econs (e0, e3) ->
          let e4 = Eop ((Olongconst n1), (Econs (e0, e3))) in
          (match e2 with
           | Eop (o0, e5) ->
             (match o0 with
              | Olongconst n2 ->
                (match e5 with
                 | Enil -> Coq_mull_case2 (e4, n2)
                 | Econs (e6, e7) ->
                   Coq_mull_default (e4, (Eop ((Olongconst n2), (Econs (e6,
                     e7))))))
              | x -> Coq_mull_default (e4, (Eop (x, e5))))
           | x -> Coq_mull_default (e4, x)))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Olongconst n2 ->
             (match e0 with
              | Enil -> Coq_mull_case2 (e3, n2)
              | Econs (e4, e5) ->
                Coq_mull_default (e3, (Eop ((Olongconst n2), (Econs (e4,
                  e5))))))
           | x0 -> Coq_mull_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_mull_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Olongconst n2 ->
          (match e0 with
           | Enil -> Coq_mull_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_mull_default (e3, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | x -> Coq_mull_default (e3, (Eop (x, e0))))
     | x -> Coq_mull_default (e3, x))
  | Eletvar n ->
    let e3 = Eletvar n in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Olongconst n2 ->
          (match e with
           | Enil -> Coq_mull_case2 (e3, n2)
           | Econs (e0, e4) ->
             Coq_mull_default (e3, (Eop ((Olongconst n2), (Econs (e0, e4))))))
        | x -> Coq_mull_default (e3, (Eop (x, e))))
     | x -> Coq_mull_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Olongconst n2 ->
          (match e0 with
           | Enil -> Coq_mull_case2 (e3, n2)
           | Econs (e4, e5) ->
             Coq_mull_default (e3, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | x -> Coq_mull_default (e3, (Eop (x, e0))))
     | x -> Coq_mull_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Olongconst n2 ->
          (match e3 with
           | Enil -> Coq_mull_case2 (x, n2)
           | Econs (e4, e5) ->
             Coq_mull_default (x, (Eop ((Olongconst n2), (Econs (e4, e5))))))
        | x0 -> Coq_mull_default (x, (Eop (x0, e3))))
     | x0 -> Coq_mull_default (x, x0))

(** val mull : helper_functions -> expr -> expr -> expr **)

let mull hf e1 e2 =
  if Archi.splitlong
  then mull hf e1 e2
  else (match mull_match e1 e2 with
        | Coq_mull_case1 (n1, t2) -> mullimm hf n1 t2
        | Coq_mull_case2 (t1, n2) -> mullimm hf n2 t1
        | Coq_mull_default (e3, e4) ->
          Eop (Omull, (Econs (e3, (Econs (e4, Enil))))))

(** val mullhu : helper_functions -> expr -> Int64.int -> expr **)

let mullhu hf e1 n2 =
  if Archi.splitlong
  then mullhu hf e1 n2
  else Eop (Omullhu, (Econs (e1, (Econs ((longconst n2), Enil)))))

(** val mullhs : helper_functions -> expr -> Int64.int -> expr **)

let mullhs hf e1 n2 =
  if Archi.splitlong
  then mullhs hf e1 n2
  else Eop (Omullhs, (Econs (e1, (Econs ((longconst n2), Enil)))))

(** val shrxlimm : helper_functions -> expr -> Int.int -> expr **)

let shrxlimm hf e n =
  if Archi.splitlong
  then shrxlimm hf e n
  else if Int.eq n Int.zero then e else Eop ((Oshrxlimm n), (Econs (e, Enil)))

(** val divlu_base : helper_functions -> expr -> expr -> expr **)

let divlu_base hf e1 e2 =
  if Archi.splitlong
  then divlu_base hf e1 e2
  else Eop (Odivlu, (Econs (e1, (Econs (e2, Enil)))))

(** val modlu_base : helper_functions -> expr -> expr -> expr **)

let modlu_base hf e1 e2 =
  if Archi.splitlong
  then modlu_base hf e1 e2
  else Eop (Omodlu, (Econs (e1, (Econs (e2, Enil)))))

(** val divls_base : helper_functions -> expr -> expr -> expr **)

let divls_base hf e1 e2 =
  if Archi.splitlong
  then divls_base hf e1 e2
  else Eop (Odivl, (Econs (e1, (Econs (e2, Enil)))))

(** val modls_base : helper_functions -> expr -> expr -> expr **)

let modls_base hf e1 e2 =
  if Archi.splitlong
  then modls_base hf e1 e2
  else Eop (Omodl, (Econs (e1, (Econs (e2, Enil)))))

(** val cmplu : comparison -> expr -> expr -> expr **)

let cmplu c e1 e2 =
  if Archi.splitlong
  then cmplu c e1 e2
  else (match is_longconst e1 with
        | Some n1 ->
          (match is_longconst e2 with
           | Some n2 ->
             Eop ((Ointconst
               (if Int64.cmpu c n1 n2 then Int.one else Int.zero)), Enil)
           | None ->
             Eop ((Ocmp (Ccompluimm ((swap_comparison c), n1))), (Econs (e2,
               Enil))))
        | None ->
          (match is_longconst e2 with
           | Some n2 -> Eop ((Ocmp (Ccompluimm (c, n2))), (Econs (e1, Enil)))
           | None ->
             Eop ((Ocmp (Ccomplu c)), (Econs (e1, (Econs (e2, Enil)))))))

(** val cmpl : comparison -> expr -> expr -> expr **)

let cmpl c e1 e2 =
  if Archi.splitlong
  then cmpl c e1 e2
  else (match is_longconst e1 with
        | Some n1 ->
          (match is_longconst e2 with
           | Some n2 ->
             Eop ((Ointconst
               (if Int64.cmp c n1 n2 then Int.one else Int.zero)), Enil)
           | None ->
             Eop ((Ocmp (Ccomplimm ((swap_comparison c), n1))), (Econs (e2,
               Enil))))
        | None ->
          (match is_longconst e2 with
           | Some n2 -> Eop ((Ocmp (Ccomplimm (c, n2))), (Econs (e1, Enil)))
           | None -> Eop ((Ocmp (Ccompl c)), (Econs (e1, (Econs (e2, Enil)))))))

(** val longoffloat : helper_functions -> expr -> expr **)

let longoffloat hf e =
  if Archi.splitlong
  then longoffloat hf e
  else Eop (Olongoffloat, (Econs (e, Enil)))

(** val floatoflong : helper_functions -> expr -> expr **)

let floatoflong hf e =
  if Archi.splitlong
  then floatoflong hf e
  else Eop (Ofloatoflong, (Econs (e, Enil)))

(** val longofsingle : helper_functions -> expr -> expr **)

let longofsingle hf e =
  if Archi.splitlong
  then longofsingle hf e
  else Eop (Olongofsingle, (Econs (e, Enil)))

(** val singleoflong : helper_functions -> expr -> expr **)

let singleoflong hf e =
  if Archi.splitlong
  then singleoflong hf e
  else Eop (Osingleoflong, (Econs (e, Enil)))
