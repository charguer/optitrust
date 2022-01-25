open AST
open BinNums
open CminorSel
open Datatypes
open Integers
open Op
open SelectOp

type helper_functions = { i64_dtos : ident; i64_dtou : ident;
                          i64_stod : ident; i64_utod : ident;
                          i64_stof : ident; i64_utof : ident;
                          i64_sdiv : ident; i64_udiv : ident;
                          i64_smod : ident; i64_umod : ident;
                          i64_shl : ident; i64_shr : ident; i64_sar : 
                          ident; i64_umulh : ident; i64_smulh : ident }

(** val sig_l_l : signature **)

let sig_l_l =
  { sig_args = (Tlong :: []); sig_res = (Tret Tlong); sig_cc = cc_default }

(** val sig_l_f : signature **)

let sig_l_f =
  { sig_args = (Tlong :: []); sig_res = (Tret Tfloat); sig_cc = cc_default }

(** val sig_l_s : signature **)

let sig_l_s =
  { sig_args = (Tlong :: []); sig_res = (Tret Tsingle); sig_cc = cc_default }

(** val sig_f_l : signature **)

let sig_f_l =
  { sig_args = (Tfloat :: []); sig_res = (Tret Tlong); sig_cc = cc_default }

(** val sig_ll_l : signature **)

let sig_ll_l =
  { sig_args = (Tlong :: (Tlong :: [])); sig_res = (Tret Tlong); sig_cc =
    cc_default }

(** val sig_li_l : signature **)

let sig_li_l =
  { sig_args = (Tlong :: (Tint :: [])); sig_res = (Tret Tlong); sig_cc =
    cc_default }

(** val sig_ii_l : signature **)

let sig_ii_l =
  { sig_args = (Tint :: (Tint :: [])); sig_res = (Tret Tlong); sig_cc =
    cc_default }

(** val makelong : expr -> expr -> expr **)

let makelong h l =
  Eop (Omakelong, (Econs (h, (Econs (l, Enil)))))

type splitlong_cases =
| Coq_splitlong_case1 of expr * expr
| Coq_splitlong_default of expr

(** val splitlong_match : expr -> splitlong_cases **)

let splitlong_match = function
| Eop (o, e0) ->
  (match o with
   | Omakelong ->
     (match e0 with
      | Enil -> Coq_splitlong_default (Eop (Omakelong, Enil))
      | Econs (h, e1) ->
        (match e1 with
         | Enil -> Coq_splitlong_default (Eop (Omakelong, (Econs (h, Enil))))
         | Econs (l, e2) ->
           (match e2 with
            | Enil -> Coq_splitlong_case1 (h, l)
            | Econs (e3, e4) ->
              Coq_splitlong_default (Eop (Omakelong, (Econs (h, (Econs (l,
                (Econs (e3, e4)))))))))))
   | x -> Coq_splitlong_default (Eop (x, e0)))
| x -> Coq_splitlong_default x

(** val splitlong : expr -> (expr -> expr -> expr) -> expr **)

let splitlong e f =
  match splitlong_match e with
  | Coq_splitlong_case1 (h, l) -> f h l
  | Coq_splitlong_default e0 ->
    Elet (e0,
      (f (Eop (Ohighlong, (Econs ((Eletvar O), Enil)))) (Eop (Olowlong,
        (Econs ((Eletvar O), Enil))))))

type splitlong2_cases =
| Coq_splitlong2_case1 of expr * expr * expr * expr
| Coq_splitlong2_case2 of expr * expr * expr
| Coq_splitlong2_case3 of expr * expr * expr
| Coq_splitlong2_default of expr * expr

(** val splitlong2_match : expr -> expr -> splitlong2_cases **)

let splitlong2_match e1 e2 =
  match e1 with
  | Evar i ->
    let e3 = Evar i in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Omakelong ->
          (match e with
           | Enil -> Coq_splitlong2_default (e3, (Eop (Omakelong, Enil)))
           | Econs (h2, e0) ->
             (match e0 with
              | Enil ->
                Coq_splitlong2_default (e3, (Eop (Omakelong, (Econs (h2,
                  Enil)))))
              | Econs (l2, e4) ->
                (match e4 with
                 | Enil -> Coq_splitlong2_case3 (e3, h2, l2)
                 | Econs (e5, e6) ->
                   Coq_splitlong2_default (e3, (Eop (Omakelong, (Econs (h2,
                     (Econs (l2, (Econs (e5, e6))))))))))))
        | x -> Coq_splitlong2_default (e3, (Eop (x, e))))
     | x -> Coq_splitlong2_default (e3, x))
  | Eop (o, e) ->
    (match o with
     | Omakelong ->
       (match e with
        | Enil ->
          let e3 = Eop (Omakelong, Enil) in
          (match e2 with
           | Eop (o0, e0) ->
             (match o0 with
              | Omakelong ->
                (match e0 with
                 | Enil ->
                   Coq_splitlong2_default (e3, (Eop (Omakelong, Enil)))
                 | Econs (h2, e4) ->
                   (match e4 with
                    | Enil ->
                      Coq_splitlong2_default (e3, (Eop (Omakelong, (Econs
                        (h2, Enil)))))
                    | Econs (l2, e5) ->
                      (match e5 with
                       | Enil -> Coq_splitlong2_case3 (e3, h2, l2)
                       | Econs (e6, e7) ->
                         Coq_splitlong2_default (e3, (Eop (Omakelong, (Econs
                           (h2, (Econs (l2, (Econs (e6, e7))))))))))))
              | x -> Coq_splitlong2_default (e3, (Eop (x, e0))))
           | x -> Coq_splitlong2_default (e3, x))
        | Econs (h1, e0) ->
          (match e0 with
           | Enil ->
             let e3 = Eop (Omakelong, (Econs (h1, Enil))) in
             (match e2 with
              | Eop (o0, e4) ->
                (match o0 with
                 | Omakelong ->
                   (match e4 with
                    | Enil ->
                      Coq_splitlong2_default (e3, (Eop (Omakelong, Enil)))
                    | Econs (h2, e5) ->
                      (match e5 with
                       | Enil ->
                         Coq_splitlong2_default (e3, (Eop (Omakelong, (Econs
                           (h2, Enil)))))
                       | Econs (l2, e6) ->
                         (match e6 with
                          | Enil -> Coq_splitlong2_case3 (e3, h2, l2)
                          | Econs (e7, e8) ->
                            Coq_splitlong2_default (e3, (Eop (Omakelong,
                              (Econs (h2, (Econs (l2, (Econs (e7, e8))))))))))))
                 | x -> Coq_splitlong2_default (e3, (Eop (x, e4))))
              | x -> Coq_splitlong2_default (e3, x))
           | Econs (l1, e3) ->
             (match e3 with
              | Enil ->
                (match e2 with
                 | Eop (o0, e4) ->
                   (match o0 with
                    | Omakelong ->
                      (match e4 with
                       | Enil ->
                         Coq_splitlong2_case2 (h1, l1, (Eop (Omakelong,
                           Enil)))
                       | Econs (h2, e5) ->
                         (match e5 with
                          | Enil ->
                            Coq_splitlong2_case2 (h1, l1, (Eop (Omakelong,
                              (Econs (h2, Enil)))))
                          | Econs (l2, e6) ->
                            (match e6 with
                             | Enil -> Coq_splitlong2_case1 (h1, l1, h2, l2)
                             | Econs (e7, e8) ->
                               Coq_splitlong2_case2 (h1, l1, (Eop (Omakelong,
                                 (Econs (h2, (Econs (l2, (Econs (e7,
                                 e8))))))))))))
                    | x -> Coq_splitlong2_case2 (h1, l1, (Eop (x, e4))))
                 | x -> Coq_splitlong2_case2 (h1, l1, x))
              | Econs (e4, e5) ->
                let e6 = Eop (Omakelong, (Econs (h1, (Econs (l1, (Econs (e4,
                  e5)))))))
                in
                (match e2 with
                 | Eop (o0, e7) ->
                   (match o0 with
                    | Omakelong ->
                      (match e7 with
                       | Enil ->
                         Coq_splitlong2_default (e6, (Eop (Omakelong, Enil)))
                       | Econs (h2, e8) ->
                         (match e8 with
                          | Enil ->
                            Coq_splitlong2_default (e6, (Eop (Omakelong,
                              (Econs (h2, Enil)))))
                          | Econs (l2, e9) ->
                            (match e9 with
                             | Enil -> Coq_splitlong2_case3 (e6, h2, l2)
                             | Econs (e10, e11) ->
                               Coq_splitlong2_default (e6, (Eop (Omakelong,
                                 (Econs (h2, (Econs (l2, (Econs (e10,
                                 e11))))))))))))
                    | x -> Coq_splitlong2_default (e6, (Eop (x, e7))))
                 | x -> Coq_splitlong2_default (e6, x)))))
     | x ->
       let e3 = Eop (x, e) in
       (match e2 with
        | Eop (o0, e0) ->
          (match o0 with
           | Omakelong ->
             (match e0 with
              | Enil -> Coq_splitlong2_default (e3, (Eop (Omakelong, Enil)))
              | Econs (h2, e4) ->
                (match e4 with
                 | Enil ->
                   Coq_splitlong2_default (e3, (Eop (Omakelong, (Econs (h2,
                     Enil)))))
                 | Econs (l2, e5) ->
                   (match e5 with
                    | Enil -> Coq_splitlong2_case3 (e3, h2, l2)
                    | Econs (e6, e7) ->
                      Coq_splitlong2_default (e3, (Eop (Omakelong, (Econs
                        (h2, (Econs (l2, (Econs (e6, e7))))))))))))
           | x0 -> Coq_splitlong2_default (e3, (Eop (x0, e0))))
        | x0 -> Coq_splitlong2_default (e3, x0)))
  | Eload (m, a, e) ->
    let e3 = Eload (m, a, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Omakelong ->
          (match e0 with
           | Enil -> Coq_splitlong2_default (e3, (Eop (Omakelong, Enil)))
           | Econs (h2, e4) ->
             (match e4 with
              | Enil ->
                Coq_splitlong2_default (e3, (Eop (Omakelong, (Econs (h2,
                  Enil)))))
              | Econs (l2, e5) ->
                (match e5 with
                 | Enil -> Coq_splitlong2_case3 (e3, h2, l2)
                 | Econs (e6, e7) ->
                   Coq_splitlong2_default (e3, (Eop (Omakelong, (Econs (h2,
                     (Econs (l2, (Econs (e6, e7))))))))))))
        | x -> Coq_splitlong2_default (e3, (Eop (x, e0))))
     | x -> Coq_splitlong2_default (e3, x))
  | Eletvar n ->
    let e3 = Eletvar n in
    (match e2 with
     | Eop (o, e) ->
       (match o with
        | Omakelong ->
          (match e with
           | Enil -> Coq_splitlong2_default (e3, (Eop (Omakelong, Enil)))
           | Econs (h2, e0) ->
             (match e0 with
              | Enil ->
                Coq_splitlong2_default (e3, (Eop (Omakelong, (Econs (h2,
                  Enil)))))
              | Econs (l2, e4) ->
                (match e4 with
                 | Enil -> Coq_splitlong2_case3 (e3, h2, l2)
                 | Econs (e5, e6) ->
                   Coq_splitlong2_default (e3, (Eop (Omakelong, (Econs (h2,
                     (Econs (l2, (Econs (e5, e6))))))))))))
        | x -> Coq_splitlong2_default (e3, (Eop (x, e))))
     | x -> Coq_splitlong2_default (e3, x))
  | Eexternal (i, s, e) ->
    let e3 = Eexternal (i, s, e) in
    (match e2 with
     | Eop (o, e0) ->
       (match o with
        | Omakelong ->
          (match e0 with
           | Enil -> Coq_splitlong2_default (e3, (Eop (Omakelong, Enil)))
           | Econs (h2, e4) ->
             (match e4 with
              | Enil ->
                Coq_splitlong2_default (e3, (Eop (Omakelong, (Econs (h2,
                  Enil)))))
              | Econs (l2, e5) ->
                (match e5 with
                 | Enil -> Coq_splitlong2_case3 (e3, h2, l2)
                 | Econs (e6, e7) ->
                   Coq_splitlong2_default (e3, (Eop (Omakelong, (Econs (h2,
                     (Econs (l2, (Econs (e6, e7))))))))))))
        | x -> Coq_splitlong2_default (e3, (Eop (x, e0))))
     | x -> Coq_splitlong2_default (e3, x))
  | x ->
    (match e2 with
     | Eop (o, e3) ->
       (match o with
        | Omakelong ->
          (match e3 with
           | Enil -> Coq_splitlong2_default (x, (Eop (Omakelong, Enil)))
           | Econs (h2, e4) ->
             (match e4 with
              | Enil ->
                Coq_splitlong2_default (x, (Eop (Omakelong, (Econs (h2,
                  Enil)))))
              | Econs (l2, e5) ->
                (match e5 with
                 | Enil -> Coq_splitlong2_case3 (x, h2, l2)
                 | Econs (e6, e7) ->
                   Coq_splitlong2_default (x, (Eop (Omakelong, (Econs (h2,
                     (Econs (l2, (Econs (e6, e7))))))))))))
        | x0 -> Coq_splitlong2_default (x, (Eop (x0, e3))))
     | x0 -> Coq_splitlong2_default (x, x0))

(** val splitlong2 :
    expr -> expr -> (expr -> expr -> expr -> expr -> expr) -> expr **)

let splitlong2 e1 e2 f =
  match splitlong2_match e1 e2 with
  | Coq_splitlong2_case1 (h1, l1, h2, l2) -> f h1 l1 h2 l2
  | Coq_splitlong2_case2 (h1, l1, t2) ->
    Elet (t2,
      (f (lift h1) (lift l1) (Eop (Ohighlong, (Econs ((Eletvar O), Enil))))
        (Eop (Olowlong, (Econs ((Eletvar O), Enil))))))
  | Coq_splitlong2_case3 (t1, h2, l2) ->
    Elet (t1,
      (f (Eop (Ohighlong, (Econs ((Eletvar O), Enil)))) (Eop (Olowlong,
        (Econs ((Eletvar O), Enil)))) (lift h2) (lift l2)))
  | Coq_splitlong2_default (e3, e4) ->
    Elet (e3, (Elet ((lift e4),
      (f (Eop (Ohighlong, (Econs ((Eletvar (S O)), Enil)))) (Eop (Olowlong,
        (Econs ((Eletvar (S O)), Enil)))) (Eop (Ohighlong, (Econs ((Eletvar
        O), Enil)))) (Eop (Olowlong, (Econs ((Eletvar O), Enil))))))))

type lowlong_cases =
| Coq_lowlong_case1 of expr * expr
| Coq_lowlong_default of expr

(** val lowlong_match : expr -> lowlong_cases **)

let lowlong_match = function
| Eop (o, e0) ->
  (match o with
   | Omakelong ->
     (match e0 with
      | Enil -> Coq_lowlong_default (Eop (Omakelong, Enil))
      | Econs (e1, e3) ->
        (match e3 with
         | Enil -> Coq_lowlong_default (Eop (Omakelong, (Econs (e1, Enil))))
         | Econs (e2, e4) ->
           (match e4 with
            | Enil -> Coq_lowlong_case1 (e1, e2)
            | Econs (e5, e6) ->
              Coq_lowlong_default (Eop (Omakelong, (Econs (e1, (Econs (e2,
                (Econs (e5, e6)))))))))))
   | x -> Coq_lowlong_default (Eop (x, e0)))
| x -> Coq_lowlong_default x

(** val lowlong : expr -> expr **)

let lowlong e =
  match lowlong_match e with
  | Coq_lowlong_case1 (_, e2) -> e2
  | Coq_lowlong_default e0 -> Eop (Olowlong, (Econs (e0, Enil)))

type highlong_cases =
| Coq_highlong_case1 of expr * expr
| Coq_highlong_default of expr

(** val highlong_match : expr -> highlong_cases **)

let highlong_match = function
| Eop (o, e0) ->
  (match o with
   | Omakelong ->
     (match e0 with
      | Enil -> Coq_highlong_default (Eop (Omakelong, Enil))
      | Econs (e1, e3) ->
        (match e3 with
         | Enil -> Coq_highlong_default (Eop (Omakelong, (Econs (e1, Enil))))
         | Econs (e2, e4) ->
           (match e4 with
            | Enil -> Coq_highlong_case1 (e1, e2)
            | Econs (e5, e6) ->
              Coq_highlong_default (Eop (Omakelong, (Econs (e1, (Econs (e2,
                (Econs (e5, e6)))))))))))
   | x -> Coq_highlong_default (Eop (x, e0)))
| x -> Coq_highlong_default x

(** val highlong : expr -> expr **)

let highlong e =
  match highlong_match e with
  | Coq_highlong_case1 (e1, _) -> e1
  | Coq_highlong_default e0 -> Eop (Ohighlong, (Econs (e0, Enil)))

(** val longconst : Int64.int -> expr **)

let longconst n =
  makelong (Eop ((Ointconst (Int64.hiword n)), Enil)) (Eop ((Ointconst
    (Int64.loword n)), Enil))

type is_longconst_cases =
| Coq_is_longconst_case1 of Int.int * Int.int
| Coq_is_longconst_default of expr

(** val is_longconst_match : expr -> is_longconst_cases **)

let is_longconst_match = function
| Eop (o, e0) ->
  (match o with
   | Omakelong ->
     (match e0 with
      | Enil -> Coq_is_longconst_default (Eop (Omakelong, Enil))
      | Econs (e1, e2) ->
        (match e1 with
         | Eop (o0, e3) ->
           (match o0 with
            | Ointconst h ->
              (match e3 with
               | Enil ->
                 (match e2 with
                  | Enil ->
                    Coq_is_longconst_default (Eop (Omakelong, (Econs ((Eop
                      ((Ointconst h), Enil)), Enil))))
                  | Econs (e4, e5) ->
                    (match e4 with
                     | Eop (o1, e6) ->
                       (match o1 with
                        | Ointconst l ->
                          (match e6 with
                           | Enil ->
                             (match e5 with
                              | Enil -> Coq_is_longconst_case1 (h, l)
                              | Econs (e7, e8) ->
                                Coq_is_longconst_default (Eop (Omakelong,
                                  (Econs ((Eop ((Ointconst h), Enil)), (Econs
                                  ((Eop ((Ointconst l), Enil)), (Econs (e7,
                                  e8)))))))))
                           | Econs (e7, e8) ->
                             Coq_is_longconst_default (Eop (Omakelong, (Econs
                               ((Eop ((Ointconst h), Enil)), (Econs ((Eop
                               ((Ointconst l), (Econs (e7, e8)))), e5)))))))
                        | Omakelong ->
                          Coq_is_longconst_default (Eop (Omakelong, (Econs
                            ((Eop ((Ointconst h), Enil)), (Econs ((Eop
                            (Omakelong, e6)), e5))))))
                        | x ->
                          Coq_is_longconst_default (Eop (Omakelong, (Econs
                            ((Eop ((Ointconst h), Enil)), (Econs ((Eop (x,
                            e6)), e5)))))))
                     | x ->
                       Coq_is_longconst_default (Eop (Omakelong, (Econs ((Eop
                         ((Ointconst h), Enil)), (Econs (x, e5))))))))
               | Econs (e4, e5) ->
                 Coq_is_longconst_default (Eop (Omakelong, (Econs ((Eop
                   ((Ointconst h), (Econs (e4, e5)))), e2)))))
            | Omakelong ->
              Coq_is_longconst_default (Eop (Omakelong, (Econs ((Eop
                (Omakelong, e3)), e2))))
            | x ->
              Coq_is_longconst_default (Eop (Omakelong, (Econs ((Eop (x,
                e3)), e2)))))
         | x -> Coq_is_longconst_default (Eop (Omakelong, (Econs (x, e2))))))
   | x -> Coq_is_longconst_default (Eop (x, e0)))
| x -> Coq_is_longconst_default x

(** val is_longconst : expr -> Int64.int option **)

let is_longconst e =
  match is_longconst_match e with
  | Coq_is_longconst_case1 (h, l) -> Some (Int64.ofwords h l)
  | Coq_is_longconst_default _ -> None

(** val is_longconst_zero : expr -> bool **)

let is_longconst_zero e =
  match is_longconst e with
  | Some n -> Int64.eq n Int64.zero
  | None -> false

(** val intoflong : expr -> expr **)

let intoflong =
  lowlong

type longofint_cases =
| Coq_longofint_case1 of Int.int
| Coq_longofint_default of expr

(** val longofint_match : expr -> longofint_cases **)

let longofint_match = function
| Eop (o, e0) ->
  (match o with
   | Ointconst n ->
     (match e0 with
      | Enil -> Coq_longofint_case1 n
      | Econs (e1, e2) ->
        Coq_longofint_default (Eop ((Ointconst n), (Econs (e1, e2)))))
   | x -> Coq_longofint_default (Eop (x, e0)))
| x -> Coq_longofint_default x

(** val longofint : expr -> expr **)

let longofint e =
  match longofint_match e with
  | Coq_longofint_case1 n -> longconst (Int64.repr (Int.signed n))
  | Coq_longofint_default e0 ->
    Elet (e0,
      (makelong
        (shrimm (Eletvar O)
          (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))
        (Eletvar O)))

(** val longofintu : expr -> expr **)

let longofintu e =
  makelong (Eop ((Ointconst Int.zero), Enil)) e

(** val negl : expr -> expr **)

let negl e =
  match is_longconst e with
  | Some n -> longconst (Int64.neg n)
  | None ->
    Ebuiltin ((EF_builtin
      (('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('n'::('e'::('g'::('l'::[])))))))))))))),
      sig_l_l)), (Econs (e, Enil)))

(** val notl : expr -> expr **)

let notl e =
  splitlong e (fun h l -> makelong (notint h) (notint l))

(** val longoffloat : helper_functions -> expr -> expr **)

let longoffloat hf arg =
  Eexternal (hf.i64_dtos, sig_f_l, (Econs (arg, Enil)))

(** val longuoffloat : helper_functions -> expr -> expr **)

let longuoffloat hf arg =
  Eexternal (hf.i64_dtou, sig_f_l, (Econs (arg, Enil)))

(** val floatoflong : helper_functions -> expr -> expr **)

let floatoflong hf arg =
  Eexternal (hf.i64_stod, sig_l_f, (Econs (arg, Enil)))

(** val floatoflongu : helper_functions -> expr -> expr **)

let floatoflongu hf arg =
  Eexternal (hf.i64_utod, sig_l_f, (Econs (arg, Enil)))

(** val longofsingle : helper_functions -> expr -> expr **)

let longofsingle hf arg =
  longoffloat hf (floatofsingle arg)

(** val longuofsingle : helper_functions -> expr -> expr **)

let longuofsingle hf arg =
  longuoffloat hf (floatofsingle arg)

(** val singleoflong : helper_functions -> expr -> expr **)

let singleoflong hf arg =
  Eexternal (hf.i64_stof, sig_l_s, (Econs (arg, Enil)))

(** val singleoflongu : helper_functions -> expr -> expr **)

let singleoflongu hf arg =
  Eexternal (hf.i64_utof, sig_l_s, (Econs (arg, Enil)))

(** val andl : expr -> expr -> expr **)

let andl e1 e2 =
  splitlong2 e1 e2 (fun h1 l1 h2 l2 ->
    makelong (coq_and h1 h2) (coq_and l1 l2))

(** val orl : expr -> expr -> expr **)

let orl e1 e2 =
  splitlong2 e1 e2 (fun h1 l1 h2 l2 -> makelong (coq_or h1 h2) (coq_or l1 l2))

(** val xorl : expr -> expr -> expr **)

let xorl e1 e2 =
  splitlong2 e1 e2 (fun h1 l1 h2 l2 -> makelong (xor h1 h2) (xor l1 l2))

(** val shllimm : helper_functions -> expr -> Int.int -> expr **)

let shllimm hf e1 n =
  if Int.eq n Int.zero
  then e1
  else if Int.ltu n Int.iwordsize
       then splitlong e1 (fun h l ->
              makelong
                (coq_or (shlimm h n) (shruimm l (Int.sub Int.iwordsize n)))
                (shlimm l n))
       else if Int.ltu n Int64.iwordsize'
            then makelong (shlimm (lowlong e1) (Int.sub n Int.iwordsize))
                   (Eop ((Ointconst Int.zero), Enil))
            else Eexternal (hf.i64_shl, sig_li_l, (Econs (e1, (Econs ((Eop
                   ((Ointconst n), Enil)), Enil)))))

(** val shrluimm : helper_functions -> expr -> Int.int -> expr **)

let shrluimm hf e1 n =
  if Int.eq n Int.zero
  then e1
  else if Int.ltu n Int.iwordsize
       then splitlong e1 (fun h l ->
              makelong (shruimm h n)
                (coq_or (shruimm l n) (shlimm h (Int.sub Int.iwordsize n))))
       else if Int.ltu n Int64.iwordsize'
            then makelong (Eop ((Ointconst Int.zero), Enil))
                   (shruimm (highlong e1) (Int.sub n Int.iwordsize))
            else Eexternal (hf.i64_shr, sig_li_l, (Econs (e1, (Econs ((Eop
                   ((Ointconst n), Enil)), Enil)))))

(** val shrlimm : helper_functions -> expr -> Int.int -> expr **)

let shrlimm hf e1 n =
  if Int.eq n Int.zero
  then e1
  else if Int.ltu n Int.iwordsize
       then splitlong e1 (fun h l ->
              makelong (shrimm h n)
                (coq_or (shruimm l n) (shlimm h (Int.sub Int.iwordsize n))))
       else if Int.ltu n Int64.iwordsize'
            then Elet ((highlong e1),
                   (makelong
                     (shrimm (Eletvar O)
                       (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI
                         Coq_xH)))))))
                     (shrimm (Eletvar O) (Int.sub n Int.iwordsize))))
            else Eexternal (hf.i64_sar, sig_li_l, (Econs (e1, (Econs ((Eop
                   ((Ointconst n), Enil)), Enil)))))

(** val is_intconst : expr -> Int.int option **)

let is_intconst = function
| Eop (o, e0) ->
  (match o with
   | Ointconst n -> (match e0 with
                     | Enil -> Some n
                     | Econs (_, _) -> None)
   | _ -> None)
| _ -> None

(** val shll : helper_functions -> expr -> expr -> expr **)

let shll hf e1 e2 =
  match is_intconst e2 with
  | Some n -> shllimm hf e1 n
  | None -> Eexternal (hf.i64_shl, sig_li_l, (Econs (e1, (Econs (e2, Enil)))))

(** val shrlu : helper_functions -> expr -> expr -> expr **)

let shrlu hf e1 e2 =
  match is_intconst e2 with
  | Some n -> shrluimm hf e1 n
  | None -> Eexternal (hf.i64_shr, sig_li_l, (Econs (e1, (Econs (e2, Enil)))))

(** val shrl : helper_functions -> expr -> expr -> expr **)

let shrl hf e1 e2 =
  match is_intconst e2 with
  | Some n -> shrlimm hf e1 n
  | None -> Eexternal (hf.i64_sar, sig_li_l, (Econs (e1, (Econs (e2, Enil)))))

(** val addl : expr -> expr -> expr **)

let addl e1 e2 =
  let default = Ebuiltin ((EF_builtin
    (('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('a'::('d'::('d'::('l'::[])))))))))))))),
    sig_ll_l)), (Econs (e1, (Econs (e2, Enil)))))
  in
  (match is_longconst e1 with
   | Some n1 ->
     (match is_longconst e2 with
      | Some n2 -> longconst (Int64.add n1 n2)
      | None -> if Int64.eq n1 Int64.zero then e2 else default)
   | None ->
     (match is_longconst e2 with
      | Some n2 -> if Int64.eq n2 Int64.zero then e1 else default
      | None -> default))

(** val subl : expr -> expr -> expr **)

let subl e1 e2 =
  let default = Ebuiltin ((EF_builtin
    (('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('s'::('u'::('b'::('l'::[])))))))))))))),
    sig_ll_l)), (Econs (e1, (Econs (e2, Enil)))))
  in
  (match is_longconst e1 with
   | Some n1 ->
     (match is_longconst e2 with
      | Some n2 -> longconst (Int64.sub n1 n2)
      | None -> if Int64.eq n1 Int64.zero then negl e2 else default)
   | None ->
     (match is_longconst e2 with
      | Some n2 -> if Int64.eq n2 Int64.zero then e1 else default
      | None -> default))

(** val mull_base : expr -> expr -> expr **)

let mull_base e1 e2 =
  splitlong2 e1 e2 (fun h1 l1 h2 l2 -> Elet ((Ebuiltin ((EF_builtin
    (('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('m'::('u'::('l'::('l'::[])))))))))))))),
    sig_ii_l)), (Econs (l1, (Econs (l2, Enil)))))),
    (makelong
      (add
        (add (Eop (Ohighlong, (Econs ((Eletvar O), Enil))))
          (mul (lift l1) (lift h2))) (mul (lift h1) (lift l2))) (Eop
      (Olowlong, (Econs ((Eletvar O), Enil)))))))

(** val mullimm : helper_functions -> Int64.int -> expr -> expr **)

let mullimm hf n e =
  if Int64.eq n Int64.zero
  then longconst Int64.zero
  else if Int64.eq n Int64.one
       then e
       else (match Int64.is_power2' n with
             | Some l -> shllimm hf e l
             | None -> mull_base e (longconst n))

(** val mull : helper_functions -> expr -> expr -> expr **)

let mull hf e1 e2 =
  match is_longconst e1 with
  | Some n1 ->
    (match is_longconst e2 with
     | Some n2 -> longconst (Int64.mul n1 n2)
     | None -> mullimm hf n1 e2)
  | None ->
    (match is_longconst e2 with
     | Some n2 -> mullimm hf n2 e1
     | None -> mull_base e1 e2)

(** val mullhu : helper_functions -> expr -> Int64.int -> expr **)

let mullhu hf e1 n2 =
  Eexternal (hf.i64_umulh, sig_ll_l, (Econs (e1, (Econs ((longconst n2),
    Enil)))))

(** val mullhs : helper_functions -> expr -> Int64.int -> expr **)

let mullhs hf e1 n2 =
  Eexternal (hf.i64_smulh, sig_ll_l, (Econs (e1, (Econs ((longconst n2),
    Enil)))))

(** val shrxlimm : helper_functions -> expr -> Int.int -> expr **)

let shrxlimm hf e n =
  if Int.eq n Int.zero
  then e
  else Elet (e,
         (shrlimm hf
           (addl (Eletvar O)
             (shrluimm hf
               (shrlimm hf (Eletvar O)
                 (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
                   Coq_xH))))))))
               (Int.sub
                 (Int.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                   (Coq_xO Coq_xH)))))))) n))) n))

(** val divlu_base : helper_functions -> expr -> expr -> expr **)

let divlu_base hf e1 e2 =
  Eexternal (hf.i64_udiv, sig_ll_l, (Econs (e1, (Econs (e2, Enil)))))

(** val modlu_base : helper_functions -> expr -> expr -> expr **)

let modlu_base hf e1 e2 =
  Eexternal (hf.i64_umod, sig_ll_l, (Econs (e1, (Econs (e2, Enil)))))

(** val divls_base : helper_functions -> expr -> expr -> expr **)

let divls_base hf e1 e2 =
  Eexternal (hf.i64_sdiv, sig_ll_l, (Econs (e1, (Econs (e2, Enil)))))

(** val modls_base : helper_functions -> expr -> expr -> expr **)

let modls_base hf e1 e2 =
  Eexternal (hf.i64_smod, sig_ll_l, (Econs (e1, (Econs (e2, Enil)))))

(** val cmpl_eq_zero : expr -> expr **)

let cmpl_eq_zero e =
  splitlong e (fun h l ->
    comp Ceq (coq_or h l) (Eop ((Ointconst Int.zero), Enil)))

(** val cmpl_ne_zero : expr -> expr **)

let cmpl_ne_zero e =
  splitlong e (fun h l ->
    comp Cne (coq_or h l) (Eop ((Ointconst Int.zero), Enil)))

(** val cmplu_gen : comparison -> comparison -> expr -> expr -> expr **)

let cmplu_gen ch cl e1 e2 =
  splitlong2 e1 e2 (fun h1 l1 h2 l2 -> Econdition ((CEcond ((Ccomp Ceq),
    (Econs (h1, (Econs (h2, Enil)))))), (Eop ((Ocmp (Ccompu cl)), (Econs (l1,
    (Econs (l2, Enil)))))), (Eop ((Ocmp (Ccompu ch)), (Econs (h1, (Econs (h2,
    Enil))))))))

(** val cmplu : comparison -> expr -> expr -> expr **)

let cmplu c e1 e2 =
  match c with
  | Ceq -> cmpl_eq_zero (xorl e1 e2)
  | Cne -> cmpl_ne_zero (xorl e1 e2)
  | Cle -> cmplu_gen Clt Cle e1 e2
  | Cge -> cmplu_gen Cgt Cge e1 e2
  | x -> cmplu_gen x x e1 e2

(** val cmpl_gen : comparison -> comparison -> expr -> expr -> expr **)

let cmpl_gen ch cl e1 e2 =
  splitlong2 e1 e2 (fun h1 l1 h2 l2 -> Econdition ((CEcond ((Ccomp Ceq),
    (Econs (h1, (Econs (h2, Enil)))))), (Eop ((Ocmp (Ccompu cl)), (Econs (l1,
    (Econs (l2, Enil)))))), (Eop ((Ocmp (Ccomp ch)), (Econs (h1, (Econs (h2,
    Enil))))))))

(** val cmpl : comparison -> expr -> expr -> expr **)

let cmpl c e1 e2 =
  match c with
  | Ceq -> cmpl_eq_zero (xorl e1 e2)
  | Cne -> cmpl_ne_zero (xorl e1 e2)
  | Clt ->
    if is_longconst_zero e2
    then comp Clt (highlong e1) (Eop ((Ointconst Int.zero), Enil))
    else cmpl_gen Clt Clt e1 e2
  | Cle -> cmpl_gen Clt Cle e1 e2
  | Cgt -> cmpl_gen Cgt Cgt e1 e2
  | Cge ->
    if is_longconst_zero e2
    then comp Cge (highlong e1) (Eop ((Ointconst Int.zero), Enil))
    else cmpl_gen Cgt Cge e1 e2
