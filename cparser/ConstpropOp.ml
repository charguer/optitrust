open AST
open Archi
open BinInt
open BinNums
open Compopts
open Datatypes
open Floats
open Integers
open Op
open Registers
open SelectOp
open ValueAOp
open ValueDomain

(** val coq_Olea_ptr : addressing -> operation **)

let coq_Olea_ptr a =
  if ptr64 then Oleal a else Olea a

(** val const_for_result : aval -> operation option **)

let const_for_result = function
| I n -> Some (Ointconst n)
| L n -> if ptr64 then Some (Olongconst n) else None
| F n -> if generate_float_constants () then Some (Ofloatconst n) else None
| FS n -> if generate_float_constants () then Some (Osingleconst n) else None
| Ptr p ->
  (match p with
   | Gl (id, ofs) ->
     if symbol_is_external id
     then if Ptrofs.eq ofs Ptrofs.zero
          then Some (Oindirectsymbol id)
          else None
     else Some (coq_Olea_ptr (Aglobal (id, ofs)))
   | Stk ofs -> Some (coq_Olea_ptr (Ainstack ofs))
   | _ -> None)
| _ -> None

type cond_strength_reduction_cases =
| Coq_cond_strength_reduction_case1 of comparison * reg * reg * Int.int * aval
| Coq_cond_strength_reduction_case2 of comparison * reg * reg * aval * Int.int
| Coq_cond_strength_reduction_case3 of comparison * reg * reg * Int.int * aval
| Coq_cond_strength_reduction_case4 of comparison * reg * reg * aval * Int.int
| Coq_cond_strength_reduction_case5 of comparison * reg * reg * Int64.int
   * aval
| Coq_cond_strength_reduction_case6 of comparison * reg * reg * aval
   * Int64.int
| Coq_cond_strength_reduction_case7 of comparison * reg * reg * Int64.int
   * aval
| Coq_cond_strength_reduction_case8 of comparison * reg * reg * aval
   * Int64.int
| Coq_cond_strength_reduction_default of condition * reg list * aval list

(** val cond_strength_reduction_match :
    condition -> reg list -> aval list -> cond_strength_reduction_cases **)

let cond_strength_reduction_match cond args vl =
  match cond with
  | Ccomp c ->
    let cond0 = Ccomp c in
    (match args with
     | [] -> Coq_cond_strength_reduction_default (cond0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_cond_strength_reduction_default (cond0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_cond_strength_reduction_default (cond0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_cond_strength_reduction_default (cond0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           (Vbot :: (Vbot :: l2)))
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_cond_strength_reduction_case2 (c, r1, r2,
                              Vbot, n2)
                          | a0 :: l3 ->
                            Coq_cond_strength_reduction_default (cond0,
                              args0, (Vbot :: ((I n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           (Vbot :: (x :: l2)))))
                 | I n1 ->
                   (match l1 with
                    | [] ->
                      Coq_cond_strength_reduction_default (cond0, args0, ((I
                        n1) :: []))
                    | v2 :: l2 ->
                      (match l2 with
                       | [] ->
                         Coq_cond_strength_reduction_case1 (c, r1, r2, n1, v2)
                       | a :: l3 ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           ((I n1) :: (v2 :: (a :: l3))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_cond_strength_reduction_default (cond0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_cond_strength_reduction_case2 (c, r1, r2, x,
                              n2)
                          | a0 :: l3 ->
                            Coq_cond_strength_reduction_default (cond0,
                              args0, (x :: ((I n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_cond_strength_reduction_default (cond0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Ccompu c ->
    let cond0 = Ccompu c in
    (match args with
     | [] -> Coq_cond_strength_reduction_default (cond0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_cond_strength_reduction_default (cond0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_cond_strength_reduction_default (cond0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_cond_strength_reduction_default (cond0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           (Vbot :: (Vbot :: l2)))
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_cond_strength_reduction_case4 (c, r1, r2,
                              Vbot, n2)
                          | a0 :: l3 ->
                            Coq_cond_strength_reduction_default (cond0,
                              args0, (Vbot :: ((I n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           (Vbot :: (x :: l2)))))
                 | I n1 ->
                   (match l1 with
                    | [] ->
                      Coq_cond_strength_reduction_default (cond0, args0, ((I
                        n1) :: []))
                    | v2 :: l2 ->
                      (match l2 with
                       | [] ->
                         Coq_cond_strength_reduction_case3 (c, r1, r2, n1, v2)
                       | a :: l3 ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           ((I n1) :: (v2 :: (a :: l3))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_cond_strength_reduction_default (cond0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_cond_strength_reduction_case4 (c, r1, r2, x,
                              n2)
                          | a0 :: l3 ->
                            Coq_cond_strength_reduction_default (cond0,
                              args0, (x :: ((I n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_cond_strength_reduction_default (cond0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Ccompl c ->
    let cond0 = Ccompl c in
    (match args with
     | [] -> Coq_cond_strength_reduction_default (cond0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_cond_strength_reduction_default (cond0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_cond_strength_reduction_default (cond0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_cond_strength_reduction_default (cond0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           (Vbot :: (Vbot :: l2)))
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_cond_strength_reduction_case6 (c, r1, r2,
                              Vbot, n2)
                          | a0 :: l3 ->
                            Coq_cond_strength_reduction_default (cond0,
                              args0, (Vbot :: ((L n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           (Vbot :: (x :: l2)))))
                 | L n1 ->
                   (match l1 with
                    | [] ->
                      Coq_cond_strength_reduction_default (cond0, args0, ((L
                        n1) :: []))
                    | v2 :: l2 ->
                      (match l2 with
                       | [] ->
                         Coq_cond_strength_reduction_case5 (c, r1, r2, n1, v2)
                       | a :: l3 ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           ((L n1) :: (v2 :: (a :: l3))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_cond_strength_reduction_default (cond0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_cond_strength_reduction_case6 (c, r1, r2, x,
                              n2)
                          | a0 :: l3 ->
                            Coq_cond_strength_reduction_default (cond0,
                              args0, (x :: ((L n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_cond_strength_reduction_default (cond0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Ccomplu c ->
    let cond0 = Ccomplu c in
    (match args with
     | [] -> Coq_cond_strength_reduction_default (cond0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_cond_strength_reduction_default (cond0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_cond_strength_reduction_default (cond0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_cond_strength_reduction_default (cond0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           (Vbot :: (Vbot :: l2)))
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_cond_strength_reduction_case8 (c, r1, r2,
                              Vbot, n2)
                          | a0 :: l3 ->
                            Coq_cond_strength_reduction_default (cond0,
                              args0, (Vbot :: ((L n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           (Vbot :: (x :: l2)))))
                 | L n1 ->
                   (match l1 with
                    | [] ->
                      Coq_cond_strength_reduction_default (cond0, args0, ((L
                        n1) :: []))
                    | v2 :: l2 ->
                      (match l2 with
                       | [] ->
                         Coq_cond_strength_reduction_case7 (c, r1, r2, n1, v2)
                       | a :: l3 ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           ((L n1) :: (v2 :: (a :: l3))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_cond_strength_reduction_default (cond0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_cond_strength_reduction_case8 (c, r1, r2, x,
                              n2)
                          | a0 :: l3 ->
                            Coq_cond_strength_reduction_default (cond0,
                              args0, (x :: ((L n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_cond_strength_reduction_default (cond0, args0,
                           (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_cond_strength_reduction_default (cond0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | x -> Coq_cond_strength_reduction_default (x, args, vl)

(** val cond_strength_reduction :
    condition -> reg list -> aval list -> condition * reg list **)

let cond_strength_reduction cond args vl =
  match cond_strength_reduction_match cond args vl with
  | Coq_cond_strength_reduction_case1 (c, _, r2, n1, _) ->
    ((Ccompimm ((swap_comparison c), n1)), (r2 :: []))
  | Coq_cond_strength_reduction_case2 (c, r1, _, _, n2) ->
    ((Ccompimm (c, n2)), (r1 :: []))
  | Coq_cond_strength_reduction_case3 (c, _, r2, n1, _) ->
    ((Ccompuimm ((swap_comparison c), n1)), (r2 :: []))
  | Coq_cond_strength_reduction_case4 (c, r1, _, _, n2) ->
    ((Ccompuimm (c, n2)), (r1 :: []))
  | Coq_cond_strength_reduction_case5 (c, _, r2, n1, _) ->
    ((Ccomplimm ((swap_comparison c), n1)), (r2 :: []))
  | Coq_cond_strength_reduction_case6 (c, r1, _, _, n2) ->
    ((Ccomplimm (c, n2)), (r1 :: []))
  | Coq_cond_strength_reduction_case7 (c, _, r2, n1, _) ->
    ((Ccompluimm ((swap_comparison c), n1)), (r2 :: []))
  | Coq_cond_strength_reduction_case8 (c, r1, _, _, n2) ->
    ((Ccompluimm (c, n2)), (r1 :: []))
  | Coq_cond_strength_reduction_default (cond0, args0, _) -> (cond0, args0)

(** val make_cmp_base :
    condition -> reg list -> aval list -> operation * reg list **)

let make_cmp_base c args vl =
  let (c', args') = cond_strength_reduction c args vl in ((Ocmp c'), args')

(** val make_cmp_imm_eq :
    condition -> reg list -> aval list -> Int.int -> reg -> aval ->
    operation * reg list **)

let make_cmp_imm_eq c args vl n r1 v1 =
  if (&&) ((fun x -> x) (Int.eq_dec n Int.one))
       (vincl v1 (Uns (Ptop, (Zpos Coq_xH))))
  then (Omove, (r1 :: []))
  else if (&&) ((fun x -> x) (Int.eq_dec n Int.zero))
            (vincl v1 (Uns (Ptop, (Zpos Coq_xH))))
       then ((Oxorimm Int.one), (r1 :: []))
       else make_cmp_base c args vl

(** val make_cmp_imm_ne :
    condition -> reg list -> aval list -> Int.int -> reg -> aval ->
    operation * reg list **)

let make_cmp_imm_ne c args vl n r1 v1 =
  if (&&) ((fun x -> x) (Int.eq_dec n Int.zero))
       (vincl v1 (Uns (Ptop, (Zpos Coq_xH))))
  then (Omove, (r1 :: []))
  else if (&&) ((fun x -> x) (Int.eq_dec n Int.one))
            (vincl v1 (Uns (Ptop, (Zpos Coq_xH))))
       then ((Oxorimm Int.one), (r1 :: []))
       else make_cmp_base c args vl

type make_cmp_cases =
| Coq_make_cmp_case1 of Int.int * reg * aval
| Coq_make_cmp_case2 of Int.int * reg * aval
| Coq_make_cmp_case3 of Int.int * reg * aval
| Coq_make_cmp_case4 of Int.int * reg * aval
| Coq_make_cmp_default of condition * reg list * aval list

(** val make_cmp_match :
    condition -> reg list -> aval list -> make_cmp_cases **)

let make_cmp_match c args vl =
  match c with
  | Ccompimm (c0, n) ->
    (match c0 with
     | Ceq ->
       let c1 = Ccompimm (Ceq, n) in
       (match args with
        | [] -> Coq_make_cmp_default (c1, [], vl)
        | r1 :: l ->
          (match l with
           | [] ->
             let args0 = r1 :: [] in
             (match vl with
              | [] -> Coq_make_cmp_default (c1, args0, [])
              | v1 :: l0 ->
                (match l0 with
                 | [] -> Coq_make_cmp_case1 (n, r1, v1)
                 | a :: l1 ->
                   Coq_make_cmp_default (c1, args0, (v1 :: (a :: l1)))))
           | r :: l0 -> Coq_make_cmp_default (c1, (r1 :: (r :: l0)), vl)))
     | Cne ->
       let c1 = Ccompimm (Cne, n) in
       (match args with
        | [] -> Coq_make_cmp_default (c1, [], vl)
        | r1 :: l ->
          (match l with
           | [] ->
             let args0 = r1 :: [] in
             (match vl with
              | [] -> Coq_make_cmp_default (c1, args0, [])
              | v1 :: l0 ->
                (match l0 with
                 | [] -> Coq_make_cmp_case2 (n, r1, v1)
                 | a :: l1 ->
                   Coq_make_cmp_default (c1, args0, (v1 :: (a :: l1)))))
           | r :: l0 -> Coq_make_cmp_default (c1, (r1 :: (r :: l0)), vl)))
     | x -> Coq_make_cmp_default ((Ccompimm (x, n)), args, vl))
  | Ccompuimm (c0, n) ->
    (match c0 with
     | Ceq ->
       let c1 = Ccompuimm (Ceq, n) in
       (match args with
        | [] -> Coq_make_cmp_default (c1, [], vl)
        | r1 :: l ->
          (match l with
           | [] ->
             let args0 = r1 :: [] in
             (match vl with
              | [] -> Coq_make_cmp_default (c1, args0, [])
              | v1 :: l0 ->
                (match l0 with
                 | [] -> Coq_make_cmp_case3 (n, r1, v1)
                 | a :: l1 ->
                   Coq_make_cmp_default (c1, args0, (v1 :: (a :: l1)))))
           | r :: l0 -> Coq_make_cmp_default (c1, (r1 :: (r :: l0)), vl)))
     | Cne ->
       let c1 = Ccompuimm (Cne, n) in
       (match args with
        | [] -> Coq_make_cmp_default (c1, [], vl)
        | r1 :: l ->
          (match l with
           | [] ->
             let args0 = r1 :: [] in
             (match vl with
              | [] -> Coq_make_cmp_default (c1, args0, [])
              | v1 :: l0 ->
                (match l0 with
                 | [] -> Coq_make_cmp_case4 (n, r1, v1)
                 | a :: l1 ->
                   Coq_make_cmp_default (c1, args0, (v1 :: (a :: l1)))))
           | r :: l0 -> Coq_make_cmp_default (c1, (r1 :: (r :: l0)), vl)))
     | x -> Coq_make_cmp_default ((Ccompuimm (x, n)), args, vl))
  | x -> Coq_make_cmp_default (x, args, vl)

(** val make_cmp :
    condition -> reg list -> aval list -> operation * reg list **)

let make_cmp c args vl =
  match make_cmp_match c args vl with
  | Coq_make_cmp_case1 (n, r1, v1) -> make_cmp_imm_eq c args vl n r1 v1
  | Coq_make_cmp_case2 (n, r1, v1) -> make_cmp_imm_ne c args vl n r1 v1
  | Coq_make_cmp_case3 (n, r1, v1) -> make_cmp_imm_eq c args vl n r1 v1
  | Coq_make_cmp_case4 (n, r1, v1) -> make_cmp_imm_ne c args vl n r1 v1
  | Coq_make_cmp_default (c0, args0, vl0) -> make_cmp_base c0 args0 vl0

(** val make_select :
    condition -> typ -> reg -> reg -> reg list -> aval list ->
    operation * reg list **)

let make_select c ty r1 r2 args vl =
  match resolve_branch (eval_static_condition c vl) with
  | Some b -> (Omove, ((if b then r1 else r2) :: []))
  | None ->
    let (c', args') = cond_strength_reduction c args vl in
    ((Osel (c', ty)), (r1 :: (r2 :: args')))

type addr_strength_reduction_32_generic_cases =
| Coq_addr_strength_reduction_32_generic_case1 of coq_Z * reg * reg * 
   Int.int * aval
| Coq_addr_strength_reduction_32_generic_case2 of coq_Z * reg * reg * 
   aval * Int.int
| Coq_addr_strength_reduction_32_generic_case3 of coq_Z * coq_Z * reg * 
   reg * aval * Int.int
| Coq_addr_strength_reduction_32_generic_case4 of coq_Z * coq_Z * reg * 
   reg * Int.int * aval
| Coq_addr_strength_reduction_32_generic_default of addressing * reg list
   * aval list

(** val addr_strength_reduction_32_generic_match :
    addressing -> reg list -> aval list ->
    addr_strength_reduction_32_generic_cases **)

let addr_strength_reduction_32_generic_match addr args vl =
  match addr with
  | Aindexed2 ofs ->
    let addr0 = Aindexed2 ofs in
    (match args with
     | [] -> Coq_addr_strength_reduction_32_generic_default (addr0, [], vl)
     | r1 :: l ->
       (match l with
        | [] ->
          Coq_addr_strength_reduction_32_generic_default (addr0, (r1 :: []),
            vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] ->
                Coq_addr_strength_reduction_32_generic_default (addr0, args0,
                  [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_32_generic_default (addr0,
                        args0, (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_addr_strength_reduction_32_generic_default
                           (addr0, args0, (Vbot :: (Vbot :: l2)))
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_addr_strength_reduction_32_generic_case2
                              (ofs, r1, r2, Vbot, n2)
                          | a0 :: l3 ->
                            Coq_addr_strength_reduction_32_generic_default
                              (addr0, args0, (Vbot :: ((I n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_addr_strength_reduction_32_generic_default
                           (addr0, args0, (Vbot :: (x :: l2)))))
                 | I n1 ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_32_generic_default (addr0,
                        args0, ((I n1) :: []))
                    | v2 :: l2 ->
                      (match l2 with
                       | [] ->
                         Coq_addr_strength_reduction_32_generic_case1 (ofs,
                           r1, r2, n1, v2)
                       | a :: l3 ->
                         Coq_addr_strength_reduction_32_generic_default
                           (addr0, args0, ((I n1) :: (v2 :: (a :: l3))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_32_generic_default (addr0,
                        args0, (x :: []))
                    | a :: l2 ->
                      (match a with
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_addr_strength_reduction_32_generic_case2
                              (ofs, r1, r2, x, n2)
                          | a0 :: l3 ->
                            Coq_addr_strength_reduction_32_generic_default
                              (addr0, args0, (x :: ((I n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_addr_strength_reduction_32_generic_default
                           (addr0, args0, (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_addr_strength_reduction_32_generic_default (addr0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Aindexed2scaled (sc, ofs) ->
    let addr0 = Aindexed2scaled (sc, ofs) in
    (match args with
     | [] -> Coq_addr_strength_reduction_32_generic_default (addr0, [], vl)
     | r1 :: l ->
       (match l with
        | [] ->
          Coq_addr_strength_reduction_32_generic_default (addr0, (r1 :: []),
            vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] ->
                Coq_addr_strength_reduction_32_generic_default (addr0, args0,
                  [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_32_generic_default (addr0,
                        args0, (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_addr_strength_reduction_32_generic_default
                           (addr0, args0, (Vbot :: (Vbot :: l2)))
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_addr_strength_reduction_32_generic_case3 (sc,
                              ofs, r1, r2, Vbot, n2)
                          | a0 :: l3 ->
                            Coq_addr_strength_reduction_32_generic_default
                              (addr0, args0, (Vbot :: ((I n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_addr_strength_reduction_32_generic_default
                           (addr0, args0, (Vbot :: (x :: l2)))))
                 | I n1 ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_32_generic_default (addr0,
                        args0, ((I n1) :: []))
                    | v2 :: l2 ->
                      (match v2 with
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_addr_strength_reduction_32_generic_case3 (sc,
                              ofs, r1, r2, (I n1), n2)
                          | a :: l3 ->
                            Coq_addr_strength_reduction_32_generic_default
                              (addr0, args0, ((I n1) :: ((I
                              n2) :: (a :: l3)))))
                       | x ->
                         (match l2 with
                          | [] ->
                            Coq_addr_strength_reduction_32_generic_case4 (sc,
                              ofs, r1, r2, n1, x)
                          | a :: l3 ->
                            Coq_addr_strength_reduction_32_generic_default
                              (addr0, args0, ((I n1) :: (x :: (a :: l3)))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_32_generic_default (addr0,
                        args0, (x :: []))
                    | a :: l2 ->
                      (match a with
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_addr_strength_reduction_32_generic_case3 (sc,
                              ofs, r1, r2, x, n2)
                          | a0 :: l3 ->
                            Coq_addr_strength_reduction_32_generic_default
                              (addr0, args0, (x :: ((I n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_addr_strength_reduction_32_generic_default
                           (addr0, args0, (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_addr_strength_reduction_32_generic_default (addr0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | x -> Coq_addr_strength_reduction_32_generic_default (x, args, vl)

(** val addr_strength_reduction_32_generic :
    addressing -> reg list -> aval list -> addressing * reg list **)

let addr_strength_reduction_32_generic addr args vl =
  match addr_strength_reduction_32_generic_match addr args vl with
  | Coq_addr_strength_reduction_32_generic_case1 (ofs, _, r2, n1, _) ->
    ((Aindexed (Z.add (Int.signed n1) ofs)), (r2 :: []))
  | Coq_addr_strength_reduction_32_generic_case2 (ofs, r1, _, _, n2) ->
    ((Aindexed (Z.add (Int.signed n2) ofs)), (r1 :: []))
  | Coq_addr_strength_reduction_32_generic_case3 (sc, ofs, r1, _, _, n2) ->
    ((Aindexed (Z.add (Z.mul (Int.signed n2) sc) ofs)), (r1 :: []))
  | Coq_addr_strength_reduction_32_generic_case4 (sc, ofs, _, r2, n1, _) ->
    ((Ascaled (sc, (Z.add (Int.signed n1) ofs))), (r2 :: []))
  | Coq_addr_strength_reduction_32_generic_default (addr0, args0, _) ->
    (addr0, args0)

type addr_strength_reduction_32_cases =
| Coq_addr_strength_reduction_32_case1 of coq_Z * reg * ident * Ptrofs.int
| Coq_addr_strength_reduction_32_case2 of coq_Z * reg * Ptrofs.int
| Coq_addr_strength_reduction_32_case3 of coq_Z * reg * reg * ident
   * Ptrofs.int * Int.int
| Coq_addr_strength_reduction_32_case4 of coq_Z * reg * reg * Int.int * 
   ident * Ptrofs.int
| Coq_addr_strength_reduction_32_case5 of coq_Z * reg * reg * Ptrofs.int
   * Int.int
| Coq_addr_strength_reduction_32_case6 of coq_Z * reg * reg * Int.int
   * Ptrofs.int
| Coq_addr_strength_reduction_32_case7 of coq_Z * reg * reg * ident
   * Ptrofs.int * aval
| Coq_addr_strength_reduction_32_case8 of coq_Z * reg * reg * aval * 
   ident * Ptrofs.int
| Coq_addr_strength_reduction_32_case9 of coq_Z * coq_Z * reg * reg * 
   ident * Ptrofs.int * Int.int
| Coq_addr_strength_reduction_32_case10 of coq_Z * coq_Z * reg * reg * 
   ident * Ptrofs.int * aval
| Coq_addr_strength_reduction_32_case11 of ident * Ptrofs.int * reg * Int.int
| Coq_addr_strength_reduction_32_case12 of coq_Z * ident * Ptrofs.int * 
   reg * Int.int
| Coq_addr_strength_reduction_32_default of addressing * reg list * aval list

(** val addr_strength_reduction_32_match :
    addressing -> reg list -> aval list -> addr_strength_reduction_32_cases **)

let addr_strength_reduction_32_match addr args vl =
  match addr with
  | Aindexed ofs ->
    let addr0 = Aindexed ofs in
    (match args with
     | [] -> Coq_addr_strength_reduction_32_default (addr0, [], vl)
     | r1 :: l ->
       (match l with
        | [] ->
          let args0 = r1 :: [] in
          (match vl with
           | [] -> Coq_addr_strength_reduction_32_default (addr0, args0, [])
           | a :: l0 ->
             (match a with
              | Ptr p ->
                (match p with
                 | Gl (symb, n) ->
                   (match l0 with
                    | [] ->
                      Coq_addr_strength_reduction_32_case1 (ofs, r1, symb, n)
                    | a0 :: l1 ->
                      Coq_addr_strength_reduction_32_default (addr0, args0,
                        ((Ptr (Gl (symb, n))) :: (a0 :: l1))))
                 | Stk n ->
                   (match l0 with
                    | [] -> Coq_addr_strength_reduction_32_case2 (ofs, r1, n)
                    | a0 :: l1 ->
                      Coq_addr_strength_reduction_32_default (addr0, args0,
                        ((Ptr (Stk n)) :: (a0 :: l1))))
                 | x ->
                   Coq_addr_strength_reduction_32_default (addr0, args0,
                     ((Ptr x) :: l0)))
              | x ->
                Coq_addr_strength_reduction_32_default (addr0, args0,
                  (x :: l0))))
        | r :: l0 ->
          Coq_addr_strength_reduction_32_default (addr0, (r1 :: (r :: l0)),
            vl)))
  | Aindexed2 ofs ->
    let addr0 = Aindexed2 ofs in
    (match args with
     | [] -> Coq_addr_strength_reduction_32_default (addr0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_addr_strength_reduction_32_default (addr0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] ->
                Coq_addr_strength_reduction_32_default (addr0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_32_default (addr0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, (Vbot :: (Vbot :: l2)))
                       | Ptr p ->
                         (match p with
                          | Gl (symb, n2) ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_32_case8 (ofs, r1,
                                 r2, Vbot, symb, n2)
                             | a0 :: l3 ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, (Vbot :: ((Ptr (Gl (symb,
                                 n2))) :: (a0 :: l3)))))
                          | x ->
                            Coq_addr_strength_reduction_32_default (addr0,
                              args0, (Vbot :: ((Ptr x) :: l2))))
                       | x ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, (Vbot :: (x :: l2)))))
                 | I n1 ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_32_default (addr0, args0,
                        ((I n1) :: []))
                    | a :: l2 ->
                      (match a with
                       | Ptr p ->
                         (match p with
                          | Gl (symb, n2) ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_32_case4 (ofs, r1,
                                 r2, n1, symb, n2)
                             | a0 :: l3 ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((I n1) :: ((Ptr (Gl (symb,
                                 n2))) :: (a0 :: l3)))))
                          | Stk n2 ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_32_case6 (ofs, r1,
                                 r2, n1, n2)
                             | a0 :: l3 ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((I n1) :: ((Ptr (Stk
                                 n2)) :: (a0 :: l3)))))
                          | x ->
                            Coq_addr_strength_reduction_32_default (addr0,
                              args0, ((I n1) :: ((Ptr x) :: l2))))
                       | x ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, ((I n1) :: (x :: l2)))))
                 | Uns (p, n) ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_32_default (addr0, args0,
                        ((Uns (p, n)) :: []))
                    | a :: l2 ->
                      (match a with
                       | Ptr p0 ->
                         (match p0 with
                          | Gl (symb, n2) ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_32_case8 (ofs, r1,
                                 r2, (Uns (p, n)), symb, n2)
                             | a0 :: l3 ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Uns (p, n)) :: ((Ptr (Gl (symb,
                                 n2))) :: (a0 :: l3)))))
                          | x ->
                            Coq_addr_strength_reduction_32_default (addr0,
                              args0, ((Uns (p, n)) :: ((Ptr x) :: l2))))
                       | x ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, ((Uns (p, n)) :: (x :: l2)))))
                 | Sgn (p, n) ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_32_default (addr0, args0,
                        ((Sgn (p, n)) :: []))
                    | a :: l2 ->
                      (match a with
                       | Ptr p0 ->
                         (match p0 with
                          | Gl (symb, n2) ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_32_case8 (ofs, r1,
                                 r2, (Sgn (p, n)), symb, n2)
                             | a0 :: l3 ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Sgn (p, n)) :: ((Ptr (Gl (symb,
                                 n2))) :: (a0 :: l3)))))
                          | x ->
                            Coq_addr_strength_reduction_32_default (addr0,
                              args0, ((Sgn (p, n)) :: ((Ptr x) :: l2))))
                       | x ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, ((Sgn (p, n)) :: (x :: l2)))))
                 | Ptr p ->
                   (match p with
                    | Pbot ->
                      (match l1 with
                       | [] ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, ((Ptr Pbot) :: []))
                       | a :: l2 ->
                         (match a with
                          | Ptr p0 ->
                            (match p0 with
                             | Pbot ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr Pbot) :: ((Ptr Pbot) :: l2)))
                             | Gl (symb, n2) ->
                               (match l2 with
                                | [] ->
                                  Coq_addr_strength_reduction_32_case8 (ofs,
                                    r1, r2, (Ptr Pbot), symb, n2)
                                | a0 :: l3 ->
                                  Coq_addr_strength_reduction_32_default
                                    (addr0, args0, ((Ptr Pbot) :: ((Ptr (Gl
                                    (symb, n2))) :: (a0 :: l3)))))
                             | x ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr Pbot) :: ((Ptr x) :: l2))))
                          | x ->
                            Coq_addr_strength_reduction_32_default (addr0,
                              args0, ((Ptr Pbot) :: (x :: l2)))))
                    | Gl (symb, n1) ->
                      (match l1 with
                       | [] ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, ((Ptr (Gl (symb, n1))) :: []))
                       | v2 :: l2 ->
                         (match v2 with
                          | I n2 ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_32_case3 (ofs, r1,
                                 r2, symb, n1, n2)
                             | a :: l3 ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr (Gl (symb, n1))) :: ((I
                                 n2) :: (a :: l3)))))
                          | x ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_32_case7 (ofs, r1,
                                 r2, symb, n1, x)
                             | a :: l3 ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr (Gl (symb,
                                 n1))) :: (x :: (a :: l3)))))))
                    | Glo id ->
                      (match l1 with
                       | [] ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, ((Ptr (Glo id)) :: []))
                       | a :: l2 ->
                         (match a with
                          | Ptr p0 ->
                            (match p0 with
                             | Gl (symb, n2) ->
                               (match l2 with
                                | [] ->
                                  Coq_addr_strength_reduction_32_case8 (ofs,
                                    r1, r2, (Ptr (Glo id)), symb, n2)
                                | a0 :: l3 ->
                                  Coq_addr_strength_reduction_32_default
                                    (addr0, args0, ((Ptr (Glo id)) :: ((Ptr
                                    (Gl (symb, n2))) :: (a0 :: l3)))))
                             | x ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr (Glo id)) :: ((Ptr x) :: l2))))
                          | x ->
                            Coq_addr_strength_reduction_32_default (addr0,
                              args0, ((Ptr (Glo id)) :: (x :: l2)))))
                    | Glob ->
                      (match l1 with
                       | [] ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, ((Ptr Glob) :: []))
                       | a :: l2 ->
                         (match a with
                          | Ptr p0 ->
                            (match p0 with
                             | Gl (symb, n2) ->
                               (match l2 with
                                | [] ->
                                  Coq_addr_strength_reduction_32_case8 (ofs,
                                    r1, r2, (Ptr Glob), symb, n2)
                                | a0 :: l3 ->
                                  Coq_addr_strength_reduction_32_default
                                    (addr0, args0, ((Ptr Glob) :: ((Ptr (Gl
                                    (symb, n2))) :: (a0 :: l3)))))
                             | Glob ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr Glob) :: ((Ptr Glob) :: l2)))
                             | x ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr Glob) :: ((Ptr x) :: l2))))
                          | x ->
                            Coq_addr_strength_reduction_32_default (addr0,
                              args0, ((Ptr Glob) :: (x :: l2)))))
                    | Stk n1 ->
                      (match l1 with
                       | [] ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, ((Ptr (Stk n1)) :: []))
                       | a :: l2 ->
                         (match a with
                          | I n2 ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_32_case5 (ofs, r1,
                                 r2, n1, n2)
                             | a0 :: l3 ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr (Stk n1)) :: ((I
                                 n2) :: (a0 :: l3)))))
                          | Ptr p0 ->
                            (match p0 with
                             | Gl (symb, n2) ->
                               (match l2 with
                                | [] ->
                                  Coq_addr_strength_reduction_32_case8 (ofs,
                                    r1, r2, (Ptr (Stk n1)), symb, n2)
                                | a0 :: l3 ->
                                  Coq_addr_strength_reduction_32_default
                                    (addr0, args0, ((Ptr (Stk n1)) :: ((Ptr
                                    (Gl (symb, n2))) :: (a0 :: l3)))))
                             | x ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr (Stk n1)) :: ((Ptr x) :: l2))))
                          | x ->
                            Coq_addr_strength_reduction_32_default (addr0,
                              args0, ((Ptr (Stk n1)) :: (x :: l2)))))
                    | Stack ->
                      (match l1 with
                       | [] ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, ((Ptr Stack) :: []))
                       | a :: l2 ->
                         (match a with
                          | Ptr p0 ->
                            (match p0 with
                             | Gl (symb, n2) ->
                               (match l2 with
                                | [] ->
                                  Coq_addr_strength_reduction_32_case8 (ofs,
                                    r1, r2, (Ptr Stack), symb, n2)
                                | a0 :: l3 ->
                                  Coq_addr_strength_reduction_32_default
                                    (addr0, args0, ((Ptr Stack) :: ((Ptr (Gl
                                    (symb, n2))) :: (a0 :: l3)))))
                             | Stack ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr Stack) :: ((Ptr Stack) :: l2)))
                             | x ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr Stack) :: ((Ptr x) :: l2))))
                          | x ->
                            Coq_addr_strength_reduction_32_default (addr0,
                              args0, ((Ptr Stack) :: (x :: l2)))))
                    | Nonstack ->
                      (match l1 with
                       | [] ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, ((Ptr Nonstack) :: []))
                       | a :: l2 ->
                         (match a with
                          | Ptr p0 ->
                            (match p0 with
                             | Gl (symb, n2) ->
                               (match l2 with
                                | [] ->
                                  Coq_addr_strength_reduction_32_case8 (ofs,
                                    r1, r2, (Ptr Nonstack), symb, n2)
                                | a0 :: l3 ->
                                  Coq_addr_strength_reduction_32_default
                                    (addr0, args0, ((Ptr Nonstack) :: ((Ptr
                                    (Gl (symb, n2))) :: (a0 :: l3)))))
                             | Nonstack ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr Nonstack) :: ((Ptr
                                 Nonstack) :: l2)))
                             | x ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr Nonstack) :: ((Ptr x) :: l2))))
                          | x ->
                            Coq_addr_strength_reduction_32_default (addr0,
                              args0, ((Ptr Nonstack) :: (x :: l2)))))
                    | Ptop ->
                      (match l1 with
                       | [] ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, ((Ptr Ptop) :: []))
                       | a :: l2 ->
                         (match a with
                          | Ptr p0 ->
                            (match p0 with
                             | Gl (symb, n2) ->
                               (match l2 with
                                | [] ->
                                  Coq_addr_strength_reduction_32_case8 (ofs,
                                    r1, r2, (Ptr Ptop), symb, n2)
                                | a0 :: l3 ->
                                  Coq_addr_strength_reduction_32_default
                                    (addr0, args0, ((Ptr Ptop) :: ((Ptr (Gl
                                    (symb, n2))) :: (a0 :: l3)))))
                             | Ptop ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr Ptop) :: ((Ptr Ptop) :: l2)))
                             | x ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr Ptop) :: ((Ptr x) :: l2))))
                          | x ->
                            Coq_addr_strength_reduction_32_default (addr0,
                              args0, ((Ptr Ptop) :: (x :: l2))))))
                 | Ifptr p ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_32_default (addr0, args0,
                        ((Ifptr p) :: []))
                    | a :: l2 ->
                      (match a with
                       | Ptr p0 ->
                         (match p0 with
                          | Gl (symb, n2) ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_32_case8 (ofs, r1,
                                 r2, (Ifptr p), symb, n2)
                             | a0 :: l3 ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ifptr p) :: ((Ptr (Gl (symb,
                                 n2))) :: (a0 :: l3)))))
                          | x ->
                            Coq_addr_strength_reduction_32_default (addr0,
                              args0, ((Ifptr p) :: ((Ptr x) :: l2))))
                       | x ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, ((Ifptr p) :: (x :: l2)))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_32_default (addr0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | Ptr p ->
                         (match p with
                          | Gl (symb, n2) ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_32_case8 (ofs, r1,
                                 r2, x, symb, n2)
                             | a0 :: l3 ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, (x :: ((Ptr (Gl (symb,
                                 n2))) :: (a0 :: l3)))))
                          | x0 ->
                            Coq_addr_strength_reduction_32_default (addr0,
                              args0, (x :: ((Ptr x0) :: l2))))
                       | x0 ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_addr_strength_reduction_32_default (addr0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Aindexed2scaled (sc, ofs) ->
    let addr0 = Aindexed2scaled (sc, ofs) in
    (match args with
     | [] -> Coq_addr_strength_reduction_32_default (addr0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_addr_strength_reduction_32_default (addr0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] ->
                Coq_addr_strength_reduction_32_default (addr0, args0, [])
              | a :: l1 ->
                (match a with
                 | Ptr p ->
                   (match p with
                    | Gl (symb, n1) ->
                      (match l1 with
                       | [] ->
                         Coq_addr_strength_reduction_32_default (addr0,
                           args0, ((Ptr (Gl (symb, n1))) :: []))
                       | v2 :: l2 ->
                         (match v2 with
                          | I n2 ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_32_case9 (sc, ofs,
                                 r1, r2, symb, n1, n2)
                             | a0 :: l3 ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr (Gl (symb, n1))) :: ((I
                                 n2) :: (a0 :: l3)))))
                          | x ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_32_case10 (sc,
                                 ofs, r1, r2, symb, n1, x)
                             | a0 :: l3 ->
                               Coq_addr_strength_reduction_32_default (addr0,
                                 args0, ((Ptr (Gl (symb,
                                 n1))) :: (x :: (a0 :: l3)))))))
                    | x ->
                      Coq_addr_strength_reduction_32_default (addr0, args0,
                        ((Ptr x) :: l1)))
                 | x ->
                   Coq_addr_strength_reduction_32_default (addr0, args0,
                     (x :: l1))))
           | r :: l1 ->
             Coq_addr_strength_reduction_32_default (addr0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Abased (id, ofs) ->
    let addr0 = Abased (id, ofs) in
    (match args with
     | [] -> Coq_addr_strength_reduction_32_default (addr0, [], vl)
     | r1 :: l ->
       (match l with
        | [] ->
          let args0 = r1 :: [] in
          (match vl with
           | [] -> Coq_addr_strength_reduction_32_default (addr0, args0, [])
           | a :: l0 ->
             (match a with
              | I n1 ->
                (match l0 with
                 | [] ->
                   Coq_addr_strength_reduction_32_case11 (id, ofs, r1, n1)
                 | a0 :: l1 ->
                   Coq_addr_strength_reduction_32_default (addr0, args0, ((I
                     n1) :: (a0 :: l1))))
              | x ->
                Coq_addr_strength_reduction_32_default (addr0, args0,
                  (x :: l0))))
        | r :: l0 ->
          Coq_addr_strength_reduction_32_default (addr0, (r1 :: (r :: l0)),
            vl)))
  | Abasedscaled (sc, id, ofs) ->
    let addr0 = Abasedscaled (sc, id, ofs) in
    (match args with
     | [] -> Coq_addr_strength_reduction_32_default (addr0, [], vl)
     | r1 :: l ->
       (match l with
        | [] ->
          let args0 = r1 :: [] in
          (match vl with
           | [] -> Coq_addr_strength_reduction_32_default (addr0, args0, [])
           | a :: l0 ->
             (match a with
              | I n1 ->
                (match l0 with
                 | [] ->
                   Coq_addr_strength_reduction_32_case12 (sc, id, ofs, r1, n1)
                 | a0 :: l1 ->
                   Coq_addr_strength_reduction_32_default (addr0, args0, ((I
                     n1) :: (a0 :: l1))))
              | x ->
                Coq_addr_strength_reduction_32_default (addr0, args0,
                  (x :: l0))))
        | r :: l0 ->
          Coq_addr_strength_reduction_32_default (addr0, (r1 :: (r :: l0)),
            vl)))
  | x -> Coq_addr_strength_reduction_32_default (x, args, vl)

(** val addr_strength_reduction_32 :
    addressing -> reg list -> aval list -> addressing * reg list **)

let addr_strength_reduction_32 addr args vl =
  if ptr64
  then addr_strength_reduction_32_generic addr args vl
  else (match addr_strength_reduction_32_match addr args vl with
        | Coq_addr_strength_reduction_32_case1 (ofs, _, symb, n) ->
          ((Aglobal (symb, (Ptrofs.add n (Ptrofs.repr ofs)))), [])
        | Coq_addr_strength_reduction_32_case2 (ofs, _, n) ->
          ((Ainstack (Ptrofs.add n (Ptrofs.repr ofs))), [])
        | Coq_addr_strength_reduction_32_case3 (ofs, _, _, symb, n1, n2) ->
          ((Aglobal (symb,
            (Ptrofs.add (Ptrofs.add n1 (Ptrofs.of_int n2)) (Ptrofs.repr ofs)))),
            [])
        | Coq_addr_strength_reduction_32_case4 (ofs, _, _, n1, symb, n2) ->
          ((Aglobal (symb,
            (Ptrofs.add (Ptrofs.add n2 (Ptrofs.of_int n1)) (Ptrofs.repr ofs)))),
            [])
        | Coq_addr_strength_reduction_32_case5 (ofs, _, _, n1, n2) ->
          ((Ainstack
            (Ptrofs.add (Ptrofs.add n1 (Ptrofs.of_int n2)) (Ptrofs.repr ofs))),
            [])
        | Coq_addr_strength_reduction_32_case6 (ofs, _, _, n1, n2) ->
          ((Ainstack
            (Ptrofs.add (Ptrofs.add (Ptrofs.of_int n1) n2) (Ptrofs.repr ofs))),
            [])
        | Coq_addr_strength_reduction_32_case7 (ofs, _, r2, symb, n1, _) ->
          ((Abased (symb, (Ptrofs.add n1 (Ptrofs.repr ofs)))), (r2 :: []))
        | Coq_addr_strength_reduction_32_case8 (ofs, r1, _, _, symb, n2) ->
          ((Abased (symb, (Ptrofs.add n2 (Ptrofs.repr ofs)))), (r1 :: []))
        | Coq_addr_strength_reduction_32_case9 (sc, ofs, _, _, symb, n1, n2) ->
          ((Aglobal (symb,
            (Ptrofs.add
              (Ptrofs.add n1 (Ptrofs.of_int (Int.mul n2 (Int.repr sc))))
              (Ptrofs.repr ofs)))), [])
        | Coq_addr_strength_reduction_32_case10 (sc, ofs, _, r2, symb, n1, _) ->
          ((Abasedscaled (sc, symb, (Ptrofs.add n1 (Ptrofs.repr ofs)))),
            (r2 :: []))
        | Coq_addr_strength_reduction_32_case11 (id, ofs, _, n1) ->
          ((Aglobal (id, (Ptrofs.add ofs (Ptrofs.of_int n1)))), [])
        | Coq_addr_strength_reduction_32_case12 (sc, id, ofs, _, n1) ->
          ((Aglobal (id,
            (Ptrofs.add ofs (Ptrofs.of_int (Int.mul n1 (Int.repr sc)))))), [])
        | Coq_addr_strength_reduction_32_default (addr0, args0, vl0) ->
          addr_strength_reduction_32_generic addr0 args0 vl0)

type addr_strength_reduction_64_generic_cases =
| Coq_addr_strength_reduction_64_generic_case1 of coq_Z * reg * reg
   * Int64.int * aval
| Coq_addr_strength_reduction_64_generic_case2 of coq_Z * reg * reg * 
   aval * Int64.int
| Coq_addr_strength_reduction_64_generic_case3 of coq_Z * coq_Z * reg * 
   reg * aval * Int64.int
| Coq_addr_strength_reduction_64_generic_case4 of coq_Z * coq_Z * reg * 
   reg * Int64.int * aval
| Coq_addr_strength_reduction_64_generic_default of addressing * reg list
   * aval list

(** val addr_strength_reduction_64_generic_match :
    addressing -> reg list -> aval list ->
    addr_strength_reduction_64_generic_cases **)

let addr_strength_reduction_64_generic_match addr args vl =
  match addr with
  | Aindexed2 ofs ->
    let addr0 = Aindexed2 ofs in
    (match args with
     | [] -> Coq_addr_strength_reduction_64_generic_default (addr0, [], vl)
     | r1 :: l ->
       (match l with
        | [] ->
          Coq_addr_strength_reduction_64_generic_default (addr0, (r1 :: []),
            vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] ->
                Coq_addr_strength_reduction_64_generic_default (addr0, args0,
                  [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_64_generic_default (addr0,
                        args0, (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_addr_strength_reduction_64_generic_default
                           (addr0, args0, (Vbot :: (Vbot :: l2)))
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_addr_strength_reduction_64_generic_case2
                              (ofs, r1, r2, Vbot, n2)
                          | a0 :: l3 ->
                            Coq_addr_strength_reduction_64_generic_default
                              (addr0, args0, (Vbot :: ((L n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_addr_strength_reduction_64_generic_default
                           (addr0, args0, (Vbot :: (x :: l2)))))
                 | L n1 ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_64_generic_default (addr0,
                        args0, ((L n1) :: []))
                    | v2 :: l2 ->
                      (match l2 with
                       | [] ->
                         Coq_addr_strength_reduction_64_generic_case1 (ofs,
                           r1, r2, n1, v2)
                       | a :: l3 ->
                         Coq_addr_strength_reduction_64_generic_default
                           (addr0, args0, ((L n1) :: (v2 :: (a :: l3))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_64_generic_default (addr0,
                        args0, (x :: []))
                    | a :: l2 ->
                      (match a with
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_addr_strength_reduction_64_generic_case2
                              (ofs, r1, r2, x, n2)
                          | a0 :: l3 ->
                            Coq_addr_strength_reduction_64_generic_default
                              (addr0, args0, (x :: ((L n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_addr_strength_reduction_64_generic_default
                           (addr0, args0, (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_addr_strength_reduction_64_generic_default (addr0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Aindexed2scaled (sc, ofs) ->
    let addr0 = Aindexed2scaled (sc, ofs) in
    (match args with
     | [] -> Coq_addr_strength_reduction_64_generic_default (addr0, [], vl)
     | r1 :: l ->
       (match l with
        | [] ->
          Coq_addr_strength_reduction_64_generic_default (addr0, (r1 :: []),
            vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] ->
                Coq_addr_strength_reduction_64_generic_default (addr0, args0,
                  [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_64_generic_default (addr0,
                        args0, (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_addr_strength_reduction_64_generic_default
                           (addr0, args0, (Vbot :: (Vbot :: l2)))
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_addr_strength_reduction_64_generic_case3 (sc,
                              ofs, r1, r2, Vbot, n2)
                          | a0 :: l3 ->
                            Coq_addr_strength_reduction_64_generic_default
                              (addr0, args0, (Vbot :: ((L n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_addr_strength_reduction_64_generic_default
                           (addr0, args0, (Vbot :: (x :: l2)))))
                 | L n1 ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_64_generic_default (addr0,
                        args0, ((L n1) :: []))
                    | v2 :: l2 ->
                      (match v2 with
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_addr_strength_reduction_64_generic_case3 (sc,
                              ofs, r1, r2, (L n1), n2)
                          | a :: l3 ->
                            Coq_addr_strength_reduction_64_generic_default
                              (addr0, args0, ((L n1) :: ((L
                              n2) :: (a :: l3)))))
                       | x ->
                         (match l2 with
                          | [] ->
                            Coq_addr_strength_reduction_64_generic_case4 (sc,
                              ofs, r1, r2, n1, x)
                          | a :: l3 ->
                            Coq_addr_strength_reduction_64_generic_default
                              (addr0, args0, ((L n1) :: (x :: (a :: l3)))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_64_generic_default (addr0,
                        args0, (x :: []))
                    | a :: l2 ->
                      (match a with
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_addr_strength_reduction_64_generic_case3 (sc,
                              ofs, r1, r2, x, n2)
                          | a0 :: l3 ->
                            Coq_addr_strength_reduction_64_generic_default
                              (addr0, args0, (x :: ((L n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_addr_strength_reduction_64_generic_default
                           (addr0, args0, (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_addr_strength_reduction_64_generic_default (addr0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | x -> Coq_addr_strength_reduction_64_generic_default (x, args, vl)

(** val addr_strength_reduction_64_generic :
    addressing -> reg list -> aval list -> addressing * reg list **)

let addr_strength_reduction_64_generic addr args vl =
  match addr_strength_reduction_64_generic_match addr args vl with
  | Coq_addr_strength_reduction_64_generic_case1 (ofs, _, r2, n1, _) ->
    ((Aindexed (Z.add (Int64.signed n1) ofs)), (r2 :: []))
  | Coq_addr_strength_reduction_64_generic_case2 (ofs, r1, _, _, n2) ->
    ((Aindexed (Z.add (Int64.signed n2) ofs)), (r1 :: []))
  | Coq_addr_strength_reduction_64_generic_case3 (sc, ofs, r1, _, _, n2) ->
    ((Aindexed (Z.add (Z.mul (Int64.signed n2) sc) ofs)), (r1 :: []))
  | Coq_addr_strength_reduction_64_generic_case4 (sc, ofs, _, r2, n1, _) ->
    ((Ascaled (sc, (Z.add (Int64.signed n1) ofs))), (r2 :: []))
  | Coq_addr_strength_reduction_64_generic_default (addr0, args0, _) ->
    (addr0, args0)

type addr_strength_reduction_64_cases =
| Coq_addr_strength_reduction_64_case1 of coq_Z * reg * ident * Ptrofs.int
| Coq_addr_strength_reduction_64_case2 of coq_Z * reg * Ptrofs.int
| Coq_addr_strength_reduction_64_case3 of coq_Z * reg * reg * ident
   * Ptrofs.int * Int64.int
| Coq_addr_strength_reduction_64_case4 of coq_Z * reg * reg * Int64.int
   * ident * Ptrofs.int
| Coq_addr_strength_reduction_64_case5 of coq_Z * reg * reg * Ptrofs.int
   * Int64.int
| Coq_addr_strength_reduction_64_case6 of coq_Z * reg * reg * Int64.int
   * Ptrofs.int
| Coq_addr_strength_reduction_64_case7 of coq_Z * coq_Z * reg * reg * 
   ident * Ptrofs.int * Int64.int
| Coq_addr_strength_reduction_64_default of addressing * reg list * aval list

(** val addr_strength_reduction_64_match :
    addressing -> reg list -> aval list -> addr_strength_reduction_64_cases **)

let addr_strength_reduction_64_match addr args vl =
  match addr with
  | Aindexed ofs ->
    let addr0 = Aindexed ofs in
    (match args with
     | [] -> Coq_addr_strength_reduction_64_default (addr0, [], vl)
     | r1 :: l ->
       (match l with
        | [] ->
          let args0 = r1 :: [] in
          (match vl with
           | [] -> Coq_addr_strength_reduction_64_default (addr0, args0, [])
           | a :: l0 ->
             (match a with
              | Ptr p ->
                (match p with
                 | Gl (symb, n) ->
                   (match l0 with
                    | [] ->
                      Coq_addr_strength_reduction_64_case1 (ofs, r1, symb, n)
                    | a0 :: l1 ->
                      Coq_addr_strength_reduction_64_default (addr0, args0,
                        ((Ptr (Gl (symb, n))) :: (a0 :: l1))))
                 | Stk n ->
                   (match l0 with
                    | [] -> Coq_addr_strength_reduction_64_case2 (ofs, r1, n)
                    | a0 :: l1 ->
                      Coq_addr_strength_reduction_64_default (addr0, args0,
                        ((Ptr (Stk n)) :: (a0 :: l1))))
                 | x ->
                   Coq_addr_strength_reduction_64_default (addr0, args0,
                     ((Ptr x) :: l0)))
              | x ->
                Coq_addr_strength_reduction_64_default (addr0, args0,
                  (x :: l0))))
        | r :: l0 ->
          Coq_addr_strength_reduction_64_default (addr0, (r1 :: (r :: l0)),
            vl)))
  | Aindexed2 ofs ->
    let addr0 = Aindexed2 ofs in
    (match args with
     | [] -> Coq_addr_strength_reduction_64_default (addr0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_addr_strength_reduction_64_default (addr0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] ->
                Coq_addr_strength_reduction_64_default (addr0, args0, [])
              | a :: l1 ->
                (match a with
                 | L n1 ->
                   (match l1 with
                    | [] ->
                      Coq_addr_strength_reduction_64_default (addr0, args0,
                        ((L n1) :: []))
                    | a0 :: l2 ->
                      (match a0 with
                       | Ptr p ->
                         (match p with
                          | Gl (symb, n2) ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_64_case4 (ofs, r1,
                                 r2, n1, symb, n2)
                             | a1 :: l3 ->
                               Coq_addr_strength_reduction_64_default (addr0,
                                 args0, ((L n1) :: ((Ptr (Gl (symb,
                                 n2))) :: (a1 :: l3)))))
                          | Stk n2 ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_64_case6 (ofs, r1,
                                 r2, n1, n2)
                             | a1 :: l3 ->
                               Coq_addr_strength_reduction_64_default (addr0,
                                 args0, ((L n1) :: ((Ptr (Stk
                                 n2)) :: (a1 :: l3)))))
                          | x ->
                            Coq_addr_strength_reduction_64_default (addr0,
                              args0, ((L n1) :: ((Ptr x) :: l2))))
                       | x ->
                         Coq_addr_strength_reduction_64_default (addr0,
                           args0, ((L n1) :: (x :: l2)))))
                 | Ptr p ->
                   (match p with
                    | Gl (symb, n1) ->
                      (match l1 with
                       | [] ->
                         Coq_addr_strength_reduction_64_default (addr0,
                           args0, ((Ptr (Gl (symb, n1))) :: []))
                       | a0 :: l2 ->
                         (match a0 with
                          | L n2 ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_64_case3 (ofs, r1,
                                 r2, symb, n1, n2)
                             | a1 :: l3 ->
                               Coq_addr_strength_reduction_64_default (addr0,
                                 args0, ((Ptr (Gl (symb, n1))) :: ((L
                                 n2) :: (a1 :: l3)))))
                          | x ->
                            Coq_addr_strength_reduction_64_default (addr0,
                              args0, ((Ptr (Gl (symb, n1))) :: (x :: l2)))))
                    | Stk n1 ->
                      (match l1 with
                       | [] ->
                         Coq_addr_strength_reduction_64_default (addr0,
                           args0, ((Ptr (Stk n1)) :: []))
                       | a0 :: l2 ->
                         (match a0 with
                          | L n2 ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_64_case5 (ofs, r1,
                                 r2, n1, n2)
                             | a1 :: l3 ->
                               Coq_addr_strength_reduction_64_default (addr0,
                                 args0, ((Ptr (Stk n1)) :: ((L
                                 n2) :: (a1 :: l3)))))
                          | x ->
                            Coq_addr_strength_reduction_64_default (addr0,
                              args0, ((Ptr (Stk n1)) :: (x :: l2)))))
                    | x ->
                      Coq_addr_strength_reduction_64_default (addr0, args0,
                        ((Ptr x) :: l1)))
                 | x ->
                   Coq_addr_strength_reduction_64_default (addr0, args0,
                     (x :: l1))))
           | r :: l1 ->
             Coq_addr_strength_reduction_64_default (addr0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Aindexed2scaled (sc, ofs) ->
    let addr0 = Aindexed2scaled (sc, ofs) in
    (match args with
     | [] -> Coq_addr_strength_reduction_64_default (addr0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_addr_strength_reduction_64_default (addr0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] ->
                Coq_addr_strength_reduction_64_default (addr0, args0, [])
              | a :: l1 ->
                (match a with
                 | Ptr p ->
                   (match p with
                    | Gl (symb, n1) ->
                      (match l1 with
                       | [] ->
                         Coq_addr_strength_reduction_64_default (addr0,
                           args0, ((Ptr (Gl (symb, n1))) :: []))
                       | a0 :: l2 ->
                         (match a0 with
                          | L n2 ->
                            (match l2 with
                             | [] ->
                               Coq_addr_strength_reduction_64_case7 (sc, ofs,
                                 r1, r2, symb, n1, n2)
                             | a1 :: l3 ->
                               Coq_addr_strength_reduction_64_default (addr0,
                                 args0, ((Ptr (Gl (symb, n1))) :: ((L
                                 n2) :: (a1 :: l3)))))
                          | x ->
                            Coq_addr_strength_reduction_64_default (addr0,
                              args0, ((Ptr (Gl (symb, n1))) :: (x :: l2)))))
                    | x ->
                      Coq_addr_strength_reduction_64_default (addr0, args0,
                        ((Ptr x) :: l1)))
                 | x ->
                   Coq_addr_strength_reduction_64_default (addr0, args0,
                     (x :: l1))))
           | r :: l1 ->
             Coq_addr_strength_reduction_64_default (addr0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | x -> Coq_addr_strength_reduction_64_default (x, args, vl)

(** val addr_strength_reduction_64 :
    addressing -> reg list -> aval list -> addressing * reg list **)

let addr_strength_reduction_64 addr args vl =
  if negb ptr64
  then addr_strength_reduction_64_generic addr args vl
  else (match addr_strength_reduction_64_match addr args vl with
        | Coq_addr_strength_reduction_64_case1 (ofs, _, symb, n) ->
          ((Aglobal (symb, (Ptrofs.add n (Ptrofs.repr ofs)))), [])
        | Coq_addr_strength_reduction_64_case2 (ofs, _, n) ->
          ((Ainstack (Ptrofs.add n (Ptrofs.repr ofs))), [])
        | Coq_addr_strength_reduction_64_case3 (ofs, _, _, symb, n1, n2) ->
          ((Aglobal (symb,
            (Ptrofs.add (Ptrofs.add n1 (Ptrofs.of_int64 n2))
              (Ptrofs.repr ofs)))), [])
        | Coq_addr_strength_reduction_64_case4 (ofs, _, _, n1, symb, n2) ->
          ((Aglobal (symb,
            (Ptrofs.add (Ptrofs.add n2 (Ptrofs.of_int64 n1))
              (Ptrofs.repr ofs)))), [])
        | Coq_addr_strength_reduction_64_case5 (ofs, _, _, n1, n2) ->
          ((Ainstack
            (Ptrofs.add (Ptrofs.add n1 (Ptrofs.of_int64 n2))
              (Ptrofs.repr ofs))), [])
        | Coq_addr_strength_reduction_64_case6 (ofs, _, _, n1, n2) ->
          ((Ainstack
            (Ptrofs.add (Ptrofs.add (Ptrofs.of_int64 n1) n2)
              (Ptrofs.repr ofs))), [])
        | Coq_addr_strength_reduction_64_case7 (sc, ofs, _, _, symb, n1, n2) ->
          ((Aglobal (symb,
            (Ptrofs.add
              (Ptrofs.add n1 (Ptrofs.of_int64 (Int64.mul n2 (Int64.repr sc))))
              (Ptrofs.repr ofs)))), [])
        | Coq_addr_strength_reduction_64_default (addr0, args0, vl0) ->
          addr_strength_reduction_64_generic addr0 args0 vl0)

(** val addr_strength_reduction :
    addressing -> reg list -> aval list -> addressing * reg list **)

let addr_strength_reduction addr args vl =
  let addr_args' =
    if ptr64
    then addr_strength_reduction_64 addr args vl
    else addr_strength_reduction_32 addr args vl
  in
  if addressing_valid (fst addr_args') then addr_args' else (addr, args)

(** val make_addimm : Int.int -> reg -> operation * reg list **)

let make_addimm n r =
  if Int.eq n Int.zero
  then (Omove, (r :: []))
  else ((Olea (Aindexed (Int.signed n))), (r :: []))

(** val make_shlimm : Int.int -> reg -> reg -> operation * reg list **)

let make_shlimm n r1 r2 =
  if Int.eq n Int.zero
  then (Omove, (r1 :: []))
  else if Int.ltu n Int.iwordsize
       then ((Oshlimm n), (r1 :: []))
       else (Oshl, (r1 :: (r2 :: [])))

(** val make_shrimm : Int.int -> reg -> reg -> operation * reg list **)

let make_shrimm n r1 r2 =
  if Int.eq n Int.zero
  then (Omove, (r1 :: []))
  else if Int.ltu n Int.iwordsize
       then ((Oshrimm n), (r1 :: []))
       else (Oshr, (r1 :: (r2 :: [])))

(** val make_shruimm : Int.int -> reg -> reg -> operation * reg list **)

let make_shruimm n r1 r2 =
  if Int.eq n Int.zero
  then (Omove, (r1 :: []))
  else if Int.ltu n Int.iwordsize
       then ((Oshruimm n), (r1 :: []))
       else (Oshru, (r1 :: (r2 :: [])))

(** val make_mulimm : Int.int -> reg -> operation * reg list **)

let make_mulimm n r =
  if Int.eq n Int.zero
  then ((Ointconst Int.zero), [])
  else if Int.eq n Int.one
       then (Omove, (r :: []))
       else (match Int.is_power2 n with
             | Some l -> ((Oshlimm l), (r :: []))
             | None -> ((Omulimm n), (r :: [])))

(** val make_andimm : Int.int -> reg -> aval -> operation * reg list **)

let make_andimm n r a =
  if Int.eq n Int.zero
  then ((Ointconst Int.zero), [])
  else if Int.eq n Int.mone
       then (Omove, (r :: []))
       else if match a with
               | Uns (_, m) -> Int.eq (Int.zero_ext m (Int.not n)) Int.zero
               | _ -> false
            then (Omove, (r :: []))
            else ((Oandimm n), (r :: []))

(** val make_orimm : Int.int -> reg -> operation * reg list **)

let make_orimm n r =
  if Int.eq n Int.zero
  then (Omove, (r :: []))
  else if Int.eq n Int.mone
       then ((Ointconst Int.mone), [])
       else ((Oorimm n), (r :: []))

(** val make_xorimm : Int.int -> reg -> operation * reg list **)

let make_xorimm n r =
  if Int.eq n Int.zero
  then (Omove, (r :: []))
  else if Int.eq n Int.mone
       then (Onot, (r :: []))
       else ((Oxorimm n), (r :: []))

(** val make_divimm : Int.int -> reg -> reg -> operation * reg list **)

let make_divimm n r1 r2 =
  if Int.eq n Int.one
  then (Omove, (r1 :: []))
  else (match Int.is_power2 n with
        | Some l ->
          if Int.ltu l
               (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))
          then ((Oshrximm l), (r1 :: []))
          else (Odiv, (r1 :: (r2 :: [])))
        | None -> (Odiv, (r1 :: (r2 :: []))))

(** val make_divuimm : Int.int -> reg -> reg -> operation * reg list **)

let make_divuimm n r1 r2 =
  if Int.eq n Int.one
  then (Omove, (r1 :: []))
  else (match Int.is_power2 n with
        | Some l -> ((Oshruimm l), (r1 :: []))
        | None -> (Odivu, (r1 :: (r2 :: []))))

(** val make_moduimm : Int.int -> reg -> reg -> operation * reg list **)

let make_moduimm n r1 r2 =
  match Int.is_power2 n with
  | Some _ -> ((Oandimm (Int.sub n Int.one)), (r1 :: []))
  | None -> (Omodu, (r1 :: (r2 :: [])))

(** val make_addlimm : Int64.int -> reg -> operation * reg list **)

let make_addlimm n r =
  if Int64.eq n Int64.zero
  then (Omove, (r :: []))
  else ((Oleal (Aindexed (Int64.signed n))), (r :: []))

(** val make_shllimm : Int.int -> reg -> reg -> operation * reg list **)

let make_shllimm n r1 r2 =
  if Int.eq n Int.zero
  then (Omove, (r1 :: []))
  else if Int.ltu n Int64.iwordsize'
       then ((Oshllimm n), (r1 :: []))
       else (Oshll, (r1 :: (r2 :: [])))

(** val make_shrlimm : Int.int -> reg -> reg -> operation * reg list **)

let make_shrlimm n r1 r2 =
  if Int.eq n Int.zero
  then (Omove, (r1 :: []))
  else if Int.ltu n Int64.iwordsize'
       then ((Oshrlimm n), (r1 :: []))
       else (Oshrl, (r1 :: (r2 :: [])))

(** val make_shrluimm : Int.int -> reg -> reg -> operation * reg list **)

let make_shrluimm n r1 r2 =
  if Int.eq n Int.zero
  then (Omove, (r1 :: []))
  else if Int.ltu n Int64.iwordsize'
       then ((Oshrluimm n), (r1 :: []))
       else (Oshrlu, (r1 :: (r2 :: [])))

(** val make_mullimm : Int64.int -> reg -> operation * reg list **)

let make_mullimm n r =
  if Int64.eq n Int64.zero
  then ((Olongconst Int64.zero), [])
  else if Int64.eq n Int64.one
       then (Omove, (r :: []))
       else (match Int64.is_power2' n with
             | Some l -> ((Oshllimm l), (r :: []))
             | None -> ((Omullimm n), (r :: [])))

(** val make_andlimm : Int64.int -> reg -> aval -> operation * reg list **)

let make_andlimm n r _ =
  if Int64.eq n Int64.zero
  then ((Olongconst Int64.zero), [])
  else if Int64.eq n Int64.mone
       then (Omove, (r :: []))
       else ((Oandlimm n), (r :: []))

(** val make_orlimm : Int64.int -> reg -> operation * reg list **)

let make_orlimm n r =
  if Int64.eq n Int64.zero
  then (Omove, (r :: []))
  else if Int64.eq n Int64.mone
       then ((Olongconst Int64.mone), [])
       else ((Oorlimm n), (r :: []))

(** val make_xorlimm : Int64.int -> reg -> operation * reg list **)

let make_xorlimm n r =
  if Int64.eq n Int64.zero
  then (Omove, (r :: []))
  else if Int64.eq n Int64.mone
       then (Onotl, (r :: []))
       else ((Oxorlimm n), (r :: []))

(** val make_divlimm : Int64.int -> reg -> reg -> operation * reg list **)

let make_divlimm n r1 r2 =
  match Int64.is_power2' n with
  | Some l ->
    if Int.ltu l
         (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))
    then ((Oshrxlimm l), (r1 :: []))
    else (Odivl, (r1 :: (r2 :: [])))
  | None -> (Odivl, (r1 :: (r2 :: [])))

(** val make_divluimm : Int64.int -> reg -> reg -> operation * reg list **)

let make_divluimm n r1 r2 =
  match Int64.is_power2' n with
  | Some l -> ((Oshrluimm l), (r1 :: []))
  | None -> (Odivlu, (r1 :: (r2 :: [])))

(** val make_modluimm : Int64.int -> reg -> reg -> operation * reg list **)

let make_modluimm n r1 r2 =
  match Int64.is_power2 n with
  | Some _ -> ((Oandlimm (Int64.sub n Int64.one)), (r1 :: []))
  | None -> (Omodlu, (r1 :: (r2 :: [])))

(** val make_mulfimm : float -> reg -> reg -> reg -> operation * reg list **)

let make_mulfimm n r r1 r2 =
  if Float.eq_dec n (Float.of_int (Int.repr (Zpos (Coq_xO Coq_xH))))
  then (Oaddf, (r :: (r :: [])))
  else (Omulf, (r1 :: (r2 :: [])))

(** val make_mulfsimm :
    float32 -> reg -> reg -> reg -> operation * reg list **)

let make_mulfsimm n r r1 r2 =
  if Float32.eq_dec n (Float32.of_int (Int.repr (Zpos (Coq_xO Coq_xH))))
  then (Oaddfs, (r :: (r :: [])))
  else (Omulfs, (r1 :: (r2 :: [])))

(** val make_cast8signed : reg -> aval -> operation * reg list **)

let make_cast8signed r a =
  if vincl a (Sgn (Ptop, (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
  then (Omove, (r :: []))
  else (Ocast8signed, (r :: []))

(** val make_cast8unsigned : reg -> aval -> operation * reg list **)

let make_cast8unsigned r a =
  if vincl a (Uns (Ptop, (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
  then (Omove, (r :: []))
  else (Ocast8unsigned, (r :: []))

(** val make_cast16signed : reg -> aval -> operation * reg list **)

let make_cast16signed r a =
  if vincl a (Sgn (Ptop, (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))
  then (Omove, (r :: []))
  else (Ocast16signed, (r :: []))

(** val make_cast16unsigned : reg -> aval -> operation * reg list **)

let make_cast16unsigned r a =
  if vincl a (Uns (Ptop, (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))
  then (Omove, (r :: []))
  else (Ocast16unsigned, (r :: []))

type op_strength_reduction_cases =
| Coq_op_strength_reduction_case1 of reg * aval
| Coq_op_strength_reduction_case2 of reg * aval
| Coq_op_strength_reduction_case3 of reg * aval
| Coq_op_strength_reduction_case4 of reg * aval
| Coq_op_strength_reduction_case5 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case6 of reg * reg * Int.int * aval
| Coq_op_strength_reduction_case7 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case8 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case9 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case10 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case11 of reg * reg * Int.int * aval
| Coq_op_strength_reduction_case12 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case13 of Int.int * reg * aval
| Coq_op_strength_reduction_case14 of reg * reg * Int.int * aval
| Coq_op_strength_reduction_case15 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case16 of reg * reg * Int.int * aval
| Coq_op_strength_reduction_case17 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case18 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case19 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case20 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case21 of addressing * reg list * aval list
| Coq_op_strength_reduction_case22 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case23 of reg * reg * Int64.int * aval
| Coq_op_strength_reduction_case24 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case25 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case26 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case27 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case28 of reg * reg * Int64.int * aval
| Coq_op_strength_reduction_case29 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case30 of Int64.int * reg * aval
| Coq_op_strength_reduction_case31 of reg * reg * Int64.int * aval
| Coq_op_strength_reduction_case32 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case33 of reg * reg * Int64.int * aval
| Coq_op_strength_reduction_case34 of reg * reg * aval * Int64.int
| Coq_op_strength_reduction_case35 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case36 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case37 of reg * reg * aval * Int.int
| Coq_op_strength_reduction_case38 of addressing * reg list * aval list
| Coq_op_strength_reduction_case39 of condition * reg list * aval list
| Coq_op_strength_reduction_case40 of condition * typ * reg * reg * reg list
   * aval * aval * aval list
| Coq_op_strength_reduction_case41 of reg * reg * aval * float
| Coq_op_strength_reduction_case42 of reg * reg * float * aval
| Coq_op_strength_reduction_case43 of reg * reg * aval * float32
| Coq_op_strength_reduction_case44 of reg * reg * float32 * aval
| Coq_op_strength_reduction_default of operation * reg list * aval list

(** val op_strength_reduction_match :
    operation -> reg list -> aval list -> op_strength_reduction_cases **)

let op_strength_reduction_match op args vl =
  match op with
  | Ocast8signed ->
    let op0 = Ocast8signed in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] ->
          let args0 = r1 :: [] in
          (match vl with
           | [] -> Coq_op_strength_reduction_default (op0, args0, [])
           | v1 :: l0 ->
             (match l0 with
              | [] -> Coq_op_strength_reduction_case1 (r1, v1)
              | a :: l1 ->
                Coq_op_strength_reduction_default (op0, args0,
                  (v1 :: (a :: l1)))))
        | r :: l0 ->
          Coq_op_strength_reduction_default (op0, (r1 :: (r :: l0)), vl)))
  | Ocast8unsigned ->
    let op0 = Ocast8unsigned in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] ->
          let args0 = r1 :: [] in
          (match vl with
           | [] -> Coq_op_strength_reduction_default (op0, args0, [])
           | v1 :: l0 ->
             (match l0 with
              | [] -> Coq_op_strength_reduction_case2 (r1, v1)
              | a :: l1 ->
                Coq_op_strength_reduction_default (op0, args0,
                  (v1 :: (a :: l1)))))
        | r :: l0 ->
          Coq_op_strength_reduction_default (op0, (r1 :: (r :: l0)), vl)))
  | Ocast16signed ->
    let op0 = Ocast16signed in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] ->
          let args0 = r1 :: [] in
          (match vl with
           | [] -> Coq_op_strength_reduction_default (op0, args0, [])
           | v1 :: l0 ->
             (match l0 with
              | [] -> Coq_op_strength_reduction_case3 (r1, v1)
              | a :: l1 ->
                Coq_op_strength_reduction_default (op0, args0,
                  (v1 :: (a :: l1)))))
        | r :: l0 ->
          Coq_op_strength_reduction_default (op0, (r1 :: (r :: l0)), vl)))
  | Ocast16unsigned ->
    let op0 = Ocast16unsigned in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] ->
          let args0 = r1 :: [] in
          (match vl with
           | [] -> Coq_op_strength_reduction_default (op0, args0, [])
           | v1 :: l0 ->
             (match l0 with
              | [] -> Coq_op_strength_reduction_case4 (r1, v1)
              | a :: l1 ->
                Coq_op_strength_reduction_default (op0, args0,
                  (v1 :: (a :: l1)))))
        | r :: l0 ->
          Coq_op_strength_reduction_default (op0, (r1 :: (r :: l0)), vl)))
  | Osub ->
    let op0 = Osub in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match l1 with
                 | [] ->
                   Coq_op_strength_reduction_default (op0, args0, (v1 :: []))
                 | a :: l2 ->
                   (match a with
                    | I n2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case5 (r1, r2, v1, n2)
                       | a0 :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (v1 :: ((I n2) :: (a0 :: l3)))))
                    | x ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (v1 :: (x :: l2))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Omul ->
    let op0 = Omul in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (Vbot :: l2)))
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case7 (r1, r2, Vbot, n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (Vbot :: ((I n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (x :: l2)))))
                 | I n1 ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0, ((I
                        n1) :: []))
                    | v2 :: l2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case6 (r1, r2, n1, v2)
                       | a :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0, ((I
                           n1) :: (v2 :: (a :: l3))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case7 (r1, r2, x, n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (x :: ((I n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Odiv ->
    let op0 = Odiv in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match l1 with
                 | [] ->
                   Coq_op_strength_reduction_default (op0, args0, (v1 :: []))
                 | a :: l2 ->
                   (match a with
                    | I n2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case8 (r1, r2, v1, n2)
                       | a0 :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (v1 :: ((I n2) :: (a0 :: l3)))))
                    | x ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (v1 :: (x :: l2))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Odivu ->
    let op0 = Odivu in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match l1 with
                 | [] ->
                   Coq_op_strength_reduction_default (op0, args0, (v1 :: []))
                 | a :: l2 ->
                   (match a with
                    | I n2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case9 (r1, r2, v1, n2)
                       | a0 :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (v1 :: ((I n2) :: (a0 :: l3)))))
                    | x ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (v1 :: (x :: l2))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Omodu ->
    let op0 = Omodu in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match l1 with
                 | [] ->
                   Coq_op_strength_reduction_default (op0, args0, (v1 :: []))
                 | a :: l2 ->
                   (match a with
                    | I n2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case10 (r1, r2, v1, n2)
                       | a0 :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (v1 :: ((I n2) :: (a0 :: l3)))))
                    | x ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (v1 :: (x :: l2))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Oand ->
    let op0 = Oand in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (Vbot :: l2)))
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case12 (r1, r2, Vbot,
                              n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (Vbot :: ((I n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (x :: l2)))))
                 | I n1 ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0, ((I
                        n1) :: []))
                    | v2 :: l2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case11 (r1, r2, n1, v2)
                       | a :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0, ((I
                           n1) :: (v2 :: (a :: l3))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case12 (r1, r2, x, n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (x :: ((I n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Oandimm n ->
    let op0 = Oandimm n in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] ->
          let args0 = r1 :: [] in
          (match vl with
           | [] -> Coq_op_strength_reduction_default (op0, args0, [])
           | v1 :: l0 ->
             (match l0 with
              | [] -> Coq_op_strength_reduction_case13 (n, r1, v1)
              | a :: l1 ->
                Coq_op_strength_reduction_default (op0, args0,
                  (v1 :: (a :: l1)))))
        | r :: l0 ->
          Coq_op_strength_reduction_default (op0, (r1 :: (r :: l0)), vl)))
  | Oor ->
    let op0 = Oor in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (Vbot :: l2)))
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case15 (r1, r2, Vbot,
                              n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (Vbot :: ((I n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (x :: l2)))))
                 | I n1 ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0, ((I
                        n1) :: []))
                    | v2 :: l2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case14 (r1, r2, n1, v2)
                       | a :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0, ((I
                           n1) :: (v2 :: (a :: l3))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case15 (r1, r2, x, n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (x :: ((I n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Oxor ->
    let op0 = Oxor in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (Vbot :: l2)))
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case17 (r1, r2, Vbot,
                              n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (Vbot :: ((I n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (x :: l2)))))
                 | I n1 ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0, ((I
                        n1) :: []))
                    | v2 :: l2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case16 (r1, r2, n1, v2)
                       | a :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0, ((I
                           n1) :: (v2 :: (a :: l3))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | I n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case17 (r1, r2, x, n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (x :: ((I n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Oshl ->
    let op0 = Oshl in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match l1 with
                 | [] ->
                   Coq_op_strength_reduction_default (op0, args0, (v1 :: []))
                 | a :: l2 ->
                   (match a with
                    | I n2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case18 (r1, r2, v1, n2)
                       | a0 :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (v1 :: ((I n2) :: (a0 :: l3)))))
                    | x ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (v1 :: (x :: l2))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Oshr ->
    let op0 = Oshr in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match l1 with
                 | [] ->
                   Coq_op_strength_reduction_default (op0, args0, (v1 :: []))
                 | a :: l2 ->
                   (match a with
                    | I n2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case19 (r1, r2, v1, n2)
                       | a0 :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (v1 :: ((I n2) :: (a0 :: l3)))))
                    | x ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (v1 :: (x :: l2))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Oshru ->
    let op0 = Oshru in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match l1 with
                 | [] ->
                   Coq_op_strength_reduction_default (op0, args0, (v1 :: []))
                 | a :: l2 ->
                   (match a with
                    | I n2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case20 (r1, r2, v1, n2)
                       | a0 :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (v1 :: ((I n2) :: (a0 :: l3)))))
                    | x ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (v1 :: (x :: l2))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Olea addr -> Coq_op_strength_reduction_case21 (addr, args, vl)
  | Osubl ->
    let op0 = Osubl in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match l1 with
                 | [] ->
                   Coq_op_strength_reduction_default (op0, args0, (v1 :: []))
                 | a :: l2 ->
                   (match a with
                    | L n2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case22 (r1, r2, v1, n2)
                       | a0 :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (v1 :: ((L n2) :: (a0 :: l3)))))
                    | x ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (v1 :: (x :: l2))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Omull ->
    let op0 = Omull in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (Vbot :: l2)))
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case24 (r1, r2, Vbot,
                              n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (Vbot :: ((L n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (x :: l2)))))
                 | L n1 ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0, ((L
                        n1) :: []))
                    | v2 :: l2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case23 (r1, r2, n1, v2)
                       | a :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0, ((L
                           n1) :: (v2 :: (a :: l3))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case24 (r1, r2, x, n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (x :: ((L n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Odivl ->
    let op0 = Odivl in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match l1 with
                 | [] ->
                   Coq_op_strength_reduction_default (op0, args0, (v1 :: []))
                 | a :: l2 ->
                   (match a with
                    | L n2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case25 (r1, r2, v1, n2)
                       | a0 :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (v1 :: ((L n2) :: (a0 :: l3)))))
                    | x ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (v1 :: (x :: l2))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Odivlu ->
    let op0 = Odivlu in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match l1 with
                 | [] ->
                   Coq_op_strength_reduction_default (op0, args0, (v1 :: []))
                 | a :: l2 ->
                   (match a with
                    | L n2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case26 (r1, r2, v1, n2)
                       | a0 :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (v1 :: ((L n2) :: (a0 :: l3)))))
                    | x ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (v1 :: (x :: l2))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Omodlu ->
    let op0 = Omodlu in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match l1 with
                 | [] ->
                   Coq_op_strength_reduction_default (op0, args0, (v1 :: []))
                 | a :: l2 ->
                   (match a with
                    | L n2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case27 (r1, r2, v1, n2)
                       | a0 :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (v1 :: ((L n2) :: (a0 :: l3)))))
                    | x ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (v1 :: (x :: l2))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Oandl ->
    let op0 = Oandl in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (Vbot :: l2)))
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case29 (r1, r2, Vbot,
                              n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (Vbot :: ((L n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (x :: l2)))))
                 | L n1 ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0, ((L
                        n1) :: []))
                    | v2 :: l2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case28 (r1, r2, n1, v2)
                       | a :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0, ((L
                           n1) :: (v2 :: (a :: l3))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case29 (r1, r2, x, n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (x :: ((L n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Oandlimm n ->
    let op0 = Oandlimm n in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] ->
          let args0 = r1 :: [] in
          (match vl with
           | [] -> Coq_op_strength_reduction_default (op0, args0, [])
           | v1 :: l0 ->
             (match l0 with
              | [] -> Coq_op_strength_reduction_case30 (n, r1, v1)
              | a :: l1 ->
                Coq_op_strength_reduction_default (op0, args0,
                  (v1 :: (a :: l1)))))
        | r :: l0 ->
          Coq_op_strength_reduction_default (op0, (r1 :: (r :: l0)), vl)))
  | Oorl ->
    let op0 = Oorl in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (Vbot :: l2)))
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case32 (r1, r2, Vbot,
                              n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (Vbot :: ((L n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (x :: l2)))))
                 | L n1 ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0, ((L
                        n1) :: []))
                    | v2 :: l2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case31 (r1, r2, n1, v2)
                       | a :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0, ((L
                           n1) :: (v2 :: (a :: l3))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case32 (r1, r2, x, n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (x :: ((L n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Oxorl ->
    let op0 = Oxorl in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (Vbot :: l2)))
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case34 (r1, r2, Vbot,
                              n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (Vbot :: ((L n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (x :: l2)))))
                 | L n1 ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0, ((L
                        n1) :: []))
                    | v2 :: l2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case33 (r1, r2, n1, v2)
                       | a :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0, ((L
                           n1) :: (v2 :: (a :: l3))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | L n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case34 (r1, r2, x, n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (x :: ((L n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Oshll ->
    let op0 = Oshll in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match l1 with
                 | [] ->
                   Coq_op_strength_reduction_default (op0, args0, (v1 :: []))
                 | a :: l2 ->
                   (match a with
                    | I n2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case35 (r1, r2, v1, n2)
                       | a0 :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (v1 :: ((I n2) :: (a0 :: l3)))))
                    | x ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (v1 :: (x :: l2))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Oshrl ->
    let op0 = Oshrl in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match l1 with
                 | [] ->
                   Coq_op_strength_reduction_default (op0, args0, (v1 :: []))
                 | a :: l2 ->
                   (match a with
                    | I n2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case36 (r1, r2, v1, n2)
                       | a0 :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (v1 :: ((I n2) :: (a0 :: l3)))))
                    | x ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (v1 :: (x :: l2))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Oshrlu ->
    let op0 = Oshrlu in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match l1 with
                 | [] ->
                   Coq_op_strength_reduction_default (op0, args0, (v1 :: []))
                 | a :: l2 ->
                   (match a with
                    | I n2 ->
                      (match l2 with
                       | [] ->
                         Coq_op_strength_reduction_case37 (r1, r2, v1, n2)
                       | a0 :: l3 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (v1 :: ((I n2) :: (a0 :: l3)))))
                    | x ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (v1 :: (x :: l2))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Oleal addr -> Coq_op_strength_reduction_case38 (addr, args, vl)
  | Omulf ->
    let op0 = Omulf in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (Vbot :: l2)))
                       | F n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case41 (r1, r2, Vbot,
                              n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (Vbot :: ((F n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (x :: l2)))))
                 | F n1 ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0, ((F
                        n1) :: []))
                    | v2 :: l2 ->
                      (match v2 with
                       | F n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case41 (r1, r2, (F n1),
                              n2)
                          | a :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              ((F n1) :: ((F n2) :: (a :: l3)))))
                       | x ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case42 (r1, r2, n1, x)
                          | a :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              ((F n1) :: (x :: (a :: l3)))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | F n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case41 (r1, r2, x, n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (x :: ((F n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Omulfs ->
    let op0 = Omulfs in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: l0 ->
          (match l0 with
           | [] ->
             let args0 = r1 :: (r2 :: []) in
             (match vl with
              | [] -> Coq_op_strength_reduction_default (op0, args0, [])
              | v1 :: l1 ->
                (match v1 with
                 | Vbot ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (Vbot :: []))
                    | a :: l2 ->
                      (match a with
                       | Vbot ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (Vbot :: l2)))
                       | FS n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case43 (r1, r2, Vbot,
                              n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (Vbot :: ((FS n2) :: (a0 :: l3)))))
                       | x ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (Vbot :: (x :: l2)))))
                 | FS n1 ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0, ((FS
                        n1) :: []))
                    | v2 :: l2 ->
                      (match v2 with
                       | FS n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case43 (r1, r2, (FS
                              n1), n2)
                          | a :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              ((FS n1) :: ((FS n2) :: (a :: l3)))))
                       | x ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case44 (r1, r2, n1, x)
                          | a :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              ((FS n1) :: (x :: (a :: l3)))))))
                 | x ->
                   (match l1 with
                    | [] ->
                      Coq_op_strength_reduction_default (op0, args0,
                        (x :: []))
                    | a :: l2 ->
                      (match a with
                       | FS n2 ->
                         (match l2 with
                          | [] ->
                            Coq_op_strength_reduction_case43 (r1, r2, x, n2)
                          | a0 :: l3 ->
                            Coq_op_strength_reduction_default (op0, args0,
                              (x :: ((FS n2) :: (a0 :: l3)))))
                       | x0 ->
                         Coq_op_strength_reduction_default (op0, args0,
                           (x :: (x0 :: l2)))))))
           | r :: l1 ->
             Coq_op_strength_reduction_default (op0,
               (r1 :: (r2 :: (r :: l1))), vl))))
  | Ocmp c -> Coq_op_strength_reduction_case39 (c, args, vl)
  | Osel (c, ty) ->
    let op0 = Osel (c, ty) in
    (match args with
     | [] -> Coq_op_strength_reduction_default (op0, [], vl)
     | r1 :: l ->
       (match l with
        | [] -> Coq_op_strength_reduction_default (op0, (r1 :: []), vl)
        | r2 :: args0 ->
          let args1 = r1 :: (r2 :: args0) in
          (match vl with
           | [] -> Coq_op_strength_reduction_default (op0, args1, [])
           | v1 :: l0 ->
             (match l0 with
              | [] ->
                Coq_op_strength_reduction_default (op0, args1, (v1 :: []))
              | v2 :: vl0 ->
                Coq_op_strength_reduction_case40 (c, ty, r1, r2, args0, v1,
                  v2, vl0)))))
  | x -> Coq_op_strength_reduction_default (x, args, vl)

(** val op_strength_reduction :
    operation -> reg list -> aval list -> operation * reg list **)

let op_strength_reduction op args vl =
  match op_strength_reduction_match op args vl with
  | Coq_op_strength_reduction_case1 (r1, v1) -> make_cast8signed r1 v1
  | Coq_op_strength_reduction_case2 (r1, v1) -> make_cast8unsigned r1 v1
  | Coq_op_strength_reduction_case3 (r1, v1) -> make_cast16signed r1 v1
  | Coq_op_strength_reduction_case4 (r1, v1) -> make_cast16unsigned r1 v1
  | Coq_op_strength_reduction_case5 (r1, _, _, n2) ->
    make_addimm (Int.neg n2) r1
  | Coq_op_strength_reduction_case6 (_, r2, n1, _) -> make_mulimm n1 r2
  | Coq_op_strength_reduction_case7 (r1, _, _, n2) -> make_mulimm n2 r1
  | Coq_op_strength_reduction_case8 (r1, r2, _, n2) -> make_divimm n2 r1 r2
  | Coq_op_strength_reduction_case9 (r1, r2, _, n2) -> make_divuimm n2 r1 r2
  | Coq_op_strength_reduction_case10 (r1, r2, _, n2) -> make_moduimm n2 r1 r2
  | Coq_op_strength_reduction_case11 (_, r2, n1, v2) -> make_andimm n1 r2 v2
  | Coq_op_strength_reduction_case12 (r1, _, v1, n2) -> make_andimm n2 r1 v1
  | Coq_op_strength_reduction_case13 (n, r1, v1) -> make_andimm n r1 v1
  | Coq_op_strength_reduction_case14 (_, r2, n1, _) -> make_orimm n1 r2
  | Coq_op_strength_reduction_case15 (r1, _, _, n2) -> make_orimm n2 r1
  | Coq_op_strength_reduction_case16 (_, r2, n1, _) -> make_xorimm n1 r2
  | Coq_op_strength_reduction_case17 (r1, _, _, n2) -> make_xorimm n2 r1
  | Coq_op_strength_reduction_case18 (r1, r2, _, n2) -> make_shlimm n2 r1 r2
  | Coq_op_strength_reduction_case19 (r1, r2, _, n2) -> make_shrimm n2 r1 r2
  | Coq_op_strength_reduction_case20 (r1, r2, _, n2) -> make_shruimm n2 r1 r2
  | Coq_op_strength_reduction_case21 (addr, args0, vl0) ->
    let (addr', args') = addr_strength_reduction_32 addr args0 vl0 in
    ((Olea addr'), args')
  | Coq_op_strength_reduction_case22 (r1, _, _, n2) ->
    make_addlimm (Int64.neg n2) r1
  | Coq_op_strength_reduction_case23 (_, r2, n1, _) -> make_mullimm n1 r2
  | Coq_op_strength_reduction_case24 (r1, _, _, n2) -> make_mullimm n2 r1
  | Coq_op_strength_reduction_case25 (r1, r2, _, n2) -> make_divlimm n2 r1 r2
  | Coq_op_strength_reduction_case26 (r1, r2, _, n2) -> make_divluimm n2 r1 r2
  | Coq_op_strength_reduction_case27 (r1, r2, _, n2) -> make_modluimm n2 r1 r2
  | Coq_op_strength_reduction_case28 (_, r2, n1, v2) -> make_andlimm n1 r2 v2
  | Coq_op_strength_reduction_case29 (r1, _, v1, n2) -> make_andlimm n2 r1 v1
  | Coq_op_strength_reduction_case30 (n, r1, v1) -> make_andlimm n r1 v1
  | Coq_op_strength_reduction_case31 (_, r2, n1, _) -> make_orlimm n1 r2
  | Coq_op_strength_reduction_case32 (r1, _, _, n2) -> make_orlimm n2 r1
  | Coq_op_strength_reduction_case33 (_, r2, n1, _) -> make_xorlimm n1 r2
  | Coq_op_strength_reduction_case34 (r1, _, _, n2) -> make_xorlimm n2 r1
  | Coq_op_strength_reduction_case35 (r1, r2, _, n2) -> make_shllimm n2 r1 r2
  | Coq_op_strength_reduction_case36 (r1, r2, _, n2) -> make_shrlimm n2 r1 r2
  | Coq_op_strength_reduction_case37 (r1, r2, _, n2) -> make_shrluimm n2 r1 r2
  | Coq_op_strength_reduction_case38 (addr, args0, vl0) ->
    let (addr', args') = addr_strength_reduction_64 addr args0 vl0 in
    ((Oleal addr'), args')
  | Coq_op_strength_reduction_case39 (c, args0, vl0) -> make_cmp c args0 vl0
  | Coq_op_strength_reduction_case40 (c, ty, r1, r2, args0, _, _, vl0) ->
    make_select c ty r1 r2 args0 vl0
  | Coq_op_strength_reduction_case41 (r1, r2, _, n2) ->
    make_mulfimm n2 r1 r1 r2
  | Coq_op_strength_reduction_case42 (r1, r2, n1, _) ->
    make_mulfimm n1 r2 r1 r2
  | Coq_op_strength_reduction_case43 (r1, r2, _, n2) ->
    make_mulfsimm n2 r1 r1 r2
  | Coq_op_strength_reduction_case44 (r1, r2, n1, _) ->
    make_mulfsimm n1 r2 r1 r2
  | Coq_op_strength_reduction_default (op0, args0, _) -> (op0, args0)
