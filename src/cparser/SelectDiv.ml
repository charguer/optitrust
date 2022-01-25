open BinInt
open BinNums
open CminorSel
open Compopts
open Coqlib
open Datatypes
open Floats
open Integers
open Op
open SelectLong
open SelectOp
open SplitLong
open Zpower

(** val is_intconst : expr -> Int.int option **)

let is_intconst = function
| Eop (o, _) -> (match o with
                 | Ointconst n -> Some n
                 | _ -> None)
| _ -> None

(** val find_div_mul_params :
    nat -> coq_Z -> coq_Z -> coq_Z -> (coq_Z * coq_Z) option **)

let rec find_div_mul_params fuel nc d p =
  match fuel with
  | O -> None
  | S fuel' ->
    let twp = two_p p in
    if zlt (Z.mul nc (Z.sub d (Z.modulo twp d))) twp
    then Some (p, (Z.div (Z.sub (Z.add twp d) (Z.modulo twp d)) d))
    else find_div_mul_params fuel' nc d (Z.add p (Zpos Coq_xH))

(** val divs_mul_params : coq_Z -> (coq_Z * coq_Z) option **)

let divs_mul_params d =
  match find_div_mul_params Int.wordsize
          (Z.sub (Z.sub Int.half_modulus (Z.modulo Int.half_modulus d)) (Zpos
            Coq_xH)) d (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
          Coq_xH)))))) with
  | Some p0 ->
    let (p, m) = p0 in
    let p1 =
      Z.sub p (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
    in
    if (&&)
         ((&&)
           ((&&)
             ((&&)
               ((&&)
                 ((&&) ((fun x -> x) (zlt Z0 d))
                   ((fun x -> x)
                     (zlt
                       (two_p
                         (Z.add (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                           Coq_xH)))))) p1)) (Z.mul m d))))
                 ((fun x -> x)
                   (zle (Z.mul m d)
                     (Z.add
                       (two_p
                         (Z.add (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                           Coq_xH)))))) p1)) (two_p (Z.add p1 (Zpos Coq_xH)))))))
               ((fun x -> x) (zle Z0 m))) ((fun x -> x) (zlt m Int.modulus)))
           ((fun x -> x) (zle Z0 p1)))
         ((fun x -> x)
           (zlt p1 (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))
    then Some (p1, m)
    else None
  | None -> None

(** val divu_mul_params : coq_Z -> (coq_Z * coq_Z) option **)

let divu_mul_params d =
  match find_div_mul_params Int.wordsize
          (Z.sub (Z.sub Int.modulus (Z.modulo Int.modulus d)) (Zpos Coq_xH))
          d (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))) with
  | Some p0 ->
    let (p, m) = p0 in
    let p1 =
      Z.sub p (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
    in
    if (&&)
         ((&&)
           ((&&)
             ((&&)
               ((&&)
                 ((&&) ((fun x -> x) (zlt Z0 d))
                   ((fun x -> x)
                     (zle
                       (two_p
                         (Z.add (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                           Coq_xH)))))) p1)) (Z.mul m d))))
                 ((fun x -> x)
                   (zle (Z.mul m d)
                     (Z.add
                       (two_p
                         (Z.add (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                           Coq_xH)))))) p1)) (two_p p1)))))
               ((fun x -> x) (zle Z0 m))) ((fun x -> x) (zlt m Int.modulus)))
           ((fun x -> x) (zle Z0 p1)))
         ((fun x -> x)
           (zlt p1 (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))
    then Some (p1, m)
    else None
  | None -> None

(** val divls_mul_params : coq_Z -> (coq_Z * coq_Z) option **)

let divls_mul_params d =
  match find_div_mul_params Int64.wordsize
          (Z.sub (Z.sub Int64.half_modulus (Z.modulo Int64.half_modulus d))
            (Zpos Coq_xH)) d (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
          (Coq_xO Coq_xH))))))) with
  | Some p0 ->
    let (p, m) = p0 in
    let p1 =
      Z.sub p (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        Coq_xH)))))))
    in
    if (&&)
         ((&&)
           ((&&)
             ((&&)
               ((&&)
                 ((&&) ((fun x -> x) (zlt Z0 d))
                   ((fun x -> x)
                     (zlt
                       (two_p
                         (Z.add (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                           (Coq_xO Coq_xH))))))) p1)) (Z.mul m d))))
                 ((fun x -> x)
                   (zle (Z.mul m d)
                     (Z.add
                       (two_p
                         (Z.add (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                           (Coq_xO Coq_xH))))))) p1))
                       (two_p (Z.add p1 (Zpos Coq_xH)))))))
               ((fun x -> x) (zle Z0 m)))
             ((fun x -> x) (zlt m Int64.modulus))) ((fun x -> x) (zle Z0 p1)))
         ((fun x -> x)
           (zlt p1 (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
             Coq_xH)))))))))
    then Some (p1, m)
    else None
  | None -> None

(** val divlu_mul_params : coq_Z -> (coq_Z * coq_Z) option **)

let divlu_mul_params d =
  match find_div_mul_params Int64.wordsize
          (Z.sub (Z.sub Int64.modulus (Z.modulo Int64.modulus d)) (Zpos
            Coq_xH)) d (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
          Coq_xH))))))) with
  | Some p0 ->
    let (p, m) = p0 in
    let p1 =
      Z.sub p (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        Coq_xH)))))))
    in
    if (&&)
         ((&&)
           ((&&)
             ((&&)
               ((&&)
                 ((&&) ((fun x -> x) (zlt Z0 d))
                   ((fun x -> x)
                     (zle
                       (two_p
                         (Z.add (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                           (Coq_xO Coq_xH))))))) p1)) (Z.mul m d))))
                 ((fun x -> x)
                   (zle (Z.mul m d)
                     (Z.add
                       (two_p
                         (Z.add (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                           (Coq_xO Coq_xH))))))) p1)) (two_p p1)))))
               ((fun x -> x) (zle Z0 m)))
             ((fun x -> x) (zlt m Int64.modulus))) ((fun x -> x) (zle Z0 p1)))
         ((fun x -> x)
           (zlt p1 (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
             Coq_xH)))))))))
    then Some (p1, m)
    else None
  | None -> None

(** val divu_mul : coq_Z -> coq_Z -> expr **)

let divu_mul p m =
  shruimm (mulhu (Eletvar O) (Eop ((Ointconst (Int.repr m)), Enil)))
    (Int.repr p)

(** val divuimm : expr -> Int.int -> expr **)

let divuimm e1 n2 =
  match Int.is_power2 n2 with
  | Some l -> shruimm e1 l
  | None ->
    if optim_for_size ()
    then divu_base e1 (Eop ((Ointconst n2), Enil))
    else (match divu_mul_params (Int.unsigned n2) with
          | Some p0 -> let (p, m) = p0 in Elet (e1, (divu_mul p m))
          | None -> divu_base e1 (Eop ((Ointconst n2), Enil)))

(** val divu : expr -> expr -> expr **)

let divu e1 e2 =
  match is_intconst e2 with
  | Some n2 ->
    (match is_intconst e1 with
     | Some n1 ->
       if Int.eq n2 Int.zero
       then divu_base e1 e2
       else Eop ((Ointconst (Int.divu n1 n2)), Enil)
     | None -> divuimm e1 n2)
  | None -> divu_base e1 e2

(** val mod_from_div : expr -> Int.int -> expr **)

let mod_from_div equo n =
  Eop (Osub, (Econs ((Eletvar O), (Econs ((mulimm n equo), Enil)))))

(** val moduimm : expr -> Int.int -> expr **)

let moduimm e1 n2 =
  match Int.is_power2 n2 with
  | Some _ -> andimm (Int.sub n2 Int.one) e1
  | None ->
    if optim_for_size ()
    then modu_base e1 (Eop ((Ointconst n2), Enil))
    else (match divu_mul_params (Int.unsigned n2) with
          | Some p0 ->
            let (p, m) = p0 in Elet (e1, (mod_from_div (divu_mul p m) n2))
          | None -> modu_base e1 (Eop ((Ointconst n2), Enil)))

(** val modu : expr -> expr -> expr **)

let modu e1 e2 =
  match is_intconst e2 with
  | Some n2 ->
    (match is_intconst e1 with
     | Some n1 ->
       if Int.eq n2 Int.zero
       then modu_base e1 e2
       else Eop ((Ointconst (Int.modu n1 n2)), Enil)
     | None -> moduimm e1 n2)
  | None -> modu_base e1 e2

(** val divs_mul : coq_Z -> coq_Z -> expr **)

let divs_mul p m =
  let e2 = mulhs (Eletvar O) (Eop ((Ointconst (Int.repr m)), Enil)) in
  let e3 = if zlt m Int.half_modulus then e2 else add e2 (Eletvar O) in
  add (shrimm e3 (Int.repr p))
    (shruimm (Eletvar O) (Int.repr (Z.sub Int.zwordsize (Zpos Coq_xH))))

(** val divsimm : expr -> Int.int -> expr **)

let divsimm e1 n2 =
  match Int.is_power2 n2 with
  | Some l ->
    if Int.ltu l (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))
    then shrximm e1 l
    else divs_base e1 (Eop ((Ointconst n2), Enil))
  | None ->
    if optim_for_size ()
    then divs_base e1 (Eop ((Ointconst n2), Enil))
    else (match divs_mul_params (Int.signed n2) with
          | Some p0 -> let (p, m) = p0 in Elet (e1, (divs_mul p m))
          | None -> divs_base e1 (Eop ((Ointconst n2), Enil)))

(** val divs : expr -> expr -> expr **)

let divs e1 e2 =
  match is_intconst e2 with
  | Some n2 ->
    (match is_intconst e1 with
     | Some n1 ->
       if Int.eq n2 Int.zero
       then divs_base e1 e2
       else Eop ((Ointconst (Int.divs n1 n2)), Enil)
     | None -> divsimm e1 n2)
  | None -> divs_base e1 e2

(** val modsimm : expr -> Int.int -> expr **)

let modsimm e1 n2 =
  match Int.is_power2 n2 with
  | Some l ->
    if Int.ltu l (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))
    then Elet (e1, (mod_from_div (shrximm (Eletvar O) l) n2))
    else mods_base e1 (Eop ((Ointconst n2), Enil))
  | None ->
    if optim_for_size ()
    then mods_base e1 (Eop ((Ointconst n2), Enil))
    else (match divs_mul_params (Int.signed n2) with
          | Some p0 ->
            let (p, m) = p0 in Elet (e1, (mod_from_div (divs_mul p m) n2))
          | None -> mods_base e1 (Eop ((Ointconst n2), Enil)))

(** val mods : expr -> expr -> expr **)

let mods e1 e2 =
  match is_intconst e2 with
  | Some n2 ->
    (match is_intconst e1 with
     | Some n1 ->
       if Int.eq n2 Int.zero
       then mods_base e1 e2
       else Eop ((Ointconst (Int.mods n1 n2)), Enil)
     | None -> modsimm e1 n2)
  | None -> mods_base e1 e2

(** val modl_from_divl : helper_functions -> expr -> Int64.int -> expr **)

let modl_from_divl hf equo n =
  SelectLong.subl (Eletvar O) (SelectLong.mullimm hf n equo)

(** val divlu_mull : helper_functions -> coq_Z -> coq_Z -> expr **)

let divlu_mull hf p m =
  SelectLong.shrluimm hf (SelectLong.mullhu hf (Eletvar O) (Int64.repr m))
    (Int.repr p)

(** val divlu : helper_functions -> expr -> expr -> expr **)

let divlu hf e1 e2 =
  match SelectLong.is_longconst e2 with
  | Some n2 ->
    (match SelectLong.is_longconst e1 with
     | Some n1 -> SelectLong.longconst (Int64.divu n1 n2)
     | None ->
       (match Int64.is_power2' n2 with
        | Some l -> SelectLong.shrluimm hf e1 l
        | None ->
          if optim_for_size ()
          then SelectLong.divlu_base hf e1 e2
          else (match divlu_mul_params (Int64.unsigned n2) with
                | Some p0 -> let (p, m) = p0 in Elet (e1, (divlu_mull hf p m))
                | None -> SelectLong.divlu_base hf e1 e2)))
  | None -> SelectLong.divlu_base hf e1 e2

(** val modlu : helper_functions -> expr -> expr -> expr **)

let modlu hf e1 e2 =
  match SelectLong.is_longconst e2 with
  | Some n2 ->
    (match SelectLong.is_longconst e1 with
     | Some n1 -> SelectLong.longconst (Int64.modu n1 n2)
     | None ->
       (match Int64.is_power2 n2 with
        | Some _ ->
          SelectLong.andl e1 (SelectLong.longconst (Int64.sub n2 Int64.one))
        | None ->
          if optim_for_size ()
          then SelectLong.modlu_base hf e1 e2
          else (match divlu_mul_params (Int64.unsigned n2) with
                | Some p0 ->
                  let (p, m) = p0 in
                  Elet (e1, (modl_from_divl hf (divlu_mull hf p m) n2))
                | None -> SelectLong.modlu_base hf e1 e2)))
  | None -> SelectLong.modlu_base hf e1 e2

(** val divls_mull : helper_functions -> coq_Z -> coq_Z -> expr **)

let divls_mull hf p m =
  let e2 = SelectLong.mullhs hf (Eletvar O) (Int64.repr m) in
  let e3 =
    if zlt m Int64.half_modulus then e2 else SelectLong.addl e2 (Eletvar O)
  in
  SelectLong.addl (SelectLong.shrlimm hf e3 (Int.repr p))
    (SelectLong.shrluimm hf (Eletvar O)
      (Int.repr (Z.sub Int64.zwordsize (Zpos Coq_xH))))

(** val divls : helper_functions -> expr -> expr -> expr **)

let divls hf e1 e2 =
  match SelectLong.is_longconst e2 with
  | Some n2 ->
    (match SelectLong.is_longconst e1 with
     | Some n1 -> SelectLong.longconst (Int64.divs n1 n2)
     | None ->
       (match Int64.is_power2' n2 with
        | Some l ->
          if Int.ltu l
               (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
                 Coq_xH)))))))
          then SelectLong.shrxlimm hf e1 l
          else SelectLong.divls_base hf e1 e2
        | None ->
          if optim_for_size ()
          then SelectLong.divls_base hf e1 e2
          else (match divls_mul_params (Int64.signed n2) with
                | Some p0 -> let (p, m) = p0 in Elet (e1, (divls_mull hf p m))
                | None -> SelectLong.divls_base hf e1 e2)))
  | None -> SelectLong.divls_base hf e1 e2

(** val modls : helper_functions -> expr -> expr -> expr **)

let modls hf e1 e2 =
  match SelectLong.is_longconst e2 with
  | Some n2 ->
    (match SelectLong.is_longconst e1 with
     | Some n1 -> SelectLong.longconst (Int64.mods n1 n2)
     | None ->
       (match Int64.is_power2' n2 with
        | Some l ->
          if Int.ltu l
               (Int.repr (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
                 Coq_xH)))))))
          then Elet (e1,
                 (modl_from_divl hf (SelectLong.shrxlimm hf (Eletvar O) l) n2))
          else SelectLong.modls_base hf e1 e2
        | None ->
          if optim_for_size ()
          then SelectLong.modls_base hf e1 e2
          else (match divls_mul_params (Int64.signed n2) with
                | Some p0 ->
                  let (p, m) = p0 in
                  Elet (e1, (modl_from_divl hf (divls_mull hf p m) n2))
                | None -> SelectLong.modls_base hf e1 e2)))
  | None -> SelectLong.modls_base hf e1 e2

(** val divfimm : expr -> float -> expr **)

let divfimm e n =
  match Float.exact_inverse n with
  | Some n' ->
    Eop (Omulf, (Econs (e, (Econs ((Eop ((Ofloatconst n'), Enil)), Enil)))))
  | None ->
    Eop (Odivf, (Econs (e, (Econs ((Eop ((Ofloatconst n), Enil)), Enil)))))

type divf_cases =
| Coq_divf_case1 of float
| Coq_divf_default of expr

(** val divf_match : expr -> divf_cases **)

let divf_match = function
| Eop (o, e) ->
  (match o with
   | Ofloatconst n2 ->
     (match e with
      | Enil -> Coq_divf_case1 n2
      | Econs (e0, e1) ->
        Coq_divf_default (Eop ((Ofloatconst n2), (Econs (e0, e1)))))
   | x -> Coq_divf_default (Eop (x, e)))
| x -> Coq_divf_default x

(** val divf : expr -> expr -> expr **)

let divf e1 e2 =
  match divf_match e2 with
  | Coq_divf_case1 n2 -> divfimm e1 n2
  | Coq_divf_default e3 -> Eop (Odivf, (Econs (e1, (Econs (e3, Enil)))))

(** val divfsimm : expr -> float32 -> expr **)

let divfsimm e n =
  match Float32.exact_inverse n with
  | Some n' ->
    Eop (Omulfs, (Econs (e, (Econs ((Eop ((Osingleconst n'), Enil)), Enil)))))
  | None ->
    Eop (Odivfs, (Econs (e, (Econs ((Eop ((Osingleconst n), Enil)), Enil)))))

type divfs_cases =
| Coq_divfs_case1 of float32
| Coq_divfs_default of expr

(** val divfs_match : expr -> divfs_cases **)

let divfs_match = function
| Eop (o, e) ->
  (match o with
   | Osingleconst n2 ->
     (match e with
      | Enil -> Coq_divfs_case1 n2
      | Econs (e0, e1) ->
        Coq_divfs_default (Eop ((Osingleconst n2), (Econs (e0, e1)))))
   | x -> Coq_divfs_default (Eop (x, e)))
| x -> Coq_divfs_default x

(** val divfs : expr -> expr -> expr **)

let divfs e1 e2 =
  match divfs_match e2 with
  | Coq_divfs_case1 n2 -> divfsimm e1 n2
  | Coq_divfs_default e3 -> Eop (Odivfs, (Econs (e1, (Econs (e3, Enil)))))
