open BinNums
open NeedDomain
open Op

(** val op1 : nval -> nval list **)

let op1 nv =
  nv :: []

(** val op2 : nval -> nval list **)

let op2 nv =
  nv :: (nv :: [])

(** val needs_of_condition : condition -> nval list **)

let needs_of_condition = function
| Cmaskzero n -> op1 (maskzero n)
| Cmasknotzero n -> op1 (maskzero n)
| _ -> []

(** val needs_of_addressing_32 : addressing -> nval -> nval list **)

let needs_of_addressing_32 addr nv =
  match addr with
  | Aindexed _ -> op1 (modarith nv)
  | Aindexed2 _ -> op2 (modarith nv)
  | Ascaled (_, _) -> op1 (modarith (modarith nv))
  | Aindexed2scaled (_, _) -> op2 (modarith nv)
  | Abased (_, _) -> op1 (modarith nv)
  | Abasedscaled (_, _, _) -> op1 (modarith (modarith nv))
  | _ -> []

(** val needs_of_addressing_64 : addressing -> nval -> nval list **)

let needs_of_addressing_64 addr nv =
  match addr with
  | Aindexed2 _ -> op2 (default nv)
  | Aindexed2scaled (_, _) -> op2 (default nv)
  | Aglobal (_, _) -> []
  | Ainstack _ -> []
  | _ -> op1 (default nv)

(** val needs_of_operation : operation -> nval -> nval list **)

let needs_of_operation op nv =
  match op with
  | Omove -> op1 nv
  | Ointconst _ -> []
  | Olongconst _ -> []
  | Ofloatconst _ -> []
  | Osingleconst _ -> []
  | Oindirectsymbol _ -> []
  | Ocast8signed -> op1 (sign_ext (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))) nv)
  | Ocast8unsigned ->
    op1 (zero_ext (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))) nv)
  | Ocast16signed ->
    op1 (sign_ext (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))) nv)
  | Ocast16unsigned ->
    op1 (zero_ext (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))) nv)
  | Oneg -> op1 (modarith nv)
  | Omul -> op2 (modarith nv)
  | Omulimm _ -> op1 (modarith nv)
  | Oand -> op2 (bitwise nv)
  | Oandimm n -> op1 (andimm nv n)
  | Oor -> op2 (bitwise nv)
  | Oorimm n -> op1 (orimm nv n)
  | Oxor -> op2 (bitwise nv)
  | Oxorimm _ -> op1 (bitwise nv)
  | Onot -> op1 (bitwise nv)
  | Oshlimm n -> op1 (shlimm nv n)
  | Oshrimm n -> op1 (shrimm nv n)
  | Oshrximm _ -> op1 (default nv)
  | Oshruimm n -> op1 (shruimm nv n)
  | Ororimm n -> op1 (ror nv n)
  | Oshldimm _ -> op1 (default nv)
  | Olea addr -> needs_of_addressing_32 addr nv
  | Olowlong -> op1 (default nv)
  | Ohighlong -> op1 (default nv)
  | Ocast32signed -> op1 (default nv)
  | Ocast32unsigned -> op1 (default nv)
  | Onegl -> op1 (default nv)
  | Oaddlimm _ -> op1 (default nv)
  | Omullimm _ -> op1 (default nv)
  | Oandlimm _ -> op1 (default nv)
  | Oorlimm _ -> op1 (default nv)
  | Oxorlimm _ -> op1 (default nv)
  | Onotl -> op1 (default nv)
  | Oshllimm _ -> op1 (default nv)
  | Oshrlimm _ -> op1 (default nv)
  | Oshrxlimm _ -> op1 (default nv)
  | Oshrluimm _ -> op1 (default nv)
  | Ororlimm _ -> op1 (default nv)
  | Oleal addr -> needs_of_addressing_64 addr nv
  | Onegf -> op1 (default nv)
  | Oabsf -> op1 (default nv)
  | Onegfs -> op1 (default nv)
  | Oabsfs -> op1 (default nv)
  | Osingleoffloat -> op1 (default nv)
  | Ofloatofsingle -> op1 (default nv)
  | Ointoffloat -> op1 (default nv)
  | Ofloatofint -> op1 (default nv)
  | Ointofsingle -> op1 (default nv)
  | Osingleofint -> op1 (default nv)
  | Olongoffloat -> op1 (default nv)
  | Ofloatoflong -> op1 (default nv)
  | Olongofsingle -> op1 (default nv)
  | Osingleoflong -> op1 (default nv)
  | Ocmp c -> needs_of_condition c
  | Osel (c, _) -> nv :: (nv :: (needs_of_condition c))
  | _ -> op2 (default nv)

(** val operation_is_redundant : operation -> nval -> bool **)

let operation_is_redundant op nv =
  match op with
  | Ocast8signed ->
    sign_ext_redundant (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))) nv
  | Ocast8unsigned ->
    zero_ext_redundant (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))) nv
  | Ocast16signed ->
    sign_ext_redundant (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))) nv
  | Ocast16unsigned ->
    zero_ext_redundant (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))) nv
  | Oandimm n -> andimm_redundant nv n
  | Oorimm n -> orimm_redundant nv n
  | _ -> false
