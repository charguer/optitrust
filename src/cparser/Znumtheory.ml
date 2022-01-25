open BinInt
open BinNums

(** val coq_Zdivide_dec : coq_Z -> coq_Z -> bool **)

let coq_Zdivide_dec a b =
  let s = Z.eq_dec a Z0 in
  if s then Z.eq_dec b Z0 else Z.eq_dec (Z.modulo b a) Z0
