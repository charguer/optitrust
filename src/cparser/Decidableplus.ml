open BinInt
open BinNums
open BinPos
open DecidableClass

(** val coq_Decidable_eq_positive : positive -> positive -> coq_Decidable **)

let coq_Decidable_eq_positive =
  Pos.eqb

(** val coq_Decidable_eq_Z : coq_Z -> coq_Z -> coq_Decidable **)

let coq_Decidable_eq_Z =
  Z.eqb

(** val coq_Decidable_le_Z : coq_Z -> coq_Z -> coq_Decidable **)

let coq_Decidable_le_Z =
  Z.leb

(** val coq_Decidable_ge_Z : coq_Z -> coq_Z -> coq_Decidable **)

let coq_Decidable_ge_Z =
  Z.geb

(** val coq_Decidable_gt_Z : coq_Z -> coq_Z -> coq_Decidable **)

let coq_Decidable_gt_Z =
  Z.gtb

(** val coq_Decidable_divides : coq_Z -> coq_Z -> coq_Decidable **)

let coq_Decidable_divides x y =
  Z.eqb y (Z.mul (Z.div y x) x)
