open BinInt
open BinNums
open BinPos
open Datatypes
open List0
open ZArith_dec

(** val peq : positive -> positive -> bool **)

let peq =
  Pos.eq_dec

(** val plt : positive -> positive -> bool **)

let plt x y =
  let c = Pos.compare x y in (match c with
                              | Lt -> true
                              | _ -> false)

(** val zeq : coq_Z -> coq_Z -> bool **)

let zeq =
  Z.eq_dec

(** val zlt : coq_Z -> coq_Z -> bool **)

let zlt =
  coq_Z_lt_dec

(** val zle : coq_Z -> coq_Z -> bool **)

let zle =
  coq_Z_le_gt_dec

(** val align : coq_Z -> coq_Z -> coq_Z **)

let align n amount =
  Z.mul (Z.div (Z.sub (Z.add n amount) (Zpos Coq_xH)) amount) amount

(** val floor : coq_Z -> coq_Z -> coq_Z **)

let floor n amount =
  Z.mul (Z.div n amount) amount

(** val option_eq :
    ('a1 -> 'a1 -> bool) -> 'a1 option -> 'a1 option -> bool **)

let option_eq eqA x y =
  match x with
  | Some x0 -> (match y with
                | Some a0 -> eqA x0 a0
                | None -> false)
  | None -> (match y with
             | Some _ -> false
             | None -> true)

(** val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)

let option_map f = function
| Some y -> Some (f y)
| None -> None

(** val sum_left_map : ('a1 -> 'a2) -> ('a1, 'a3) sum -> ('a2, 'a3) sum **)

let sum_left_map f = function
| Coq_inl y -> Coq_inl (f y)
| Coq_inr z -> Coq_inr z

(** val list_length_z_aux : 'a1 list -> coq_Z -> coq_Z **)

let rec list_length_z_aux l acc =
  match l with
  | [] -> acc
  | _ :: tl -> list_length_z_aux tl (Z.succ acc)

(** val list_length_z : 'a1 list -> coq_Z **)

let list_length_z l =
  list_length_z_aux l Z0

(** val list_nth_z : 'a1 list -> coq_Z -> 'a1 option **)

let rec list_nth_z l n =
  match l with
  | [] -> None
  | hd :: tl -> if zeq n Z0 then Some hd else list_nth_z tl (Z.pred n)

(** val list_fold_left : ('a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 list -> 'a2 **)

let rec list_fold_left f accu = function
| [] -> accu
| x :: l' -> list_fold_left f (f x accu) l'

(** val list_fold_right : ('a1 -> 'a2 -> 'a2) -> 'a1 list -> 'a2 -> 'a2 **)

let list_fold_right f l base =
  list_fold_left f base (rev' l)

(** val list_disjoint_dec :
    ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool **)

let rec list_disjoint_dec eqA_dec l1 l2 =
  match l1 with
  | [] -> true
  | y :: l ->
    if in_dec eqA_dec y l2 then false else list_disjoint_dec eqA_dec l l2

(** val list_norepet_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> bool **)

let rec list_norepet_dec eqA_dec = function
| [] -> true
| y :: l0 ->
  if list_norepet_dec eqA_dec l0
  then if in_dec eqA_dec y l0 then false else true
  else false


