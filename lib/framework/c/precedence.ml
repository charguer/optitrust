open Ast
open Trm

(** [associativity]: associativity type of primitive operations. *)
type associativity = LtoR | RtoL | NA

(** [precedence]: precedence is a pair of precedence value and associativity type. *)
type precedence = int * associativity

(** [precedence_none]: no precedence. *)
let precedence_default : precedence = (100, NA)

(** [precedence_prim p]: computes precedence of the primitive [p]. *)
let precedence_prim (p : prim) : precedence =
  match p with
  | Prim_unop unop ->
    begin match unop with
    | Unop_struct_get _ | Unop_struct_access _
    | Unop_post_incr | Unop_post_decr -> (16, LtoR)
    | Unop_cast _ -> (14, RtoL)
    | _ -> (15, RtoL)
    end
  | Prim_binop binop ->
    begin match binop with
    | Binop_array_access | Binop_array_get -> (16, LtoR)
    | Binop_mul | Binop_exact_div | Binop_trunc_div | Binop_trunc_mod -> (13, LtoR)
    | Binop_add | Binop_sub -> (12, LtoR)
    | Binop_shiftl | Binop_shiftr -> (11, LtoR)
    | Binop_lt | Binop_gt | Binop_le | Binop_ge -> (10, LtoR)
    | Binop_eq | Binop_neq -> (9, LtoR)
    | Binop_bitwise_and -> (8, LtoR)
    | Binop_xor -> (7, LtoR)
    | Binop_bitwise_or -> (6, LtoR)
    | Binop_set -> (2, RtoL)
    | Binop_gpu_set -> (2, RtoL)
    end
  | Prim_compound_assign_op _ -> (2, RtoL)
  | _ -> precedence_default

let precedence_and = (5, LtoR)
let precedence_or = (4, LtoR)
let precedence_ternary_cond = (3, RtoL)

(** [precedence_trm t]: computes precedence of the trm [t].
   FIXME: Remove this badly designed precedence system.
   There is no good concept of the precedence of a trm.
   Instead there should be a concept of precedence of a generated subdocument.
   The choice of the precedence can depend on the choices made when building the subdocument.
   *)
let precedence_trm (t : trm) : precedence =
  match t.desc with
  | Trm_apps (f, _, _, _) ->
     begin match trm_prim_inv f with
     | Some (_, p) -> precedence_prim p
     | _ -> (16, LtoR)
     end
  | Trm_prim (_, p) -> precedence_prim p
  | _ -> precedence_default

let expr_prec ((prec, _): precedence): int = prec

let sub_prec ((prec, assoc): precedence): int * int =
  match assoc with
  | LtoR -> (prec, prec + 1)
  | RtoL -> (prec + 1, prec)
  | NA -> (prec, prec)

(** [parentheses_needed ~prec t]: checks if the precedence of [t] is greater then [prec]. *)
let parentheses_needed ?(prec : int = 0)  (t : trm) =
  let (prec', assoc) = precedence_trm t in
  prec' < prec
