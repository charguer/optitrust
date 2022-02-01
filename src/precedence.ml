open Ast 

type associativity = LtoR | RtoL | NA 


type precedence = int * associativity

let precedence_none : precedence = (0, NA)

let precedence_prim (p : prim) : precedence =
  match p with 
  | Prim_unop unop -> 
    begin match unop with 
    | Unop_struct_get _ | Unop_struct_access _
    | Unop_post_inc | Unop_post_dec -> (16, LtoR)
    | Unop_cast _ -> (14, RtoL)
    | _ -> (15, RtoL) 
    end 
  | Prim_binop binop -> 
    begin match binop with 
    | Binop_array_access | Binop_array_get | Binop_mul | Binop_div | Binop_mod -> (16, LtoR)
    | Binop_add | Binop_sub -> (12, LtoR)
    | Binop_shiftl | Binop_shiftr -> (11, LtoR)
    | Binop_lt | Binop_gt | Binop_le | Binop_ge -> (10, LtoR)
    | Binop_eq | Binop_neq -> (9, LtoR)
    | Binop_and -> (8, LtoR)
    | Binop_xor -> (7, LtoR)
    | Binop_or -> (6, LtoR)
    | Binop_bitwise_and -> (5, LtoR)
    | Binop_bitwise_or -> (6, LtoR)
    | _ -> precedence_none
    end 
  | Prim_compound_assgn_op binop -> 
    begin match binop with 
    | Binop_set | Binop_add | Binop_sub | Binop_mul | Binop_div 
     | Binop_mod | Binop_and | Binop_or | Binop_xor | Binop_shiftl 
     | Binop_shiftr -> (2, RtoL)
    | _ -> precedence_none
    end
  | Prim_conditional_op -> (3, RtoL)
  | _ -> precedence_none



let precedence_trm (t : trm) : precedence = 
  match t.desc with 
  | Trm_apps (f, _) -> 
     begin match trm_prim_inv f with 
     | Some p -> precedence_prim p
     | _ -> (16, LtoR)
     end
  | Trm_val (Val_prim p) -> precedence_prim p
  | Trm_val (Val_lit _ ) -> (30, NA)
  | Trm_var _ -> (20, NA)
  | _ -> precedence_none

let parentheses_needed ?(prec : int = 0)  (t : trm) = 
  let (prec', assoc) = precedence_trm t in 
  prec' < prec